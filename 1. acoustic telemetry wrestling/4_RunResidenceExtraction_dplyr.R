#' Extract Residence and Non-residence Events Within an Acoustic Detection Database
#'
#' @description
#' A vectorised, \pkg{dplyr}-based rewrite of the original \code{RunResidenceExtraction()}.
#' It quantifies how long a transmitter stayed within the detection field of a given
#' location (a *residence*), and the movements between consecutive locations
#' (a *non-residence*). Logic is fully vectorised, so it runs orders of magnitude
#' faster than the original record-by-record loop. Per-tag work is mapped with
#' \pkg{furrr}, so multiple tags can be processed in parallel.
#'
#' Residency is qualified two ways, used together:
#'   1. a maximum gap between successive detections (timeout) defines the raw events, and
#'   2. an optional *daily* threshold - a minimum number of detections on each day,
#'      across a minimum number of calendar days - for an event to count as residency.
#'      The per-day floor also applies to day one, so an event needs at least that many
#'      detections to ever be 'real'.
#'
#' Non-residences are returned one row per movement, with departure info (the location left
#' behind) and arrival info (the location moved to) side by side, plus a haversine distance
#' (km) so you have how far each movement was. sex is carried through to all outputs.
#'
#' For background on the residency concept, see:
#' https://link.springer.com/article/10.1186/s40462-022-00364-z
#'
#' @param dat  a tidy detection data frame containing (at least) the columns
#'             tag_id, datetime, receiver_name, station_name, location, latitude, longitude, sex.
#'             lat/lon are required for the haversine distance in the non-residence output.
#' @param location_col  the spatial level to analyse residency at: one of
#'             "location", "station_name" or "receiver_name". Default "station_name".
#' @param time_threshold_secs  maximum gap (in seconds) allowed between successive detections
#'             before a residence event is closed ("timeout").
#' @param min_detections_per_day  optional daily threshold: minimum detections required on *each*
#'             day of an event (including day one) for it to qualify as residency.
#'             Default NULL (no daily filter).
#' @param min_residence_days  optional daily threshold: minimum number of calendar days (inclusive)
#'             an event must span to qualify as residency. Default NULL (no daily filter).
#' @param cores  number of cores to use for parallel processing across tags. Default 2.
#'
#' @return a list with three data frames:
#'   \code{residences} (residence event table; includes both duration_secs and duration_days),
#'   \code{residenceslog} (detection-level log), and
#'   \code{nonresidences} (one row per movement, with departure + arrival info and haversine distance in km).
#'
#' @importFrom dplyr arrange group_by mutate ungroup summarise filter lead lag transmute bind_rows n row_number first last if_else if_all everything select
#' @importFrom lubridate as_date
#' @importFrom furrr future_map
#' @importFrom future plan multisession sequential
#' @importFrom geosphere distHaversine
#' @export
RunResidenceExtraction <- function(dat,
                                   location_col = "station_name",
                                   time_threshold_secs,
                                   min_detections_per_day = NULL,  # NULL = skip the daily threshold
                                   min_residence_days     = NULL,  # NULL = skip the daily threshold
                                   cores = 2) {
  
  # --- setup --------------------------------------------------------------------
  
  # which column holds the spatial level we're analysing residency at
  if (!location_col %in% c("location", "station_name", "receiver_name"))   # guard bad input
    stop("location_col must be one of 'location', 'station_name' or 'receiver_name'.")
  
  # are we applying the daily thresholds? only if BOTH daily args are supplied
  use_daily_threshold <- !is.null(min_detections_per_day) && !is.null(min_residence_days)
  
  # standardise into a tidy frame, copying the chosen level into a generic 'loc' column.
  # we keep station_name + coordinates for the non-residence output, and sex to carry through.
  dat <- dat %>%                                                 # start from the detection data
    dplyr::mutate(
      tag_id       = as.character(tag_id),                       # tag id as chr (grouping key)
      datetime     = as.POSIXct(datetime),                       # ensure datetime is POSIXct
      loc          = as.character(.data[[location_col]]),        # chosen residency level
      station_name = as.character(station_name),                 # carried over for the output
      latitude     = as.numeric(latitude),                       # needed for haversine distance
      longitude    = as.numeric(longitude),                      # needed for haversine distance
      sex          = as.character(sex)) %>%                       # strip any names attr, carry through
    dplyr::select(tag_id, datetime, loc, station_name,           # keep only what we use downstream
                  latitude, longitude, sex) %>%
    dplyr::filter(dplyr::if_all(dplyr::everything(), ~ !is.na(.))) %>% # drop rows with NAs (your rule)
    dplyr::arrange(tag_id, datetime)                             # sort by tag then time (essential)
  
  # set up the parallel backend; tidy up on exit so we don't leave workers running
  future::plan(future::multisession, workers = cores)           # spin up worker sessions
  on.exit(future::plan(future::sequential), add = TRUE)         # return to sequential when done
  
  # split the data by tag so each tag is processed independently and in parallel
  tag_data <- split(dat, dat$tag_id)                            # one list element per tag id
  
  # --- residences ---------------------------------------------------------------
  
  # per-tag worker: extract residence events for a single tag's detections
  extract_residence <- function(df_tag) {                       # df_tag = all dets for one tag
    events <- df_tag %>%                                        # already time-ordered upstream
      dplyr::mutate(
        time_gap  = as.numeric(difftime(datetime,               # gap between consecutive dets
                                        dplyr::lag(datetime),    # previous detection's time
                                        units = "secs")),
        # a new residence event starts on the first row, after a timeout, or on a location change
        new_event = dplyr::if_else(is.na(time_gap) |            # first detection of the tag
                                     time_gap > time_threshold_secs | # gap exceeds timeout
                                     loc != dplyr::lag(loc),     # moved to a new location
                                   1L, 0L),
        event_id  = cumsum(new_event),                           # cumulative counter labels events
        day       = lubridate::as_date(datetime))                # calendar day for daily thresholds
    
    # summarise each event to one row, computing daily stats we may filter on below
    events_summ <- events %>%
      dplyr::group_by(event_id) %>%                              # collapse each event to one row
      dplyr::summarise(
        start_datetime = dplyr::first(datetime),                 # first detection in the event
        end_datetime   = dplyr::last(datetime),                  # last detection in the event
        tag_id         = dplyr::first(tag_id),                   # carry the tag id over
        loc            = dplyr::first(loc),                      # the location of this residence
        station_name   = dplyr::first(station_name),             # carried over for the output
        latitude       = dplyr::first(latitude),                 # event location lat
        longitude      = dplyr::first(longitude),                # event location lon
        sex            = dplyr::first(sex),                      # carry sex through
        n_detections   = dplyr::n(),                             # number of detections in the event
        n_days_incl    = as.integer(max(day) - min(day)) + 1L,   # inclusive calendar-day span
        # how many days within the event met the per-day detection threshold (incl. day one)
        days_meeting_threshold = if (use_daily_threshold)
          sum(as.numeric(table(day)) >= min_detections_per_day) else NA_integer_,
        .groups = "drop")
    
    # apply the optional daily threshold: span enough days AND meet the per-day count on every day.
    # the per-day floor also covers day one, so a one-detection blip can never be a residence.
    if (use_daily_threshold) {
      events_summ <- events_summ %>%
        dplyr::filter(n_days_incl >= min_residence_days,         # long enough in calendar days
                      days_meeting_threshold == n_days_incl)     # every day meets the per-day count
    }
    
    events_summ %>%
      dplyr::mutate(
        duration_secs   = as.numeric(difftime(end_datetime, start_datetime, units = "secs")), # secs
        duration_days   = round(as.numeric(difftime(end_datetime, start_datetime,             # days
                                                    units = "days")), 2),
        residence_event = dplyr::row_number()) %>%               # renumber events 1..n per tag
      dplyr::select(start_datetime, end_datetime, residence_event,  # tidy column order
                    tag_id, loc, station_name, latitude, longitude,
                    sex, duration_secs, duration_days, n_detections)
  }
  
  # run the residence extraction across all tags in parallel, then stack into one frame
  residences <- furrr::future_map(tag_data, extract_residence) %>%  # parallel per-tag map
    dplyr::bind_rows()                                          # combine all tags' residences
  
  # --- non-residences (long arrival / departure shape) --------------------------
  
  # build the movement base: connect each residence event with the next using lead().
  # one row here = one movement (departure location -> arrival location).
  move_base <- residences %>%
    dplyr::arrange(tag_id, start_datetime) %>%                  # order events within each tag
    dplyr::group_by(tag_id) %>%                                 # work per tag
    dplyr::mutate(
      next_loc       = dplyr::lead(loc),                         # location moved *to*
      next_station   = dplyr::lead(station_name),                # station moved *to*
      next_time      = dplyr::lead(start_datetime),              # arrival time at next location
      next_latitude  = dplyr::lead(latitude),                    # arrival lat
      next_longitude = dplyr::lead(longitude)) %>%               # arrival lon
    dplyr::filter(!is.na(next_loc),                              # drop final event (no move after)
                  loc != next_loc) %>%                           # a movement = a change in location
    dplyr::ungroup() %>%                                         # drop the per-tag grouping...
    dplyr::arrange(tag_id, start_datetime) %>%                   # ...then order across all tags
    dplyr::mutate(movement_id = dplyr::row_number())             # GLOBAL id (unique across all tags)
  
  # haversine distance (km) for each movement.
  # accounts for the earth being a sphere, so a little better than a direct distance
  move_base <- move_base %>%
    dplyr::mutate(
      distance = round(geosphere::distHaversine(
        cbind(longitude, latitude),             # departure point (lon, lat)
        cbind(next_longitude, next_latitude)    # arrival point (lon, lat)
      ) / 1000, 2))                             # metres -> km, 2 dp
  
  # build one shared block of movement info: every column for BOTH the departure end and
  # the arrival end of the move. both output rows (departure + arrival) will carry all of it.
  move_info <- move_base %>%
    dplyr::transmute(
      tag_id,                                                   # tag id
      sex,                                                      # carry sex through
      movement_id,                                              # GLOBAL movement id
      # --- departure end (the location left behind) ---
      departure_datetime  = end_datetime,                       # time the tag departed
      departure_loc       = loc,                                # location departed from
      departure_station   = station_name,                       # station departed from
      departure_latitude  = latitude,                           # departure coordinates
      departure_longitude = longitude,
      # --- arrival end (the location moved to) ---
      arrival_datetime    = next_time,                          # time the tag arrived
      arrival_loc         = next_loc,                           # location arrived at
      arrival_station     = next_station,                       # station arrived at
      arrival_latitude    = next_latitude,                      # arrival coordinates
      arrival_longitude   = next_longitude,
      # --- the movement itself ---
      duration_secs       = as.numeric(difftime(next_time, end_datetime, units = "secs")), # travel time
      distance)                                                 # haversine distance of the move (km)
  
  # long format: each movement becomes two rows sharing a movement_id, tagged by 'movement'.
  # both rows carry the full departure + arrival info above; only the row type differs.
  departures <- move_info %>%
    dplyr::mutate(movement = "departure",                       # this row marks leaving a location
                  datetime = departure_datetime)                # convenience: the time of THIS row
  arrivals   <- move_info %>%
    dplyr::mutate(movement = "arrival",                         # this row marks reaching a location
                  datetime = arrival_datetime)                  # convenience: the time of THIS row
  
  # stack the two halves and order so paired rows sit together (departure then arrival per movement)
  nonresidences <- dplyr::bind_rows(departures, arrivals) %>%
    dplyr::arrange(movement_id, movement) %>%                   # group the two rows of each movement
    dplyr::select(tag_id, sex, movement_id, movement, datetime, # tidy column order
                  departure_datetime, departure_loc, departure_station,
                  departure_latitude, departure_longitude,
                  arrival_datetime, arrival_loc, arrival_station,
                  arrival_latitude, arrival_longitude,
                  duration_secs, distance)
  
  # --- residences log ------------------------------------------------------------
  
  # the log is the detection-level record of which event each ping belongs to.
  extract_log <- function(df_tag) {                             # rebuild the per-detection labelling
    df_tag %>%
      dplyr::mutate(
        time_gap  = as.numeric(difftime(datetime, dplyr::lag(datetime), units = "secs")),
        new_event = dplyr::if_else(is.na(time_gap) |
                                     time_gap > time_threshold_secs |
                                     loc != dplyr::lag(loc), 1L, 0L),
        residence_event = cumsum(new_event)) %>%                 # same event id as above
      dplyr::group_by(residence_event) %>%
      dplyr::mutate(record  = dplyr::row_number() - 1L,          # 0-indexed record within event
                    elapsed = dplyr::if_else(is.na(time_gap), 0, time_gap)) %>% # 0 on first ping
      dplyr::ungroup() %>%
      dplyr::transmute(datetime, residence_event, record,        # tidy log column order
                       tag_id, loc, sex, elapsed)                # sex carried through
  }
  
  # build the log across all tags in parallel and stack it
  residenceslog <- furrr::future_map(tag_data, extract_log) %>%
    dplyr::bind_rows()
  
  # --- tidy names to match the chosen level -------------------------------------
  
  # restore the user-facing location column name (e.g. location / station_name / receiver_name)
  names(residences)[names(residences) == "loc"]       <- location_col
  names(residenceslog)[names(residenceslog) == "loc"] <- location_col
  # non-residences carry the level twice (departed from / arrived at), so prefix each
  names(nonresidences)[names(nonresidences) == "departure_loc"] <- paste0("departure_", location_col)
  names(nonresidences)[names(nonresidences) == "arrival_loc"]   <- paste0("arrival_", location_col)
  
  # return the three data frames as a list
  list(residences    = as.data.frame(residences),# residence event table
       residenceslog = as.data.frame(residenceslog),# detection-level log
       nonresidences = as.data.frame(nonresidences))# long arrival/departure + distance
}
