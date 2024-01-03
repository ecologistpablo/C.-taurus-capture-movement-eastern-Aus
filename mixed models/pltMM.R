# A function to build a plotting data frame for mixed-effects model (mod), data (d) and alpha
	# Modified from https://bbolker.github.io/mixedmodels-misc/ecostats_chap.html
	# Written by Dave S
		# August 2020
pltmm <- function(mod, d, alpha = 0.05) {
	m <- formula(mod,fixed.only = TRUE)[-2] # Fixed effects from mod
	# Build predition data frame
	mc <- as.character(m)[2] # Make the formula character, instead
	# Split out fixed effects terms, ignoring interactions
	fs <- unlist(strsplit(mc, " * "))[which(unlist(lapply(unlist(strsplit(mc, " * ")), nchar)) > 1)]
	if(length(grep(":", fs)) > 0) fs <- fs[-grep(":", fs)] 	
	fs <- gsub("\\(", "", gsub("\\)", "", fs)) # [DS] New line: gets rid of braces
	# Figure out which effects are factor and which are continuous, and fill, as necessary
	out <- list()
	for(i in 1:length(fs)) {
		if(eval(parse(text = paste0("with(d,is.factor(", fs[i], "))")))) {
			out[[fs[i]]] <- eval(parse(text = paste0("with(d, ", fs[i], " <- levels(", fs[i], "))")))
		} else {
			out[[fs[i]]] <- eval(parse(text = paste0("with(d, ", fs[i], " <- seq(min(", fs[i], "), max(", fs[i], "), length.out = 250))")))	
		}
	}
	p <- expand.grid(out) # Make preditor data frame
	mm <- model.matrix(m, p) # Model matrix for predictors
	beta <- fixef(mod) # Fixed-effects coefficients
	y <- mm %*% beta # Predicted values 
	V <- vcov(mod) # Variance-covariance matrix of beta
	pred.se <- sqrt(diag(mm %*% V %*% t(mm))) # Std errors of predictions
	linv <- family(mod)$linkinv # Extract the inverse-link function
	# Construct 95% Normal CIs on the link scale and transform back to the response (probability) scale
	crit <- -qnorm(alpha/2)
	fits <- data.frame(y = y, # Predicted value from model
										 se.hi =  y + pred.se,
										 se.lw =  y - pred.se,
										 upr = y + crit*pred.se, # Approx upper 95% conf limit for fit
										 lwr = y - crit*pred.se # Approx lower 95% conf limit for fit
	)
	Fits <- as.data.frame(lapply(fits, linv))
	return(cbind(p, Fits))
}