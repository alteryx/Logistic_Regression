library(AlteryxPredictive)

#########
# Helper function
AlteryxReportRx <- function (rx.obj, null.deviance = NULL) {
    if (!(class(rx.obj) %in% c("rxLinMod","rxLogit","rxGlm"))) 
        stop("The object provided is not an appropriate RevoScaleR class object")
    the.call <- paste(capture.output(rx.obj$call), collapse = "")
    the.call = gsub("\\s\\s", "", the.call)
	# The coefficients and related estimates need to be done by class
	if (class(rx.obj) == "rxLinMod") {
		param.names <- attributes(rx.obj$coefficients)$dimnames[[1]]
		coefs1 <- rx.obj$coefficients[,1]
		the.coefs <- format(coefs1, digits = 4)
		the.coefs[is.na(coefs1)] <- "Dropped"
		the.se <- format(rx.obj$coef.std.error[,1], digits = 4)
		the.se[is.na(coefs1)] <- "Dropped"
		the.t <- format(rx.obj$coef.t.value[,1], digits = 4)
		the.t[is.na(coefs1)] <- "Dropped"
		p.stars <- pStars(rx.obj$coef.p.value[,1])
		p.stars$p_txt <- as.character(p.stars$p_txt)
		p.stars$p_txt[is.na(coefs1)] <- "Dropped"
		p.stars$Stars <- as.character(p.stars$Stars)
		p.stars$Stars[is.na(coefs1)] <- " "
	} else {
		param.names <- names(rx.obj$coefficients)
		the.coefs <- format(rx.obj$coefficients, digits = 4)
		the.coefs[is.na(rx.obj$coefficients)] <- "Dropped"
		the.se <- format(rx.obj$coef.std.error, digits = 4)
		the.se[is.na(rx.obj$coefficients)] <- "Dropped"
		the.t <- format(rx.obj$coef.t.value, digits = 4)
		the.t[is.na(rx.obj$coefficients)] <- "Dropped"
		p.stars <- pStars(rx.obj$coef.p.value)
		p.stars$p_txt <- as.character(p.stars$p_txt)
		p.stars$p_txt[is.na(rx.obj$coefficients)] <- "Dropped"
		p.stars$Stars <- as.character(p.stars$Stars)
		p.stars$Stars[is.na(rx.obj$coefficients)] <- " "
	}
	coef.est <- paste(param.names, the.coefs, the.se, the.t, p.stars$p_txt, p.stars$Stars, sep = "|")
    coef.lab <- "Coefficients:"
    omitted <- names(rx.obj$aliased)[rx.obj$aliased]
    if (length(omitted) > 0)
        coef.lab <- paste(coef.lab, " (", length(omitted), " not defined because of singularities)", sep = "")
	# Model summary, slightly different for glm based objects versus lm objects
	if (class(rx.obj) != "rxLinMod") {
		if (class(rx.obj) == "rxGlm")
			dispersion <- paste("(Dispersion parameter for ", rx.obj$family$family, " taken to be ", rx.obj$dispersion, ")", sep = "")
		if (class(rx.obj) == "rxLogit")
			dispersion <- "(Dispersion parameter for binomial taken to be 1)"
		df.null <- rx.obj$nValidObs - 1
		df.mod <- rx.obj$nValidObs - length(param.names)
		null.dev <- paste("Null deviance:", format(null.deviance, digits = 5), "on", df.null, "degrees of freedom")
		mod.dev <- paste("Residual deviance:", format(rx.obj$deviance, digits = 5), "on", df.mod, "degrees of freedom")
		McF.R2 <- 1 - (rx.obj$deviance/null.deviance)
		mod.fit <- paste("McFadden R-Squared: ", format(McF.R2, digits = 4), ", AIC: ", format(rx.obj$aic, digits = 4), sep = "")
		fisher.it <- paste("Number of IRLS iterations:", rx.obj$iter)
		sum.grps <- c("Call", "Coef_Label", rep("Coef_Est", length(coef.est)), "Dispersion", rep("Fit_Stats", 3), "Fisher")
		sum.out <- c(the.call, coef.lab, coef.est, dispersion, null.dev, mod.dev, mod.fit, fisher.it)
		summary.df <- data.frame(grp = sum.grps, out = sum.out)
		summary.df$grp <- as.character(summary.df$grp)
		summary.df$out <- as.character(summary.df$out)
	} else {
		resid.se <- paste("Residual standard error:", format(rx.obj$sigma, digits = 5), "on", rx.obj$df[2], "degrees of freedom")
		r.sq <- paste("Multiple R-squared: ", format(rx.obj$r.squared, digits = 4), ", Adjusted R-Squared: ", format(rx.obj$adj.r.squared, 
        digits = 4), sep = "")
		p.f <- format(rx.obj$f.pvalue, digits = 4)
		p.f[rx.obj$f.pvalue < 2.2e-16] <- "< 2.2e-16"
		f.stat <- paste("F-statistic: ", format(rx.obj$fstatistic$value, digits = 4), " on ", as.integer(rx.obj$fstatistic$numdf), " and ", as.integer(rx.obj$fstatistic$dendf), " DF, p-value: ", p.f, sep = "")
		sum.grps <- c("Call", "Coef_Label", rep("Coef_Est", length(coef.est)), rep("Fit_Stats", 3))
		sum.out <- c(the.call, coef.lab, coef.est, resid.se, r.sq, f.stat)
		summary.df <- data.frame(grp = sum.grps, out = sum.out)
		summary.df$grp <- as.character(summary.df$grp)
		summary.df$out <- as.character(summary.df$out)
	}
	json.str <- paste("\"", names(rx.obj$coefficients), "\":\"", rx.obj$coefficients, "\"", sep = "", collapse = ", ")
	json.str <- paste("{", json.str, "}")
	coef.str <- c("Coef_JSON", json.str)
	summary.df <- rbind(summary.df, coef.str)
	summary.df
}


xdfLevels <- function(form, xdf) {
	factors <- rxSummary(form, data = xdf)$categorical
	the.names <- sapply(factors, function(x) names(x)[1])
	if (length(the.names) == 1) {
		the.levels <- eval(parse(text = paste("list(", the.names, " = as.character(factors[[1]][[1]]))")))
	} else {
		the.levels <- sapply(factors, function(x) as.character(x[[1]]))
		names(the.levels) <- the.names
	}
	the.levels
}

#########
# Determine if the "car" package is needed and, if it is, determine if it is
# available.
has.car <- "car" %in% row.names(installed.packages())
if(has.car) {
	library(car)
} else {
	warning("Unable to find the car package")
}
 
#########
# The core portion of the macro

## Create two lists: "config" and "inputs" ----
config <- list(
  used.weights = checkboxInput('%Question.Use Weights%', FALSE),
  model.name   = validName(textInput('%Question.Model Name%')),
  # to be removed, since there's no interface tool called "Omit Constant%"
  no.constant  = '%Question.Omit Constant%', 
  the.link     = dropdownInput('%Question.Link%', "logit"),
  resolution   = dropdownInput('%Question.graph.resolution%') 
)

inputs <- list(
  meta.data = read.AlteryxMetaInfo("#1"),
  the.data  = read.Alteryx("#1")
)


library(rjson)
#' @param metaData Meta data of input data stream from Alteryx.
#' @return A list of flag and path, where: \cr
#' "flag" indicates if the user uses Revolution R \cr
#' "path" the path of XDF \cr
checkXDF <- function(metaData) {
  flag <- FALSE
  path <- ""
  the.source <- as.character(metaData$Source)
  if (all(substr(the.source, 3, 9) == "Context")) {
    meta.list <- fromJSON(the.source[1])
    if (meta.list$Context == "XDF") {
      flag <- TRUE
      path <- meta.list$File.Loc
    } else {
      stop.Alteryx("At this time only XDF metadata streams are supported.")
    }
  }
  return(list(flag = flag, path = path))
}

XDFInfo <- checkXDF(inputs$meta.data)


# Create an is.OSR field that indicates open source R is being used
is.OSR <- !XDFInfo$flag 

# Get the field names
name.y.var <- names(inputs$the.data)[1]
names.x.vars <- names(inputs$the.data)[-1]
# Check to see if the survey package is available in cases where open source R
# is being used with sampling/case weights
has.survey <- "survey" %in% row.names(installed.packages())
# Boolean if weights are used (NOTE: Need a weight type now)
# used.weights <- '%Question.Use Weights%' == "True"

# Adjust the set of field names to remove the weight field if weights are used
if (config$used.weights) {
  # the weight is always the last column in the predictor dataframe
	the.weights <- names.x.vars[length(names.x.vars)]
	names.x.vars <- names.x.vars[1:(length(names.x.vars) - 1)]
	if (has.survey && is.OSR) {
		library(survey)
		the.design <- eval(parse(text = paste("svydesign(ids = ~1, weights = ~", the.weights, ", data = inputs$the.data)", sep = "")))
	} else {
		if (XDFInfo$flag) {
			weight.arg <- paste(", pweights = '", the.weights, "'", sep = "")
		} else {
			if (is.Pivotal) {
				AlteryxMessage("The use of sampling weights is not supported for a model estimated in a Postgresql database. A model without weights is estimated instead.", iType = 2, iPriority = 3)
			} else {
				AlteryxMessage("The survey package, needed to use sampling weights, is not installed. A model without weights is estimated instead.", iType = 2, iPriority = 3)
			}
		}
	}
}
# Make sure that the target variable is not included in the set of
# predictor variables. If it is, then remove it from the set.
if (name.y.var %in% names.x.vars)
	names.x.vars <- names.x.vars[names.x.vars != name.y.var]
# Make sure the target is binary and get its levels if the context is Pivotal
if (is.OSR && length(unique(inputs$the.data[,1])) != 2)
	stop.Alteryx("The target variable must only have two unique values.")
if (XDFInfo$flag) {
	len.target <- eval(parse(text = paste("length(rxGetVarInfo(XDFInfo$path)$", name.y.var, "$levels)", sep = "")))
	if(len.target != 2)
		stop.Alteryx("The target variable must only have two unique values.")
}

# Create the base right-hand side of the formula
x.vars <- paste(names.x.vars, collapse = " + ")

# Create the elements of the model call
if(config$no.constant == "True") {
  the.formula <- paste(name.y.var, '~ -1 +', x.vars)
} else {
  the.formula <- paste(name.y.var, '~', x.vars)
}

# The call elements when the input is a true data frame (not a schema stream)
if (is.OSR) {
	if(config$the.link == "complementary log-log")
		config$the.link <- "cloglog"
	the.family <- paste("binomial(", config$the.link, ")", sep="")
	if (config$used.weights)
		the.family <- paste("quasibinomial(", config$the.link, ")", sep="")
	model.call <- paste(config$model.name, ' <- glm(', the.formula, ', family = ', the.family, ', data = inputs$the.data)', sep="")
	# The model call if a sampling weights are used in estimation
	if (config$used.weights && has.survey)
		model.call <- paste(config$model.name, ' <- svyglm(', the.formula, ', family = ', the.family, ', design = the.design)', sep="")
}
if (XDFInfo$flag) {
	model.call <- paste(config$model.name, ' <- rxLogit(', the.formula, ', data = "', XDFInfo$path, '", dropFirst = TRUE)', sep = "")
	if ('%Question.Link%' != "logit")
		AlteryxMessage("Only the logit link function is available for XDF files, and will be used.", iType = 2, iPriority = 3)
	if (config$used.weights) {
		model.call <- paste(config$model.name, ' <- rxLogit(', the.formula, ', data = "', XDFInfo$path, '", ', weight.arg, ', dropFirst = TRUE)', sep = "")
		null.model <- eval(parse(text = paste('rxLogit(', name.y.var, ' ~ 1, data = "', XDFInfo$path, '", ', weight.arg, ')', sep = "")))
	}
	if (!config$used.weights)
		null.model <- eval(parse(text = paste('rxLogit(', name.y.var, ' ~ 1, data = "', XDFInfo$path, '")', sep = "")))
}

# Model estimation
print(model.call)
eval(parse(text = model.call))
the.model <- eval(parse(text = config$model.name))
# Add the level labels for the target and predictors, along with the target
# counts to the model object
if (is.OSR) {
	ylevels <- levels(inputs$the.data[[1]])
}
if (XDFInfo$flag) {
	target.info <- eval(parse(text = paste("rxSummary(~ ", name.y.var, ", data = XDFInfo$path)$categorical", sep = "")))
	the.model$yinfo <- list(levels = as.character(target.info[[1]][,1]), counts = target.info[[1]][,2])
	ylevels <- the.model$yinfo$levels
	the.model$xlevels <- eval(parse(text = paste("xdfLevels(~ ", x.vars, ", XDFInfo$path)")))
}
# When running the macro within Alteryx by itself, the line of code below
# causes the model summary to be written to the ouput window. However, this
# does not occur when the macro is called from another module
print(summary(the.model))
# The code below creates the key-value pair table of the glm results and the
# set of parameters that can be used downstream
if (is.OSR) {
	glm.out1 <- Alteryx.ReportGLM(the.model)
	glm.out <- glm.out1$summary.df
	singular <- glm.out1$singular
}
if (XDFInfo$flag) {
	singular <- FALSE
	glm.out <- AlteryxReportRx(the.model, null.model$deviance)
}

# Put the name of the model as the first entry in the key entry in the
# key-value table.
glm.out <- rbind(c("Model_Name", config$model.name), glm.out)
# Add the type "binomial" to the key-value pair table if non-weighted
if (!config$used.weights || !is.OSR)
	glm.out <- rbind(glm.out, c("Model_Type", "binomial"))
# Add the type "quasibinomial" to the key-value pair table if weights used
if (config$used.weights && is.OSR)
	glm.out <- rbind(glm.out, c("Model_Type", "quasibinomial"))
# If the ANOVA table is requested then create it and add its results to the
# key-value table. Its creation will be surpressed if the car package isn't
# present, if their were singularities in estimation, or if the input is an
# XDF file. 
if (has.car && !singular && is.OSR) {
	print(Anova(the.model, type="II")) # Write to the Output window
	glm.out <- rbind(glm.out, Alteryx.ReportAnova(the.model))
}
if (singular && !XDFInfo$flag)
	AlteryxMessage("Creation of the Analysis of Deviance table was surpressed due to singularities", iType = 2, iPriority = 3) 
if (!is.OSR)
	AlteryxMessage("Creation of the Analysis of Deviance tables was surpressed due to the use of an XDF file", iType = 2, iPriority = 3) 
# Write out the key-value pair table to Alteryx
write.Alteryx(glm.out)
# Prepare the basic regression diagnostic plots if it is requested
# and their isn't the combination of singularities and the use of
# sampling weights or 
if (!(singular && config$used.weights) && is.OSR) {
	whr <- graphWHR(inches = "True", in.w = 6, in.h = 6, config$resolution)
	AlteryxGraph(2, width = whr[1], height = whr[2], res = whr[3], pointsize = 9)
	par(mfrow=c(2,2), mar=c(5, 4, 2, 2) + 0.1)
	plot(the.model)
	invisible(dev.off())
} else {
	if (!is.OSR) {
		AlteryxGraph(2)
		plot(x = c(0,1), y = c(0,1), type = "n", main = "Plot not available", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
		invisible(dev.off())
		AlteryxMessage("The diagnostic plot is not available for XDF based models", iType = 2, iPriority = 3) 
	} else {
		AlteryxGraph(2)
		plot(x = c(0,1), y = c(0,1), type = "n", main = "Plot not available", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
		invisible(dev.off())
		AlteryxMessage("The diagnostic plot is not available due to singularities", iType = 2, iPriority = 3) 
	}
}
# Create a list with the model object and its name and write it out via
# the third output
the.obj <- vector(mode="list", length=2)
the.obj[[1]] <- c(config$model.name)
the.obj[[2]] <- list(the.model)
names(the.obj) <- c("Name", "Object")
#levels.list <- list(levels = ylevels)
#levels.json <- toJSON(levels.list)
#print(levels.json)
#write.Alteryx(the.obj, source = "Hello World", nOutput = 3)
write.Alteryx(the.obj, nOutput = 3)
