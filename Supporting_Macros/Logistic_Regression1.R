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
prGetLevels <- function(x) {
	x1 <- substr(x, 2, nchar(x) - 1)
	x2 <- gsub(" ", "", unlist(strsplit(x1, ",")))
	gsub("\"", "", x2)
}
AlteryxReportMadLib <- function (ml.obj, null.deviance = NULL) {
    if (!(class(ml.obj) %in% c("logregr.madlib"))) 
        stop("The object provided is not an appropriate MADlib class object")
    the.call <- paste(capture.output(ml.obj$call), collapse = "")
    the.call = gsub("\\s\\s", "", the.call)
	# The coefficients and related estimates need to be done by class
	if (class(ml.obj) == "logregr.madlib") {
		param.names <- names(ml.obj$coef)
		the.coefs <- format(ml.obj$coef, digits = 4)
		the.se <- format(ml.obj$std_err, digits = 4)
		the.t <- format(ml.obj$z_stats, digits = 4)
		p.stars <- pStars(ml.obj$p_values)
		p.stars$p_txt <- as.character(p.stars$p_txt)
		p.stars$Stars <- as.character(p.stars$Stars)
		p.stars$Stars[is.na(ml.obj$coef)] <- " "
	} else {
	}
	coef.est <- paste(param.names, the.coefs, the.se, the.t, p.stars$p_txt, p.stars$Stars, sep = "|")
    coef.lab <- "Coefficients:"
	# Model summary
	if (class(ml.obj) == "logregr.madlib") {
		dispersion <- "(Dispersion parameter for binomial taken to be 1)"
		#df.null <- rx.obj$nValidObs - 1
		#df.mod <- rx.obj$nValidObs - length(param.names)
		#null.dev <- paste("Null deviance:", format(null.deviance, digits = 5), "on", df.null, "degrees of freedom")
		#mod.dev <- paste("Residual deviance:", format(rx.obj$deviance, digits = 5), "on", df.mod, "degrees of freedom")
		#McF.R2 <- 1 - (rx.obj$deviance/null.deviance)
		#mod.fit <- paste("McFadden R-Squared: ", format(McF.R2, digits = 4), ", AIC: ", format(rx.obj$aic, digits = 4), sep = "")
		aic <- 2*length(the.coefs) - 2*ml.obj$log_likelihood
		mod.fit <- paste("AIC: ", format(aic, digits = 4), "  Log Likelihood: ", format(ml.obj$log_likelihood, digits = 4), sep = "")
		fisher.it <- paste("Number of IRLS iterations:", ml.obj$num_iterations)
		sum.grps <- c("Call", "Coef_Label", rep("Coef_Est", length(coef.est)), "Dispersion", "Fit_Stats", "Fisher")
		sum.out <- c(the.call, coef.lab, coef.est, dispersion, mod.fit, fisher.it)
		summary.df <- data.frame(grp = sum.grps, out = sum.out)
		summary.df$grp <- as.character(summary.df$grp)
		summary.df$out <- as.character(summary.df$out)
	} else {
	}
	json.str <- paste("\"", names(ml.obj$coef), "\":\"", ml.obj$coef, "\"", sep = "", collapse = ", ")
	json.str <- paste("{", json.str, "}")
	coef.str <- c("Coef_JSON", json.str)
	summary.df <- rbind(summary.df, coef.str)
	summary.df
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
library(rjson)
# Determine if a compute context is being used (currently supports XDF files and
# Pivotal/Postgresql databases
is.XDF <- FALSE
is.Pivotal <- FALSE
meta.data <- read.AlteryxMetaInfo("#1")
the.source <- as.character(meta.data$Source)
if (all(substr(the.source, 3, 9) == "Context")) {
	meta.list <- fromJSON(the.source[1])
	if (meta.list$Context == "XDF") {
		is.XDF <- TRUE
		xdf.path <- meta.list$File.Loc
	} else {
		if (meta.list$Context == "Pivotal") {
			is.Pivotal <- TRUE
			suppressWarnings(loadPackages(c("DBI", "RPostgreSQL", "PivotalR")))
			# Setup the connection parameters
			the.host <- meta.list$Host
			the.port <- meta.list$Port
			the.db <- meta.list$DB_Name
			the.user <- meta.list$User
			the.pwd <- meta.list$Password
			table.name <- meta.list$Table
			# Create the database connection
			the.conn <- db.connect(dbname = the.db, host = the.host, port = the.port, user = the.user, password = the.pwd)
		} else {
			stop.Alteryx("At this time only XDF and Pivotal/Postgresql metadata streams are supported.")
		}
	}
}
# Create an is.OSR field that indicates open source R is being used
is.OSR <- !is.XDF && !is.Pivotal
# Read the data / metadata stream into R
the.data <- read.Alteryx("#1")
# Get the field names
name.y.var <- names(the.data)[1]
names.x.vars <- names(the.data)[-1]
# Check to see if the survey package is available in cases where open source R
# is being used with sampling/case weights
has.survey <- "survey" %in% row.names(installed.packages())
# Boolean if weights are used (NOTE: Need a weight type now)
used.weights <- '%Question.Use Weights%' == "True"
# Adjust the set of field names to remove the weight field if weights are used
if (used.weights) {
	use.probs <- '%Question.samp.probs%' == "True"
	the.weights <- names.x.vars[length(names.x.vars)]
	names.x.vars <- names.x.vars[1:(length(names.x.vars) - 1)]
	if (has.survey && is.OSR) {
		library(survey)
		the.design <- eval(parse(text = paste("svydesign(ids = ~1, weights = ~", the.weights, ", data = the.data)", sep = "")))
	} else {
		if (is.XDF) {
			weight.arg <- paste(", pweights = '", the.weights, "'", sep = "")
		} else {
			if (is.Pivotal) {
				AlteryxMessage("The use of sampling weights is not supported for a model estimated in a Pivotal/Postgresql database. A model without weights is estimated instead.", iType = 2, iPriority = 3)
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
if (is.OSR && length(unique(the.data[,1])) != 2)
	stop.Alteryx("The target variable must only have two unique values.")
if (is.XDF) {
	len.target <- eval(parse(text = paste("length(rxGetVarInfo(xdf.path)$", name.y.var, "$levels)", sep = "")))
	if(len.target != 2)
		stop.Alteryx("The target variable must only have two unique values.")
}
if (is.Pivotal) {
	the.table <- db.data.frame(table.name, conn.id = the.conn, verbose = FALSE)
	ylevels1 <- eval(parse(text = paste('summary(the.table, target.cols = "', name.y.var, '")$most_frequent_values', sep = "")))
	ylevels <- prGetLevels(ylevels1)
	if (length(ylevels) != 2)
		stop.Alteryx("The target variable must only have two unique values.")
	ref.level <- ylevels[order(ylevels)][2]
}
# Create the base right-hand side of the formula
x.vars <- paste(names.x.vars, collapse = " + ")
# Obtain and prep other user inputs
model.name <- '%Question.Model Name%'
model.name <- validName(model.name)
# Create the elements of the model call
no.constant <- '%Question.Omit Constant%'
if (is.Pivotal) {
	new.y.var <- paste(name.y.var, "boolean", sep = "_")
	eval(parse(text = paste('the.table$', new.y.var, ' <- the.table$', name.y.var, ' == "', ref.level, '"', sep = '')))
	if(no.constant == "True") {
		the.formula <- paste(new.y.var, '~ -1 +', x.vars)
	} else {
		the.formula <- paste(new.y.var, '~', x.vars)
	}
} else {
	if(no.constant == "True") {
		the.formula <- paste(name.y.var, '~ -1 +', x.vars)
	} else {
		the.formula <- paste(name.y.var, '~', x.vars)
	}
}
# The call elements when the input is a true data frame (not a schema stream)
if (is.OSR) {
	the.link <- '%Question.Link%'
	if(the.link == "complementary log-log")
		the.link <- "cloglog"
	the.family <- paste("binomial(", the.link, ")", sep="")
	if (used.weights)
		the.family <- paste("quasibinomial(", the.link, ")", sep="")
	model.call <- paste(model.name, ' <- glm(', the.formula, ', family = ', the.family, ', data = the.data)', sep="")
	# The model call if a sampling weights are used in estimation
	if (used.weights && has.survey)
		model.call <- paste(model.name, ' <- svyglm(', the.formula, ', family = ', the.family, ', design = the.design)', sep="")
}
if (is.XDF) {
	model.call <- paste(model.name, ' <- rxLogit(', the.formula, ', data = "', xdf.path, '", dropFirst = TRUE)', sep = "")
	if ('%Question.Link%' != "logit")
		AlteryxMessage("Only the logit link function is available for XDF files, and will be used.", iType = 2, iPriority = 3)
	if (used.weights) {
		model.call <- paste(model.name, ' <- rxLogit(', the.formula, ', data = "', xdf.path, '", ', weight.arg, ', dropFirst = TRUE)', sep = "")
		null.model <- eval(parse(text = paste('rxLogit(', name.y.var, ' ~ 1, data = "', xdf.path, '", ', weight.arg, ')', sep = "")))
	}
	if (!used.weights)
		null.model <- eval(parse(text = paste('rxLogit(', name.y.var, ' ~ 1, data = "', xdf.path, '")', sep = "")))
}
if (is.Pivotal) {
	model.call <- paste(model.name, ' <- madlib.glm(', the.formula, ', data = the.table, family = "binomial", na.action = na.omit)', sep = "")
	if ('%Question.Link%' != "logit")
		AlteryxMessage("Only the logit link function is available for Pivotal/Postgresql in-database logistic regression models, and is being used.", iType = 2, iPriority = 3)
}
# Model estimation
print(model.call)
eval(parse(text = model.call))
the.model <- eval(parse(text = model.name))
# Add the level labels for the target and predictors, along with the target
# counts to the model object
if (is.OSR) {
	ylevels <- levels(the.data[[1]])
}
if (is.XDF) {
	target.info <- eval(parse(text = paste("rxSummary(~ ", name.y.var, ", data = xdf.path)$categorical", sep = "")))
	the.model$yinfo <- list(levels = as.character(target.info[[1]][,1]), counts = target.info[[1]][,2])
	ylevels <- the.model$yinfo$levels
	the.model$xlevels <- eval(parse(text = paste("xdfLevels(~ ", x.vars, ", xdf.path)")))
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
if (is.XDF) {
	singular <- FALSE
	glm.out <- AlteryxReportRx(the.model, null.model$deviance)
}
if (is.Pivotal) {
	singular <- FALSE
	glm.out <- AlteryxReportMadLib(the.model)
}
# Put the name of the model as the first entry in the key entry in the
# key-value table.
glm.out <- rbind(c("Model_Name", model.name), glm.out)
# Add the type "binomial" to the key-value pair table if non-weighted
if (!used.weights || !is.OSR)
	glm.out <- rbind(glm.out, c("Model_Type", "binomial"))
# Add the type "quasibinomial" to the key-value pair table if weights used
if (used.weights && is.OSR)
	glm.out <- rbind(glm.out, c("Model_Type", "quasibinomial"))
# If the ANOVA table is requested then create it and add its results to the
# key-value table. Its creation will be surpressed if the car package isn't
# present, if their were singularities in estimation, or if the input is an
# XDF file. 
if (has.car && !singular && is.OSR) {
	print(Anova(the.model, type="II")) # Write to the Output window
	glm.out <- rbind(glm.out, Alteryx.ReportAnova(the.model))
}
if (singular && !is.XDF)
	AlteryxMessage("Creation of the Analysis of Deviance table was surpressed due to singularities", iType = 2, iPriority = 3) 
if (!is.OSR)
	AlteryxMessage("Creation of the Analysis of Deviance tables was surpressed due to the use of an XDF file", iType = 2, iPriority = 3) 
# Write out the key-value pair table to Alteryx
write.Alteryx(glm.out)
# Prepare the basic regression diagnostic plots if it is requested
# and their isn't the combination of singularities and the use of
# sampling weights or 
if (!(singular && used.weights) && is.OSR) {
	whr <- graphWHR(inches = "True", in.w = 6, in.h = 6, resolution ='%Question.graph.resolution%')
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
the.obj[[1]] <- c(model.name)
the.obj[[2]] <- list(the.model)
names(the.obj) <- c("Name", "Object")
#levels.list <- list(levels = ylevels)
#levels.json <- toJSON(levels.list)
#print(levels.json)
#write.Alteryx(the.obj, source = "Hello World", nOutput = 3)
write.Alteryx(the.obj, nOutput = 3)
# Clean-up any database connections
if (is.Pivotal) {
	all.tables <- db.objects(conn.id = the.conn)
	madlib.tables <- all.tables[substr(all.tables, 1, 18) == "public.madlib_temp"]
	if (length(madlib.tables) > 0) {
		madlib.tables <- substr(madlib.tables, 8, nchar(madlib.tables))
		for (i in madlib.tables) {
			this.table <- i
			delete(x = this.table, conn.id = the.conn, is.temp = FALSE)
		}
	}
	db.disconnect(conn.id = the.conn, verbose = FALSE)
}
