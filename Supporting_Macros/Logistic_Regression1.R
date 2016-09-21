# Determine if the "car" package is needed and, if it is, determine if it is
# available.
has.car <- "car" %in% row.names(installed.packages())
if(has.car) {
	library(car)
} else {
	warning("Unable to find the car package")
}

library(AlteryxPredictive)
## Create two lists: "config" and "inputs" ----
config <- list(
  used.weights = checkboxInput('%Question.Use Weights%', FALSE),
  model.name   = validName(textInput('%Question.Model Name%')),
  the.link     = dropdownInput('%Question.Link%', "logit"),
  resolution   = dropdownInput('%Question.graph.resolution%') 
)

if(config$the.link == "complementary log-log")
  config$the.link <- "cloglog"

inputs <- list(
  meta.data = read.AlteryxMetaInfo("#1"),
  the.data  = read.Alteryx("#1")
)


## The core portion of the macro ----
library(car)
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


var_names <- getNamesFromOrdered(names(inputs$the.data), config$used.weights)
name.x.vars <- var_names$x
name.y.var <- var_names$y
the.weights <- var_names$w


# Adjust the set of field names to remove the weight field if weights are used
if (config$used.weights) {
	if (is.OSR) {
		library(survey)
		the.design <- svydesign(ids = ~1, weights = makeFormula(the.weights,""), data = inputs$the.data)
	} else if (XDFInfo$flag) {
			weight.arg <- paste(", pweights = '", the.weights, "'", sep = "")	 
	}
}


# Make sure the target is binary 
if (is.OSR && length(unique(inputs$the.data[,1])) != 2)
	stop.Alteryx("The target variable must only have two unique values.")
if (XDFInfo$flag) {
	len.target <- eval(parse(text = paste("length(rxGetVarInfo(XDFInfo$path)$", name.y.var, "$levels)", sep = "")))
	if(len.target != 2)
		stop.Alteryx("The target variable must only have two unique values.")
}

the.formula <- makeFormula(name.x.vars, name.y.var)

# The call elements when the input is a true data frame (not a schema stream)
if (is.OSR) {
	if (config$used.weights) {
		the.family <- paste("quasibinomial(", config$the.link, ")", sep="")
  } else {
    the.family <- paste("binomial(", config$the.link, ")", sep="")
   }
	model.call <- paste(config$model.name, ' <- glm(', the.formula, ', family = ', the.family, ', data = inputs$the.data)', sep="")
  model.call <- 
	# The model call if a sampling weights are used in estimation
	if (config$used.weights)
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
