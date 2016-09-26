#' ---
#' title: Logistic Regression
#' author: Dan Putler, Kuo Liu, Ramnath Vaidyanathan
#' output:
#'   html_document:
#'     toc: true
#'     toc_depth: 4
#' ---

#' #### Read Configuration
#'
#'
## DO NOT MODIFY: Auto Inserted by AlteryxRhelper ----
suppressWarnings(library(AlteryxPredictive))
config <- list(
  `graph.resolution` = dropdownInput('%Question.graph.resolution%' , '1x'),
  `the.link` = dropdownInput('%Question.Link%' , 'logit'),
  `model.name` = textInput('%Question.Model Name%'),
  `used.weights` = checkboxInput('%Question.Use Weights%' , FALSE),
  `Weight Vec` = dropdownInput('%Question.Weight Vec%'),
  `X Vars` = listInput('%Question.X Vars%', c('Sepal.Length', 'Petal.Length')),
  `Y Var` = dropdownInput('%Question.Y Var%', 'Species')
)
options(alteryx.wd = '%Engine.WorkflowDirectory%')
options(alteryx.debug = config$debug)
##----

#' #### Read Inputs
#'
#' This is a named list of all inputs that stream into the R tool.
#' We also specify defaults for use when R code is run outside Alteryx.
iris$Species <- as.factor(ifelse(iris$Species != "setosa", "other", "setosa"))
defaults <- list(
  data = iris[,c(config$`Y Var`, config$`X Vars`)]
)
inputs <- list(
  the.data = read.Alteryx2("#1", default = defaults$data),
  XDFInfo = getXdfProperties("#1", default = list(is_XDF = FALSE, xdf_path = NULL))
)

#' ### Run and Create Outputs
runLogisticRegression <- function(inputs, config){
  library(car)
  
  #' Modify the link so that it can be passed on to R.
  if (config$the.link == "complementary log-log"){
    config$the.link <- "cloglog"
  }
  
  if (inputs$XDFInfo$is_XDF){
    d <- processLogisticXDF(inputs, config)
    glm.out <- createReportLogisticXDF(d$the.model, config, d$null.model)
    plot.out <- function(){createPlotOutputsLogisticXDF()}
  } else {
    d <- processLogisticOSR(inputs, config)
    glm.out <- createReportLogisticOSR(d$the.model, config, d$model_type)
    plot.out <- function(){
      createPlotOutputsLogisticOSR(d$the.model, FALSE, config)
    }
  }
  
  # Report Output
  write.Alteryx2(glm.out, nOutput = 1)
  
  # Plot Output
  whr <- graphWHR2(inches = TRUE, in.w = 6, in.h = 6, config$graph.resolution)
  AlteryxGraph2(plot.out(), 2, width = whr[1], height = whr[2],
    res = whr[3], pointsize = 9)
  
  # Model Output
  the.obj <- prepModelForOutput(config$model.name, d$the.model)
  write.Alteryx2(the.obj, nOutput = 3)  
}

runLogisticRegression(inputs, config)

