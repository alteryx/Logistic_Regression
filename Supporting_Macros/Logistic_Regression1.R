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
  `Link` = dropdownInput('%Question.Link%' , 'logit'),
  `Model Name` = textInput('%Question.Model Name%'),
  `Use Weights` = checkboxInput('%Question.Use Weights%' , FALSE),
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


AlteryxPredictive:::runLogisticRegression(inputs, config)

