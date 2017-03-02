config <- list(
  `graph.resolution` = dropdownInput('%Question.graph.resolution%' , '1x'),
  `Link` = dropdownInput('%Question.Link%' , 'logit'),
  `Model Name` = textInput('%Question.Model Name%'),
  `Use Weights` = checkboxInput('%Question.Use Weights%' , FALSE),
  `Weight Vec` = dropdownInput('%Question.Weight Vec%'),
  `X Vars` = listInput('%Question.X Vars%', c('Sepal.Length', 'Petal.Length')),
  `Y Var` = dropdownInput('%Question.Y Var%', 'Species'),
  regularization = checkboxInput('%Question.regularization%', FALSE),
  alpha = numericInput('%Question.alpha%', .5),
  lambda_1se = radioInput('%Question.lambda_1se%', TRUE),
  lambda_min = radioInput('%Question.lambda_min%', FALSE),
  standardize_pred = checkboxInput('%Question.standardize_pred%', TRUE),
  internal_cv = checkboxInput('%Question.internal_cv%', TRUE),
  set_seed_internal_cv = checkboxInput('%Question.set_seed_internal_cvv%', TRUE),
  seed_internal_cv = numericInput('%Question.seed_internal_cv%', 1),
  nfolds = numericInput('%Question.nfolds%', 5),
  lambda_no_cv = numericInput('%Question.lambda_no_cv%', NULL),
  display_graphs = checkboxInput('%Question.display_graphs%', TRUE),
  external_cv = checkboxInput('%Question.external_cv%', FALSE),
  nfolds_external = numericInput('%Question.nfolds_external%', NULL),
  set_seed_external_cv = checkboxInput('%Question.set_seed_external_cv%', FALSE),
  external_seed_value = numericInput('%Question.external_seed_value%', NULL),
  `Omit Constant` = checkboxInput('%Question.Omit Constant%' , FALSE)
)

options(alteryx.wd = '%Engine.WorkflowDirectory%')
options(alteryx.debug = config$debug)
##----
#' ### Read Inputs
#'
#' This is a named list of all inputs that stream into the R tool.
#' We also specify defaults for use when R code is run outside Alteryx.
inputs <- list(
  the.data = read.Alteryx2("#1", default = mtcars),
  XDFInfo = getXdfProperties("#1", default = list(is_XDF = FALSE, xdf_path = NULL))
)
checkLowN(
  data = inputs$the.data,
  threshold = 25,
  mult = 1,
  msg = 
    XMSG(
      in.targetString_sc = "The incoming data may not have enough rows to generate a model successfully."
    )
)
config$lambda.1se <- config$lambda_1se
config$lambda.min <- config$lambda_min
AlteryxPredictive:::runLogisticRegression(inputs, config)
