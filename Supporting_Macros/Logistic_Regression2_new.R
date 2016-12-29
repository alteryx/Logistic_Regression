suppressWarnings(library(AlteryxPredictive))

outer_config <- list(
  `graph.resolution` = dropdownInput('%Question.graph.resolution%' , '1x'),
  `Link` = dropdownInput('%Question.Link%' , 'logit'),
  `Model Name` = textInput('%Question.Model Name%'),
  `Use Weights` = checkboxInput('%Question.Use Weights%' , FALSE),
  `Weight Vec` = dropdownInput('%Question.Weight Vec%'),
  `X Vars` = listInput('%Question.X Vars%', c('Sepal.Length', 'Petal.Length')),
  `Y Var` = dropdownInput('%Question.Y Var%', 'Species'),
  regularization = checkboxInput('%Question.regularization%', FALSE),
  alpha = numericInput('%Question.alpha%', .5),
  lambda_1se = radioInput('%Question.lambda.1se%', TRUE),
  lambda_min = radioInput('%Question.lambda.min%', FALSE),
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
options(alteryx.debug = outer_config)


if (!(outer_config$regularization)) {
  mod.df <- read.Alteryx("#1")
  mod.obj <- unserializeObject(as.character(mod.df$Object[1]))
  the.class <- class(mod.obj)[1]
  write.Alteryx(data.frame(Class = the.class))
}
if (outer_config$external_cv) {
  ###########################################
  #' ---
  #' title: Cross Validation Macro
  #' author: Bridget Toomey
  #' ---
  #' 
  #' 
  #' Utility function to check to see if the necessary packages are
  #' installed and install them if they're not.
  #'
  #'
  #' @param packages vector of package names to check and install.
  #' @export
  #' @author Bridget Toomey, Dan Putler
  checkInstalls <- function(packages) {
    # See if the desired packages are installed, and install if they're not
    if (!all(packages %in% row.names(installed.packages()))) {
      # Use the IE based "Internet2" since it is most reliable for this action,
      # it will be switched back at the end
      #setInternet2(use = TRUE)
      # Make sure the path to the users library is in place and create it if it
      # is not
      minor_ver <- strsplit(R.Version()$minor, "\\.")[[1]][1]
      R_ver <- paste(R.Version()$major, minor_ver, sep = ".")
      the_path <- paste0(normalizePath("~"), "\\R\\win-library\\", R_ver)
      # Create the user's personal folder if it doesn't already exist
      if (!dir.exists(the_path)) {
        dir.create(the_path, recursive = TRUE, showWarnings = FALSE)
      }
      # The set of possible repositories to use
      repos <- c("http://cran.revolutionanalytics.com", "https://cran.rstudio.com")
      # Select a particular repository
      repo <- sample(repos, 1)
      repo <- c(repo, "https://alteryx.github.io/drat")
      missingPackages <- packages[which(!(packages %in% row.names(installed.packages())))]
      install.packages(missingPackages, lib = the_path, repos = repo)
      #setInternet2(use = FALSE)
    }
  }
  
  checkInstalls(c("ROCR", "plyr", "TunePareto", "sm", "vioplot", "ggplot2", "AlteryxPredictive"))
  library(ROCR)
  library("TunePareto")
  library("sm")
  library("vioplot")
  library("ggplot2")
  
  #Make sure the user has at least AlteryxPredictive 0.3.1, since that version
  #contains scoreModel2
  if (packageVersion("AlteryxPredictive") < '0.3.2') {
    setInternet2(use = TRUE)
    the_path <- paste0(normalizePath("~"), "\\R\\win-library\\", R.Version()$major, ".", substr(R.Version()$minor, start = 1, stop = 1))
    install.packages("AlteryxPredictive", lib = the_path, repos = "https://alteryx.github.io/drat")
    setInternet2(use = FALSE)
  }
  
  #' ### Read Configuration
  #' ## DO NOT MODIFY: Auto Inserted by AlteryxRhelper ----
  suppressWarnings(library(AlteryxPredictive))
  config <- list(
    `classification` = TRUE,
    `modelType` = NULL,
    `numberFolds` = numericInput('%Question.nfolds_external%' , 5),
    `numberTrials` = numericInput('%Question.numberTrials%' , 3),
    #This should be added as a config option in Logistic Regression, but it's irrelevant
    #here, since we're solving a regression problem
    `posClass` = NULL,
    `regression` = FALSE,
    `stratified` = FALSE,
    `seed` = numericInput('%Question.seed%', 1)
  )
  config <- append(config, outer_config)
  options(alteryx.wd = '%Engine.WorkflowDirectory%')
  options(alteryx.debug = config$debug)
  ##----
  
  
  library(plyr)
  #' ### Defaults
  #' These defaults are used when the R code is run outside Alteryx
  if (!inAlteryx()){
    macroDirectory <- textInput('%Engine.WorkflowDirectory%', "Supporting_Macros")
    dataDir <- file.path(macroDirectory, "Data")
    defaults <- list(
      data = readRDS(file.path(dataDir, "data.rds")),
      models = readRDS(file.path(dataDir, "models.rds"))
    )
  }
  
  #' ### Inputs
  #' 
  #' This is a named list of all inputs that stream into the R tool.
  inputs <- list(
    data = read.Alteryx2("#2", default = defaults$data),
    models = readModelObjects("#1", default = defaults$models)
  )
  
  
  ##---- Inputs/Config Complete
  
  #' ### Helper Functions
  areIdentical <- function(v1, v2){
    identical(sort(v1), sort(v2))
  }
  
  #' Helper Functions For Tests
  makePayload <- function(csvFile, modelsFile){
    test_data <- read.csv(csvFile)
    list(
      #data = test_data[, c(targetVar, setdiff(names(test_data), targetVar))],
      data = test_data,
      models = AlteryxPredictive::readModelObjects("2", readRDS(modelsFile))
    )
  }
  
  runTest <- function(modelName, payload){
    inputs <- list(data = payload$data, models = payload$models[modelName])
    message(paste(modelName, 'passes'))
    test_that(paste(modelName, 'passes'), {
      expect_that(invisible(getResultsCrossValidation(inputs, config)), throws_error(NA) )
    })
  }
  
  #' ## Check predictor variables
  #' 
  #' Check if predictor variables in the models and input data are identical.
  checkXVars <- function(inputs){
    numModels <- length(inputs$models)
    modelNames <- names(inputs$models)
    modelXVars <-  if (packageVersion('AlteryxPredictive') <= '0.3.2'){
      lapply(inputs$models, getXVars2)
    } else {
      lapply(inputs$models, getXVars)
    }
    dataXVars <- names(inputs$data)[which(names(inputs$data) %in% unlist(modelXVars))]
    errorMsg <- NULL
    if (numModels > 1) {
      for (i in 1:(numModels - 1)){
        mvars1 <- modelXVars[[i]]
        mvars2 <- modelXVars[[i + 1]]
        if (!areIdentical(mvars1, mvars2)){
          errorMsg <- paste("Models", modelNames[i] , "and", modelNames[i + 1],
                            "were created using different predictor variables.")
          stopMsg <- "Please ensure all models were created using the same predictors."
        } 
        else if (!all(mvars1 %in% dataXVars)){
          errorMsg <- paste("Model ", modelNames[i], 
                            "used predictor variables which were not contained in the input data.")
          stopMsg <- paste("Please ensure input data contains all the data",
                           "used to create the models and try again.")
        }
        if (!is.null(errorMsg)){
          AlteryxMessage2(errorMsg, iType = 2, iPriority = 3)
          stop.Alteryx2(stopMsg)
        }
      }
    } else {
      mvars1 <- modelXVars[[1]]
      if (!all(mvars1 %in% dataXVars)){
        errorMsg <- paste("Model ", modelNames[1], 
                          "used predictor variables which were not contained in the input data.")
        stopMsg <- paste("Please ensure input data contains all the data",
                         "used to create the models and try again.")
      }
      if (!is.null(errorMsg)){
        AlteryxMessage2(errorMsg, iType = 2, iPriority = 3)
        stop.Alteryx2(stopMsg)
      }
    }
  }
  
  #' Given a factor variable and a set of records in a fixed trial and fold,
  #' return the list of classes not present in that trial and fold.
  getMissingClasses <- function(currentClasses, currentRecords) {
    currentClasses[(!(currentClasses %in% currentRecords))]
  }
  
  #' For each factor variable, check to see if all levels are present in each fold. 
  #' If not, warn the user.
  #' 
  
  checkFactorVars <- function(data, folds, config) {
    #All of the discrete variables will be some type of string in Alteryx. So they'll be read as factors, since stringsAsFactors is TRUE in read.Alteryx.
    factorVars <- data[,sapply(data, FUN = is.factor), drop = FALSE]
    #We only need to check if there's at least one factor variable. If all variables are continuous, we don't need to do anything.
    if (NCOL(factorVars) > 0) {
      for (k in 1:NCOL(factorVars)) {
        uniqueClasses <- unique(factorVars[,k])
        currentVar <- factorVars[,k]
        #We want to check if one of the folds on one of the trials is missing any classes.
        #If a class is missing from a fold, we output a warning suggesting that the user check their data/try to collect more data.
        #If a training set is missing a class, we output a fatal error telling the user they must ensure
        #that each training set contains all classes.
        for (i in 1: (config$numberTrials)) {
          for (j in 1:(config$numberFolds)) {
            currentTestRecords <- currentVar[unlist(folds[[i]][j])]
            currentTrainingRecords <- currentVar[unlist(folds[[i]][-j])]
            missingTestClasses <- getMissingClasses(currentClasses = uniqueClasses, currentRecords = currentTestRecords)
            missingTrainingClasses <- getMissingClasses(currentClasses = uniqueClasses, currentRecords = currentTrainingRecords)
            #testing if all classes are represented in trial i, fold j
            if (length(missingTestClasses) > 0) {
              currentColumnName <- colnames(factorVars)[k]
              if (length(missingTestClasses) > 1) {
                warningMessage1 <- paste0("Classes ", missingTestClasses, " were not present in variable ", currentColumnName, " of the test set.")
                warningMessage2 <- "It is recommended that you either check your data to ensure no records were mis-labeled or collect more data on these classes."
              } else {
                warningMessage1 <- paste0("Class ", missingTestClasses, " was not present in variable ", currentColumnName, " of the test set.")
                warningMessage2 <- "It is recommended that you either check your data to ensure no records were mis-labeled or collect more data on this class."
              }
              AlteryxMessage2(warningMessage1)
              AlteryxMessage2(warningMessage2)
            }
            #testing if all classes are represented in the training set when trial i, fold j is the test set. 
            #So the training set here is trial i, all folds except fold j.
            if (length(missingTrainingClasses) > 0) {
              currentColumnName <- colnames(factorVars)[k]
              if (length(missingTrainingClasses) > 1) {
                warningMessage1 <- paste0("Classes ", missingTrainingClasses, " were not present in variable ", currentColumnName," of the training set.")
                warningMessage2 <- "It is recommended that you either check your data to ensure no records were mis-labeled or collect more data on these classes."
                errorMessage <- "It is very difficult to create an accurate model when the training set is missing a class."
              } else {
                warningMessage1 <- paste0("Class ", missingTrainingClasses, " was not present in variable ", currentColumnName, " of the training set.")
                warningMessage2 <- "It is recommended that you either check your data to ensure no records were mis-labeled or collect more data on this class."
                errorMessage <- "It is very difficult to create an accurate model when the training set is missing classes."
              }
              AlteryxMessage2(warningMessage1)
              AlteryxMessage2(warningMessage2)
              AlteryxMessage2(errorMessage)
            }
          }
        }
      }
    }
  }
  
  
  #Create the list of cross-validation folds and output warnings/errors as appropriate
  createFolds <- function(data, config) {
    target <- data[, 1]
    set.seed(config$seed)
    foldList <- generateCVRuns(labels = target, ntimes = config$numberTrials, nfold = config$numberFolds, stratified = config$stratified)
    checkFactorVars(data = data, folds = foldList, config = config)
    return(foldList)
  }
  
  
  
  #Check if response variable is the same in the pre-built model(s) and the input data.
  #If so, output this variable.
  getYvars <- function(data, models) {
    # Get the names of the target fields and make sure they are all same. If not,
    # throw an error.
    y_names <- sapply(models, AlteryxPredictive:::getYVar)
    if (!all(y_names == y_names[1])) {
      stop.Alteryx2("More than one target variable are present in the provided models")
    } else if (!(y_names[1] %in% colnames(data))) {
      stop.Alteryx2("The target variable from the models is different than the target chosen in the configuration. Please check your configuration settings and try again.")
    }
    # get the target variable name
    y_name <- y_names[1]
    # Get the target variable
    return(list(y_col = data[[y_name]], y_name = y_name))
  }
  
  #In the 2-class classification case, get the positive class. Otherwise, do nothing.
  getPosClass <- function(config, yVar) {
    
    #Use the function from the Model Comparison tool to get/set positive class:
    setPositiveClass <- function(tar_lev) {
      # Set the postive class for two-class classification.
      # The setPositiveClass function is only triggered if the user leaves the
      # question on positive class (target level) blank.
      #   1) if there's "yes/Yes/YES ..." in the target variable, then use "yes/Yes/YES"
      #   2) if there's "true/True/TRUE..." in the target variable, then use "true/True/TRUE"
      #   3) otherwise: use the first level by alphabetical order.
      #
      # Parameters:
      #   tar_lev: a vector of string
      #            the levels of the target variable.
      #
      # Returns:
      #   no_name: a string, the name of the positive class.
      
      yes_id <- match("yes", tolower(tar_lev))
      true_id <- match("true", tolower(tar_lev))
      if (!is.na(yes_id)) {
        return (tar_lev[yes_id])
      } else if (!is.na(true_id)) {
        return (tar_lev[true_id])
      } else {
        return (tar_lev[1])
      }
    }
    return(setPositiveClass(yVar))
  }
  
  adjustGbmModel <- function(model){
    method <- if (model$cv.folds > 1){
      "cv"
    } else if (model$train.function < 1){
      "test"
    } else {
      "OOB"
    }
    model$best.trees <- gbm.perf(model, method = method)
    return(model)
  }
  
  #   naiveBayesUpdate <- function(model, trainingData, currentYvar) {
  #     
  #     if (is.null(model$laplace)) {
  #       model$laplace <- 0
  #       AlteryxMessage2("The Laplace smoothing parameter was not saved in the model object.", iType = 2, iPriority = 3)
  #       AlteryxMessage2("Hence, we are using a smoothing parameter of 0 for Cross-Validation.", iType = 2, iPriority = 3)
  #     }
  #     predictors <- trainingData[,-(which(colnames(trainingData) == currentYvar))]
  #     response <- trainingData[,(which(colnames(trainingData) == currentYvar))]
  #     naiveBayes.default <- getS3method("naiveBayes", "default")
  #     currentModel <- update(model, x = predictors, y = response, laplace = model$laplace)
  #     return(currentModel)
  #   }
  glmnetUpdate <- function(model, trainingData, currentYvar, config, weight_vec = NULL) {
    predictors <- trainingData[,AlteryxPredictive:::getXVars(model)]
    response <- trainingData[,(which(colnames(trainingData) == currentYvar))]
    library(glmnet)
    model_w_call <- if (config$internal_cv) {
      model$glmnet.fit
    } else {
      model
    }
    if (config$`Use Weights`) {
      currentModel <- update(model_w_call, x = predictors, y = response, weights = weight_vec)
    } else {
      #currentModel <- update(model, formula. = makeFormula(AlteryxPredictive:::getXVars(model), currentYvar), data = trainingData)
      currentModel <- update(model_w_call, x = predictors, y = response)
    }
    currentModel$xvars <- colnames(predictors)
    currentModel$lambda_pred <- if (config$internal_cv) {
      if (config$lambda_1se) {
        model$lambda.1se
      } else {
        model$lambda.min
      }
    } else {
      config$lambda_no_cv
    }
    return(currentModel)
  }
  
  #' Given a model, a dataset and index of test cases, return actual and response
  getActualandResponse <- function(model, data, testIndices, extras, mid, config){
    trainingData <- data[-testIndices,]
    testData <- data[testIndices,]
    testData <- matchLevels(testData, getXlevels(model))
    currentYvar <- extras$y_name
    #Check if the model is Naive Bayes and lacking a Laplace parameter.
    #If so, set the Laplace parameter to 0 and warn the user.
    #     if (inherits(model, "naiveBayes")) {
    #       currentModel <- naiveBayesUpdate(model, trainingData, currentYvar)
    #     } else 
    if ((inherits(model, "cv.glmnet")) || (inherits(model, "glmnet"))) {
      #Ideally, it would be more efficient to convert the x df to a matrix earlier so that
      #this conversion wouldn't be necessary with every trial/fold. However, the code assumes
      #that we're dealing with a df in many other places. This are could be ripe for refactoring
      #in the future.
      weights_v <- trainingData[[config$`Weight Vec`]]
      trainingData <- AlteryxPredictive:::df2NumericMatrix(trainingData)
      #No need to call df2NmericMatrix on testData, since scoreModel calls df2NumericMatrix with glmnet models.
      currentModel <- glmnetUpdate(model, trainingData, currentYvar, config, weight_vec = weights_v)
    } else {
      if (config$`Use Weights`) {
        # WORKAROUND
        # The assign() statement below moves the token ‘getActualandResponse’ to the global environment, where the update() function can find it.  
        # Otherwise, something inside update() isn’t finding ‘getActualandResponse’ on its environment search path.
        #assign(x = 'trainingDatagetActualandResponse403', value = trainingData, envir = globalenv())
        my_envir <- environment()
        lapply(
          X = 1:ncol(trainingData),
          FUN = function(i){
            assign(
              x = names(trainingData)[i],
              value = trainingData[,i],
              envir = my_envir
            )
          }
        )
        currentModel <- update(model, formula. = makeFormula(AlteryxPredictive:::getXVars(model), currentYvar), data = environment(), weights = trainingData$`Weight Vec`)
      } else {
        currentModel <- update(model, formula. = makeFormula(AlteryxPredictive:::getXVars(model), currentYvar), data = trainingData)
      }
    }
    if (inherits(currentModel, 'gbm')){
      currentModel <- adjustGbmModel(currentModel)
    }
    pred <- if (packageVersion('AlteryxPredictive') <= '0.3.2') {
      AlteryxPredictive::scoreModel2(currentModel, new.data = testData)
    } else {
      AlteryxPredictive::scoreModel(currentModel, new.data = testData)
    }
    actual <- (extras$yVar)[testIndices]
    recordID <- (data[testIndices,])$recordID
    response <- pred$Score
    return(data.frame(recordID = recordID, response = response, actual = actual))
  }
  
  safeGetActualAndResponse <- failwith(NULL, getActualandResponse, quiet = FALSE)
  
  #' 
  getCrossValidatedResults <- function(inputs, allFolds, extras, config){
    function(mid, trial, fold){
      model <- inputs$models[[mid]]
      testIndices <- allFolds[[trial]][[fold]]
      out <- (safeGetActualAndResponse(model, inputs$data, testIndices, extras, mid, config))
      if (is.null(out)) {
        AlteryxMessage2(paste0("For model ", mid, " trial ", trial, " fold ", fold, " the data could not be scored."), iType = 2, iPriority = 3)
      } else {
        out <- cbind(trial = trial, fold = fold, mid = mid, out)
      }
      return(out)
    }
  }
  
  getPkgListForModels <- function(models){
    modelClasses <- unlist(lapply(models, class))
    pkgMap = list(
      gbm = "gbm", rpart = "rpart", svm.formula = "e1071", svm = "e1071",
      naiveBayes = "e1071", svyglm = "survey", nnet.formula = "nnet",
      randomForest.formula = "randomForest", earth = "earth", glmnet = "glmnet",
      elnet = "glmnet", cv.glmnet = "glmnet", lognet = "glmnet"
    )
    unique(unlist(pkgMap[modelClasses]))
  }
  
  #Get the necessary measures in the regression case
  getMeasuresRegression <- function(outData, extras) {
    actual <- unlist(outData$actual)
    predicted <- unlist(outData$response)
    modelIndic <- outData$mid
    trialIndic <- outData$trial
    foldIndic <- outData$fold
    err <- actual - predicted
    rmse <- sqrt(mean(err*err))
    mae <- mean(abs(err))
    # When there are values near 0 in the target variable, it can lead to an attempt to divide by 0
    # In this case, use the weighted version.
    if (any(abs(actual) < 0.001)) {
      AlteryxMessage("The target variable contains values very close to 0 (-0.001, 0.001). WPE and WAPE are being used instead of MPE and MAPE.", iType = 2, iPriority = 3)
      mpe <- 100 * sum(err) / sum(actual)
      mape <- 100 * sum(abs(err)) / sum(actual)
    } else {
      mpe <- 100*mean(err/actual)
      mape <- 100*mean(abs(err/actual))
    }
    data.frame(
      Correlation = cor(predicted, actual), RMSE = rmse, MAE = mae, MPE= mpe, MAPE = mape
    )
  }
  
  #Get the necessary measures in the classification case
  getMeasuresClassification <- function(outData, extras) {
    actual <- as.character(outData$actual)
    scoredData <- outData[,7:8]
    scoredOutput <- as.character(outData$response)
    posClass <- extras$posClass
    modelIndic <- unique(outData$mid)
    trialIndic <- unique(outData$trial)
    foldIndic <- unique(outData$fold)
    overallAcc <- sum(actual == scoredOutput)/length(actual)
    if (length(extras$levels) == 2) {
      true_y <- factor(TRUE*(actual == posClass)) # if levels are strings rather than TRUE/FALSE
      #We need to know which column of scoredData corresponds to the positive class in order to set up the needed intermediate steps for obtaining the AUC
      posClassCol <- which((extras$levels) == posClass)
      negClassCol <- which((extras$levels) != posClass)
      predictions <- scoredData[,posClassCol]
      predictionObj <- prediction(predictions = predictions, labels = actual)
      
      # =================================================================
      # Quick Reference:
      #       precision = tp / (tp + fp)
      #          recall = tp / (tp + fn)
      #             tpr = tp / (tp + fn)
      #             fpr = fp / (fp + tn)
      #              f1 = 2 * precision * recall / (precision + recall)
      # ==================================================================
      
      #     perf_acc <- performance(predictionObj, "acc", "cutoff")
      #     perf_lift <- performance(predictionObj, "lift", "rpp")
      #     perf_gain <- performance(predictionObj, "tpr", "rpp")
      #     perf_roc <- performance(predictionObj, "tpr", "fpr")
      #     perf_pr <- performance(predictionObj, "prec", "rec")
      actualPosIndic <- which(actual == posClass)
      nActualPos <- length(actualPosIndic)
      nCorrectPos <- sum(scoredOutput[actualPosIndic] == posClass)
      nPredPos <- sum(scoredOutput == posClass)
      precision <- nCorrectPos/nPredPos
      recall <- nCorrectPos/nActualPos
      F1 <- 2*(precision*recall)/(precision + recall)
      AUC <- performance(prediction.obj = predictionObj, measure = "auc")
      AUC <- unlist(AUC@y.values)
      percentClass1Right <- sum(scoredOutput[which(actual == (extras$levels)[1])] == (extras$levels)[[1]])/length(which(actual == (extras$levels)[1]))
      percentClass2Right <- sum(scoredOutput[which(actual == (extras$levels)[2])] == (extras$levels)[[2]])/length(which(actual == (extras$levels)[2]))
      outVec <- c(mid = modelIndic, trial = trialIndic, fold = foldIndic, Accuracy_Overall = overallAcc, Accuracy_Class_1 = percentClass1Right, Accuracy_Class_2 = percentClass2Right, F1 = F1, AUC = AUC)
    } else {
      #Compute accuracy by class
      outVec <- vector(length = length((extras$levels)))
      for (l in 1:length((extras$levels))) {
        tempPred <- scoredOutput[actual == (extras$levels)[[l]]]
        nCorrect <- sum(tempPred == (extras$levels)[[l]])
        thisAcc <- nCorrect/sum(actual == (extras$levels)[[l]])
        outVec[l] <- thisAcc
        names(outVec)[l] <- paste0("Accuracy_Class_", l)
      }
      outVec <- c(mid = modelIndic, trial = trialIndic, fold = foldIndic, Accuracy_Overall = overallAcc, outVec)
    }
    return(outVec)
  }
  
  #' ### Functions to Generate Output
  #' 
  generateConfusionMatrices <- function(outData, extras) {
    outvec <- vector(length = length(extras$levels))
    pasteClass <- function(nameOfClass) {
      paste0("Class_", nameOfClass)
    }
    names(outvec) <- sapply(X = (extras$levels), FUN = pasteClass, simplify = TRUE)
    for (i in 1:length(extras$levels)) {
      outvec[i] <- length(which((outData$actual) == ((extras$levels)[i])))
    }
    return(c(mid = unique(outData$mid), trial = unique(outData$trial), fold = unique(outData$fold), Predicted_class = as.character(unique(outData$response)), outvec))
  }
  
  generateOutput3 <- function(data, extras, modelNames) {
    d <- ddply(data, .(trial, fold, mid, response), generateConfusionMatrices, 
               extras = extras
    )
    d$Model <- modelNames[as.numeric(d$mid)]
    d$Type <- rep.int('Classification', times = length(d$Model))
    d <- subset(d, select = -c(mid, response))
    d <- reshape2::melt(d, id = c('trial', 'fold', 'Model', 'Type', 'Predicted_class'))
    colnames(d) <- c('Trial', 'Fold', 'Model', 'Type', 'Predicted_class', 'Variable', 'Value')
    return(d)
  }
  
  generateOutput2 <- function(data, extras, modelNames) {
    fun <- if (is.null(extras$levels)) {
      getMeasuresRegression 
    } else {
      getMeasuresClassification
    }
    d <- ddply(data, .(trial, fold, mid), fun, extras = extras)
    d$Model <- modelNames[as.numeric(d$mid)]
    d <- subset(d, select = -c(mid))
    return(d)
  }
  
  generateOutput1 <- function(inputs, config, extras){
    pkgsToLoad <- getPkgListForModels(inputs$models)
    for (pkg in pkgsToLoad) library(pkg, character.only = TRUE)
    allFolds <- extras$allFolds
    g <- expand.grid(
      mid = seq_along(inputs$models),
      trial = seq_along(allFolds),
      fold = seq_along(allFolds[[1]])
    )
    return(mdply(g, getCrossValidatedResults(inputs, allFolds, extras, config)))
  }
  
  computeBinaryMetrics <- function(pred_prob, actual, threshold){
    #Pred_prob gives the predicted probability of belonging to the positive class
    #Actual is true if the record belongs to the positive class and negative if not
    actualPosIndic <- which(actual == TRUE)
    actualNegIndic <- which(actual == FALSE)
    nActualPos <- length(actualPosIndic)
    thresholdedPredictions <- (pred_prob >= threshold)
    nCorrectPos <- sum(thresholdedPredictions[actualPosIndic])
    nPredPos <- sum(thresholdedPredictions)
    nPredNeg <- length(actual) - length(nPredPos)
    nCorrectNeg <- sum(1 - (thresholdedPredictions[-actualPosIndic]))
    overallAcc <- sum(thresholdedPredictions == actual)/length(actual)
    PosAcc <- length(intersect(which(thresholdedPredictions == TRUE), actualPosIndic))/length(actualPosIndic)
    NegAcc <- length(intersect(which(thresholdedPredictions == FALSE), actualNegIndic))/length(actualNegIndic)
    precision <- nCorrectPos/nPredPos
    recall <- nCorrectPos/nActualPos
    F1 <- 2*(precision*recall)/(precision + recall)
    tpr <- recall
    rpp <- nPredPos/length(pred_prob)
    lift <- tpr/rpp
    fpr <- (nPredPos - nCorrectPos)/length(actualNegIndic)
    pred <- prediction(predictions = pred_prob, labels = actual)
    auc <- performance(pred, "auc")
    auc <- unlist(auc@y.values)
    data.frame(threshold = threshold, recall = recall, F1 = F1, lift = lift, Rate_Pos_Predictions = rpp, True_Pos_Rate = tpr, False_Pos_Rate = fpr, Precision = precision)
  }
  
  generateDataForPlots <- function(d, extras, config){
    data.frame(response = d$response, actual = d$actual)
  }
  
  generateLabels <- function(plotData, config) {
    trials <- c()
    for (i in 1:length(unique(plotData$trial))) {
      trials <- c(trials, paste0("Trial ", unique(plotData$trial))[i])
    }
    models <- c()
    for (i in 1:length(unique(plotData$mid))) {
      models <- c(models, paste0("Model ", unique(plotData$model))[i])
    }
    list(trials = trials, models = models)
  }
  
  plotBinaryData <- function(plotData, config, modelNames) {
    labels <- generateLabels(plotData, config)
    modelVec <- modelNames[plotData$mid]
    trialVec <- paste0('Trial ', plotData$trial)
    plotData <- cbind(plotData, modelVec, trialVec)
    liftdf <- data.frame(Rate_positive_predictions = plotData$Rate_Pos_Predictions, lift = plotData$lift, fold = paste0("Fold", plotData$fold), 
                         models = plotData$modelVec, trial = plotData$trialVec)
    gaindf <- data.frame(Rate_positive_predictions = plotData$Rate_Pos_Predictions, True_Pos_Rate = plotData$True_Pos_Rate, fold = paste0("Fold", plotData$fold),
                         models = plotData$modelVec, trial = plotData$trialVec)
    prec_recalldf <- data.frame(recall = plotData$recall, precision = plotData$Precision, fold = paste0("Fold", plotData$fold),
                                models = plotData$modelVec, trial = plotData$trialVec)
    rocdf <- data.frame(False_Pos_Rate = plotData$False_Pos_Rate, True_Pos_Rate = plotData$True_Pos_Rate, fold = paste0("Fold", plotData$fold),
                        models = plotData$modelVec, trial = plotData$trialVec)
    
    liftPlotObj <- ggplot(data = liftdf, aes(x = Rate_positive_predictions, y = lift)) + 
      geom_smooth(aes(colour=models)) + ggtitle("Lift curves") 
    gainPlotObj <- ggplot(data = gaindf, aes(x = Rate_positive_predictions, y = True_Pos_Rate)) + 
      geom_smooth(aes(colour=models)) + ggtitle('Gain Charts')
    PrecRecallPlotObj <- ggplot(data = prec_recalldf, aes(x = recall, y = precision)) + 
      geom_smooth(aes(colour=models)) + ggtitle('Precision and Recall Curves')
    ROCPlotObj <- ggplot(data = rocdf, aes(x = False_Pos_Rate, y = True_Pos_Rate)) +
      geom_smooth(aes(colour=models)) + ggtitle('ROC Curves')
    AlteryxGraph2(liftPlotObj, nOutput = 4)
    AlteryxGraph2(gainPlotObj, nOutput = 4)
    AlteryxGraph2(PrecRecallPlotObj, nOutput = 4)
    AlteryxGraph2(ROCPlotObj, nOutput = 4)
  }
  
  plotRegressionData <- function(plotData, config, modelNames) {
    modelVec <- modelNames[plotData$mid]
    trialVec <- paste0('Trial ', plotData$trial)
    plotData <- cbind(plotData, modelVec, trialVec)
    plotdf <- data.frame(Actual = plotData$actual, Predicted = plotData$response, fold = paste0("Fold", plotData$fold), 
                         models = plotData$modelVec, trial = plotData$trialVec)
    plotObj <- ggplot(data = plotdf, aes(x = Actual, y = Predicted)) + 
      geom_smooth(aes(colour=models)) + ggtitle("Predicted value vs actual values")
    AlteryxGraph2(plotObj, nOutput = 4)
  }
  
  # Helper Functions End ----
  getResultsCrossValidation <- function(inputs, config){
    inputs$data$recordID <- 1:NROW(inputs$data)
    yVarList <- getYvars(inputs$data, inputs$models)
    yVar <- yVarList$y_col
    y_name <- yVarList$y_name
    inputs$modelNames <- names(inputs$models)
    modelNames <- names(inputs$models)
    checkXVars(inputs)
    extras <- list(
      yVar = yVar,
      y_name = y_name,
      posClass = config$posClass,
      allFolds = createFolds(data = inputs$data, config = config),
      levels = NULL
    )
    dataOutput1 <- generateOutput1(inputs, config, extras)
    if ("Score" %in% colnames(dataOutput1)) {
      dataOutput1 <- data.frame(trial = dataOutput1$trial, fold = dataOutput1$fold, 
                                mid = dataOutput1$mid, recordID = dataOutput1$recordID,
                                response = dataOutput1$Score, actual = dataOutput1$actual)
    }
    preppedOutput1 <- data.frame(RecordID = dataOutput1$recordID, 
                                 Trial = dataOutput1$trial, Fold = dataOutput1$fold, 
                                 Model = modelNames[dataOutput1$mid], Response = dataOutput1$response, 
                                 Actual = dataOutput1$actual)
    
    dataOutput2 <- generateOutput2(dataOutput1, extras, modelNames)
    preppedOutput2 <- reshape2::melt(dataOutput2, id = c('trial', 'fold', 'Model'))
    plotData <- ddply(dataOutput1, .(trial, fold, mid), generateDataForPlots, 
                      extras = extras, config = config
    )
    outputPlot <- plotRegressionData(plotData, config, modelNames)
    list(
      data = preppedOutput1, fitMeasures = preppedOutput2, 
      outputPlot = outputPlot   
    )
  }
  
  runCrossValidation <- function(inputs, config){
    results <- getResultsCrossValidation(inputs, config)
    write.Alteryx2(results$data, 2)
    write.Alteryx2(results$fitMeasures, 3)
    AlteryxGraph2(results$outputPlot, 4)
  }
  
  if (is.null(getOption("testscript"))){
    runCrossValidation(inputs, config)
  }
  
}