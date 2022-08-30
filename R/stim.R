#' Estimate a Stability Informed Model
#'
#' @param data A dataframe with the measured variables. Not needed if S is provided
#' @param S A covariance matrix for the measured variables. Not needed if data is provided.
#' @param n Number of observations. Not needed if data is provided.
#' @param model An object with the cross-sectional model description in lavaan syntax
#' @param stability An object that contains stability information for each
#'                  variable in the model.
#'
#' @return An object of class stim
#' @export
#'
#' @examples
#'
#' model <- 'Y~X'
#' stability <- data.frame(X = .3, Y = .3)
#' dat <- data.frame(Y = rnorm(500, 0, 1), X = rnorm(500, 0, 1), Z = rnorm(500, 0, 1))
#'
#' stim(data = dat, model = model, stability = stability)


stim <- function(data = NULL, S = NULL, n = NULL,
                model, stability){

  ####################
  ##  Check inputs  ##
  ####################


  # Checks for data/covariance input

  if( !is.null(S) & is.null(n) ) stop("Sample size must be provided if a covariance matrix is used as the data input.")

  if( is.null(S) & is.null(data) ) stop("Input needed for either `dat` or `S` argument.")


  # Checks for data input
  if( !is.null(data)){

    stopifnot("`data` must be a dataframe " = is.data.frame(data))

    n <- nrow(data)
    S <- stats::cov(data)

  }

  # Checks for S input
  if( !is.null(S)){

    stopifnot("`S` must be a matrix" = is.matrix(S))

    stopifnot("`S` must be symmetric" = isSymmetric(S))

  }

  # Checks for n input
  if( !is.null(n)){

    stopifnot("`n` must be numeric" = is.numeric(n))

  }

  modelList <- list(model = model,
                    S = S,
                    n = n)

  # Checks for model input
  stopifnot("`model` must be a character element " = is.character(modelList$model))

  modelList <- c(modelList, effectTable(modelList$model))

  effects <- modelList$CLEffectTable


  # Create list of variables to use
  use <- unique(c(effects$predictor, effects$outcome))
  use <- use[order(match(use, colnames(S)))]

  modelList$p <- length(use)

  if( any(is.na(match(use, colnames(S)))) == TRUE ) {

    stop("Variable names in the effects object do not match the variable
          names in the dataset/covariance matrix")

  }


  # Checks for stability input

  if(is.matrix(stability)){

    stability <- as.data.frame(stability)
  }

  if(is.vector(stability)){

    stability <- as.data.frame(t(stability))

  }

  if(ncol(stability) != length(use)){

    stop("Provide a stability value for each variable in the model")

  }

  if(is.null(names(stability))){
    stop("The `stability` input must be named")
  }

  if( any(is.na(match(names(stability), use))) == TRUE ) {

    stop("The `stability` input names don't match the variable names")

  }

  modelList$stability <- stability

  modelList$modelsEstimated <- nrow(stability)

  ################################
  ##  Check Degrees of Freedom  ##
  ################################

  p <- length(use)
  df <- (p * (p-1)) /2

  modelList$q <- sum(effects$estimate == "Yes", nrow(modelList$ResidualCovariance$Variables))

  if (modelList$q > df ) stop("The number of specified parameters to estimate are greater than the degrees of freedom.")

  #################
  ##  Run Model  ##
  #################

  modelList$CLEffectTable$predictor <- paste0(modelList$CLEffectTable$predictor, "_0")

  ModelResults <- list()

  modelList$blueprint <- blueprint(modelList$CLEffectTable, use)

  modelList$modelWarning <- rep(0, nrow(modelList$stability))

  for(i in 1: nrow(modelList$stability)){

    stabilityIndex <- modelList$stability[i, ]

    modelList <- c(modelList,
                   modelImpliedEq(S = S,
                                  blueprint = modelList$blueprint,
                                  stability = stabilityIndex,
                                  residualcov = modelList$ResidualCovariance))

    LavaanSyntax <- lavaanEq(blueprint = modelList$blueprint,
                             S = S)

    if( !is.null(modelList$ResidualCovariance$Syntax) ){

      LavaanSyntax <- c(LavaanSyntax, modelList$ResidualCovariance$Syntax)

    }

    modelList$SIMSyntax <- c(LavaanSyntax, modelList$modelImpliedEquations)


    ModelResults[[i]] <- try(lavaan::sem(modelList$SIMSyntax, sample.cov = S, sample.nobs= n,
                                         std.lv = TRUE), silent = TRUE)

    modelList$modelWarning[i] <- lavaan::inspect( ModelResults[[i]], what = "post.check")

  }

  modelList$lavaanObjects <- ModelResults

  ResultMatrix <- resultTable(modelList)

  out <- list(stability = modelList$stability,
              CLEffectTable = modelList$CLEffectTable,
              ResultMatrix = ResultMatrix,
              lavaanObjects = modelList$lavaanObjects,
              NoWarnings = as.logical(modelList$modelWarning),
              CSModelSyntax = modelList$model,
              SIMSyntax = modelList$SIMSyntax,
              modelImpliedEquations = modelList$modelImpliedEquations,
              SymbolicMatrices = modelList$SymbolicMatrices)


  class(out) = "stim"

  print(out)

  return(out)

}
