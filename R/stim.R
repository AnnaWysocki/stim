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
#' dat <- data.frame(Y = rnorm(500, 0, 1), X = rnorm(500, 0, 1))
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

    stopifnot("input for `data` argument must be a dataframe " = is.data.frame(data))

    stopifnot("input for `data` argument must have column names" = !is.null(colnames(data)))

  }

  # Checks for S input
  if( !is.null(S)){

    stopifnot("input for `S` argument must be a matrix" = is.matrix(S))

    stopifnot("input for `S` argument must be a symmetric covariance matrix" = isSymmetric(S))

  }

  # Checks for n input
  if( !is.null(n)){

    stopifnot("input for `n` argument must be a vector" = is.vector(n))

    if(length(n) != 1){

      warning(paste0("input for `n` argument has length of ", length(n),
                    ". Only the first element will be used."))

      n <- n[1]
    }


    stopifnot("`n` must be numeric" = is.numeric(n))

  }


  # Checks for model input
  stopifnot("`model` must be a character element " = is.character(model))

  modelList <- list(model = model)


  # Create parameter tables with information about which cross-lag effects and
  # which residual covariances should be estimated

  modelList <- c(modelList, effectTable(modelList$model))

  effects <- modelList$CLEffectTable


  # Create list of variables that will be used in the STIM model
  use <- unique(c(effects$predictor, effects$outcome))


  modelList$p <- length(use)

  if( !is.null(data)){

    stopifnot("`data` must be a dataframe " = is.data.frame(data))

    n <- nrow(data)


    # Check that variables in model input and data input match
    if( any(is.na(match(use, colnames(data)))) == TRUE ) {

      stop("Variable labels in the model input do not match the
          column names in the data input")
    }

    data <- data[ , use]
    S <- stats::cov(data, use = "pairwise.complete.obs")

  }

  if( !is.null(S)){

    # Check that variables in model input and S input match
    if( any(is.na(match(use, colnames(S)))) == TRUE ) {

      stop("Variable labels in the model input do not match the
          column names in the S input")
    }

  }



  #use <- use[order(match(use, colnames(S)))]
  modelList$S <- S
  modelList$n <- n




  # Checks for stability input

  if(is.matrix(stability)){

    stability <- as.data.frame(stability)
  }

  if(is.vector(stability)){

    stability <- as.data.frame(t(stability))

  }

  stopifnot('Elements in `stability` input must be numeric' = apply(stability, 1, is.numeric))

  if(ncol(stability) != length(use)){

    stop("Provide a stability value for each variable in the model")

  }


  if(is.null(names(stability))){
    stop("The `stability` input must be named")
  }

  if( any(is.na(match(names(stability), use))) == TRUE ) {

    stop("The `stability` input names don't match the variable names")

  }

  # put stability values in the same order as the data input columns

  stability <- stability[use]

  modelList$stability <- stability

  modelList$modelsEstimated <- nrow(stability)


  ################################
  ##  Check Degrees of Freedom  ##
  ################################

  p <- length(use)
  pstar <- (p * (p-1)) /2

  modelList$q <- sum(effects$estimate == "Yes", modelList$ResidualCovariance$Variables$estimate == "Yes")

  if (modelList$q > pstar ) stop(paste0("stim can estimate ", pstar, " parameters. `model` input specifies that ", modelList$q,
                                     " parameters should be estimated."))
  modelList$df <- pstar - modelList$q

  #################
  ##  Fit Model  ##
  #################

  modelList$CLEffectTable$predictor <- paste0(modelList$CLEffectTable$predictor, "_0")

  ModelResults <- list()
  modelImpliedEquations <- list()
  SIMSyntax <- list()

  # Create a blueprint/ symbolic beta matrix.
  # This blueprint matrix will be used to specify which cross-lagged effects
  # should be estimated and which should be constrained

  modelList$blueprint <- blueprint(modelList$CLEffectTable, use)


  modelList$modelWarning <- rep(0, nrow(modelList$stability))

  # Users can specify multiple stability conditions.
  # The stim function will estimate the STIM model for each
  # stability condition

  for(i in 1: nrow(modelList$stability)){

    stabilityIndex <- modelList$stability[i, ]

    # The modelImpliedEq() function returns model implied equations for the
    # autoregressive effects and the phantom variable covariances.
    # these equations are needed to fit a STIM model

    modelImpliedEquations[[i]] <- modelImpliedEq(S = S,
                                  blueprint = modelList$blueprint,
                                  stability = stabilityIndex,
                                  residualcov = modelList$ResidualCovariance)$modelImpliedEquations

    # The lavaanEq() function returns the lavaan syntax for the STIM model
    LavaanSyntax <- lavaanEq(blueprint = modelList$blueprint,
                             S = S)

    if( !is.null(modelList$ResidualCovariance$Syntax) ){

      LavaanSyntax <- c(LavaanSyntax, modelList$ResidualCovariance$Syntax)

    }


    SIMSyntax[[i]] <- c(LavaanSyntax, modelImpliedEquations[[i]])


    ModelResults[[i]] <- try(lavaan::sem(SIMSyntax[[i]], sample.cov = S, sample.nobs= n,
                                         std.lv = TRUE), silent = TRUE)

    modelList$modelWarning[i] <- lavaan::inspect( ModelResults[[i]], what = "post.check")

  }

  modelList$lavaanObjects <- ModelResults
  modelList$SIMSyntax <- SIMSyntax
  modelList$modelImpliedEquations <- modelImpliedEquations

  Results <- resultTable(modelList)

  out <- list(n = modelList$n,
              p = modelList$p,
              q = modelList$q,
              df = modelList$df,
              stability = modelList$stability,
              CLEffectTable = modelList$CLEffectTable,
              CLMatrices = Results$CLMatrixList,
              RCovMatrices = Results$RCovMatrixList,
              ARVector = Results$ARVectorList,
              lavaanObjects = modelList$lavaanObjects,
              NoWarnings = as.logical(modelList$modelWarning),
              CSModelSyntax = modelList$model,
              SIMSyntax = modelList$SIMSyntax,
              modelImpliedEquations = modelList$modelImpliedEquations)


  class(out) = "stim"

  print.stim(out)

  return(out)

}



#' @title Summary method for \code{stim} objects
#'
#' @name summary.stim
#'
#' @description Summarize a set of Stability Informed Models
#'
#' @param object An object of class \code{stim}
#'
#' @param ... Not used
#' @seealso \code{\link{stim}}
#'
#' @return A print out containing the results for a set of Stability Informed Models
#'
#' @examples
#' \donttest{
#' model <- 'Y~X'
#' stability <- data.frame(X = .3, Y = .3)
#' dat <- data.frame(Y = rnorm(500, 0, 1), X = rnorm(500, 0, 1))
#'
#' modelFit <- stim(data = dat, model = model, stability = stability)
#'
#' summary(modelFit)
#'}
#'
#' @export
summary.stim <- function(object, ...){


  cat("StIM: Stability Informed Models \n")

  cat("-------------------------------------\n")
  cat("-------------------------------------\n")

  cat("\n")
  cat("Variables (p):", object$p, "\n")
  cat("Sample Size (n):", object$n, "\n")
  cat("Estimated Parameters (q):", object$q, "\n")
  cat("Degrees of Freedom:", object$df, "\n")

  cat("\n")

  cat("-------------------------------------\n")


  for(i in 1:nrow(object$stability)){

    cat("Model", i, "\n")
    cat("\n")

    if(object$NoWarnings[i] == FALSE){
      cat("Model" ,i, "produced an error or warning! \n")
    }

    cat("Stability:\n")
    print(object$stability[i, ], row.names =  FALSE)

    cat("\n Autoregressive Effects:\n")
    print(object$ARVector[[i]], row.names =  FALSE)

    cat("\n Cross Lagged Effects:\n")
    print(object$CLMatrices[[i]], row.names =  FALSE)

    if(nrow(object$RCovMatrices[[1]]) != 0){

    cat("\n Residual Covariances:\n")
    print(object$RCovMatrices[[i]], row.names =  FALSE)
    }

    cat("\n-------------------------------------\n ")

  }



}


#' stim print function
#'
#' @param out A stim object

#'
#' @return Overview of model
#' @keywords internal

print.stim <- function(out){

  cat("StIM: Stability Informed Models \n")

  cat("-------------------------------------\n")
  cat("-------------------------------------\n")

  cat("\n")
  cat("Variables (p):", out$p, "\n")
  cat("Sample Size (n):", out$n, "\n")
  cat("Estimated Parameters (q):", out$q, "\n")
  cat("Degrees of Freedom:", out$df, "\n")

  cat("Number of Models Estimated:", nrow(out$stability), "\n")

  if(any(out$NoWarnings) == FALSE){
    cat("Model(s)", which(out$NoWarnings == FALSE), "produced an error or warning! \n")
  }

  cat("\n")

  cat("-------------------------------------\n")

}
