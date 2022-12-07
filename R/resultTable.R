#' Creating a Result Table from a stim object
#'
#' @param modelList A list of SIModel inputs and outputs
#'
#' @return A result table
#' @keywords internal
#'


resultTable <- function(modelList){

  CLMatrixList <- NULL
  RCovMatrixList <- NULL
  ARVectorList <- NULL


  for(i in 1:modelList$modelsEstimated){

    lavaanLambda <- lavaan::inspect(modelList$lavaanObjects[[i]], what = "std")$lambda
    lavaanTable <- lavaan::parameterestimates(modelList$lavaanObjects[[i]])


    # Create AR Effect Vector
    AReffects <- diag(lavaanLambda)
    names(AReffects) <- paste0("AR", rownames(lavaanLambda))

    # Create CL Effect Matrix

    CLMatrix <- lavaanTable[is.element(lavaanTable$label, modelList$CLEffectTable$name), c("label","est", "se", "pvalue")]


    # Create Residual Covariance Matrix

    RCovMatrix <- lavaanTable[is.element(lavaanTable$label, modelList$ResidualCovariance$Variables$name), c("label","est", "se", "pvalue")]


    colnames(CLMatrix) <- colnames(RCovMatrix) <- c("Effect", "Estimate", "Standard.Error", "P.Value")

    CLMatrixList[[i]] <- CLMatrix
    RCovMatrixList[[i]] <- RCovMatrix
    ARVectorList[[i]] <- AReffects
  }

  names(CLMatrixList) <- names(RCovMatrixList) <- names(ARVectorList) <- paste0("Model", 1:modelList$modelsEstimated)


  return(list(CLMatrixList = CLMatrixList, RCovMatrixList = RCovMatrixList, ARVectorList = ARVectorList))
}





