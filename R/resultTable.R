#' Creating a Result Table from a stim object
#'
#' @param modelList A list of SIModel inputs and outputs
#'
#' @return A result table
#' @keywords internal
#'


resultTable <- function(modelList){

  estimatedEffects <- modelList$CLEffectTable[modelList$CLEffectTable$estimate == "Yes", ]

  ResultMatrix <- as.data.frame(matrix(0,
                                       nrow = modelList$modelsEstimated,
                                       ncol = modelList$q * 3 + modelList$p * 2))

  for(i in 1:modelList$modelsEstimated){

    lavaanLambda <- lavaan::inspect(modelList$lavaanObjects[[i]], what = "std")$lambda

    AReffects <- as.data.frame(t(diag(lavaanLambda)))

    ModelResults <- cbind(modelList$stability[i,], AReffects)

    lavaanTable <- lavaan::parameterestimates(modelList$lavaanObjects[[i]])

    CLName <- rep(0, nrow(estimatedEffects))
    CLTable <- NULL

    if(nrow(estimatedEffects) != 0 ){

    for(j in 1:nrow(estimatedEffects)) {

      CLeffects <- lavaanTable[which(lavaanTable$label == estimatedEffects$name[j]), c("est",
                                                                                      "se", "pvalue")]
      CLName[j] <-  estimatedEffects$name[j]


      CLTable <- unlist(c(CLTable, CLeffects))

    }

      ModelResults <- cbind(ModelResults, t(CLTable))

    }

    if(!is.null(modelList$ResidualCovariance$Syntax)){

      RcovTable <- NULL
      RcovName <- rep(0, length(modelList$ResidualCovariance$Syntax))

      for(j in 1: length(RcovName)) {

        RcovName[j] <- modelList$ResidualCovariance$Variables$name[j]
        Rcov <- lavaanTable[which(lavaanTable$label == RcovName), c("est",
                                                                       "se", "pvalue")]

        RcovTable <- unlist(c(RcovTable, Rcov))

        }

      ModelResults <- cbind(ModelResults, t(RcovTable))
      }


    ResultMatrix[i, ] <- ModelResults
  }


  ResultLabels <- c(paste0("Stability", colnames(modelList$stability)),
                    paste0("AR", colnames(modelList$stability)))


  if( !is.null(CLTable) ){

    ResultLabels <- c(ResultLabels,
                      as.vector(apply(as.data.frame(CLName), 1,
                                      function(x){paste0(x, c("", ".SE", ".Pvalue"))})))

  }

  if( !is.null(modelList$ResidualCovariance$Syntax) ){

  ResultLabels <- c(ResultLabels,
                    as.vector(apply(as.data.frame(RcovName), 1,
                                  function(x){paste0(x, c("", ".SE", ".Pvalue"))})))

  }


  colnames(ResultMatrix) <- ResultLabels
  ResultMatrix <- data.frame(Model = paste0("Model ", 1:nrow(ResultMatrix)), ResultMatrix)

  return(ResultMatrix)
}
