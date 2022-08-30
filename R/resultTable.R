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
                                       ncol = modelList$q + modelList$p * 2))
  for(i in 1:modelList$modelsEstimated){

    lavaanLambda <- lavaan::inspect(modelList$lavaanObjects[[i]], what = "std")$lambda
    AReffects <- diag(lavaanLambda)

    CLeffects <- CLName <- rep(0, nrow(estimatedEffects))

    lavaanTable <- lavaan::parameterestimates(modelList$lavaanObjects[[i]])

    for(j in 1:nrow(estimatedEffects)) {

      CLeffects[j] <- lavaanTable[which(lavaanTable$label == estimatedEffects$name[j]), "est"]
      CLName[j] <- estimatedEffects$name[j]
    }

    if(!is.null(modelList$ResidualCovariance$Syntax)){

      Rcov <- RcovName <- length(modelList$ResidualCovariance$Syntax)

      for(j in 1: length(Rcov)) {

        RcovName <- modelList$ResidualCovariance$Variables$name[j]
        Rcov[j] <- lavaanTable[which(lavaanTable$label == RcovName), "est"]

      }

      ResultMatrix[i, ] <- unlist(c(modelList$stability[i, ], AReffects, CLeffects,
                                    Rcov))


      colnames(ResultMatrix) <- c(paste0("Stability", colnames(modelList$stability)),
                                  paste0("AR", rownames(lavaanLambda)),
                                  CLName,
                                  RcovName)

    }else{
      ResultMatrix[i, ] <- unlist(c(modelList$stability[i, ], AReffects, CLeffects))


      colnames(ResultMatrix) <- c(paste0("Stability", colnames(modelList$stability)),
                                  paste0("AR", rownames(lavaanLambda)),
                                  CLName)


    }

  }

  ResultMatrix <- data.frame(Model = paste0("Model ", 1:nrow(ResultMatrix)), ResultMatrix)

  return(ResultMatrix)
}
