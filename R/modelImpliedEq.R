#' Get the model implied symbolic equations for the auto-regressive effects and
#' the covariances between the phantom variables
#'
#' @param S Sample covariance matrix
#' @param blueprint A character matrix which specifies which effects to estimate and
#'                  which effects to constrain to a non-zero value
#' @param stability A data frame that contains stability information for each
#'                  variable in the model. If unnamed SIM will assume the stability
#'                  values are in the same order as the provided data set/
#'                  covariance matrix.
#' @param residualcov A list with both the lavaan syntax for the residual covariance
#'                    and a dataframe with the variable names
#'
#' @return A character vector with the model implied equations for the autoregressive
#'         effects and the phantom covariances.
#' @keywords internal
#'
#' @examples
#'\dontrun{
#' S <- matrix( c(1, .3, .3, 1), nrow = 2, ncol = 2)
#' colnames(S) <- rownames(S) <- c("X", "Y")
#'
#' blueprint <- matrix( c("ARx", "CLxy", "ARy", "0"), nrow = 2, ncol = 2 )
#' colnames(blueprint) <- c("X", "Y")
#' rownames(blueprint) <- c("X_0", "Y_0")
#'
#'  stability <- data.frame(X = .3, Y = .3)
#'
#'  residualcov <- list(Syntax = 'X ~~ RCovXY * Y',
#'                      Variables = data.frame(V1 = "X", V2 = "Y", Name = "RCovXY"))
#'
#'  modelImpliedEq(S, blueprint, stability, residualcov)
#'}
modelImpliedEq <- function(S, blueprint, stability, residualcov){

  SymbolicMats <- symbMatrix(blueprint, residualcov)

  SymbolicCovMat <- SymbolicMats$SymbCov

  Psi <- Ryacas::ysym(SymbolicMats$Psi)
  B <- Ryacas::ysym(blueprint)
  Cov1 <- Ryacas::ysym(SymbolicCovMat)
  Cov2 <- symbMultiplication(t(B), Cov1)



  StNames <- apply(as.matrix(colnames(blueprint)), 1, function(x){paste0("Cov", x, "0", x, "1")})

  ArEquations <- rep(0, length(StNames))

  for(i in 1:length(StNames)){

    StEquations <- paste0(StNames[i], "==", Ryacas::diag(Cov2)[i])
    ArEquations[i] <- as.character(solve(Ryacas::ysym(StEquations), Ryacas::diag(blueprint)[i]))

    ArEquations[i] <- gsub(StNames[i], stability[, i], ArEquations[i])

  }


  ArEquations <- gsub('{', "", ArEquations, fixed = TRUE)
  ArEquations <- gsub('}', "", ArEquations, fixed = TRUE)

  # Covariance Equations

  Cov3 <- symbMultiplication(Cov2, B) + Psi

  Covariances <- SymbolicCovMat[upper.tri(SymbolicCovMat)]

  for( i in 1:length(Covariances) ){

    location <- which(SymbolicCovMat == Covariances[i], arr.ind = TRUE)[1,]
    ArEquations <- c(ArEquations, paste0(Covariances[i], '==',
                                         as.character(Cov3[location[1], location[2]])))

  }

  return(list(modelImpliedEquations = ArEquations, SymbolicMatrices = SymbolicMats))
}
