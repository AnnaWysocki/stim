#' Internal function used to do symbolic matrix multiplication using the Ryacas
#' R package
#'
#' @importFrom Ryacas %*%
#' @name %*%
#' @rdname SymbolicMultiplication
#' @keywords internal
#' @param x1 A Ryacas object.
#' @param x2 A Ryacas object.

symbMultiplication <- function(x1, x2) {
  x1 %*% x2
}

#' Create a symbolic covariance matrix
#'
#' @param blueprint A character matrix which specifies which effects to estimate and
#' which effects to constrain to a non-zero value
#'
#' @param residualcov A list with both the lavaan syntax for the residual covariance
#'                    and a dataframe with the variable names
#'
#' @return A list of character matrices: A symbolic covariance matrix and a symbolic
#' psi matrix
#'
#' @keywords internal
#'

symbMatrix <- function(blueprint, residualcov){

  p <- ncol(blueprint)

  StCovMat <- Psi <-  matrix(rep(0, p^2),
                             nrow = p,
                             ncol = p)

  colnames(StCovMat) <- rownames(StCovMat) <- colnames(Psi) <- rownames(Psi) <- colnames(blueprint)


  for(i in 1:ncol(StCovMat)){

    for(j in 1:nrow(StCovMat)){

      if(colnames(StCovMat)[i] == rownames(StCovMat)[j]){
        StCovMat[j, i] <- paste0("Var", colnames(StCovMat)[i])

      } else {

        StCovMat[j, i] <- paste0("Cov", colnames(StCovMat)[i], rownames(StCovMat)[j])

      }}}

  diag(Psi) <- paste0("RVar", colnames(StCovMat))

  if( !is.null(residualcov$Syntax) ){
    PsiTerms <- apply(residualcov$Variables, 1, function(x){

      Psi[x[1], x[2]] <- x[3]
      Psi[x[2], x[1]] <- x[3]
      Psi
    })

    Psi <- matrix(PsiTerms, nrow = p, ncol = p)
  }

  StCovMat[upper.tri(StCovMat)] <- t(StCovMat)[upper.tri(StCovMat)]


  return(list(SymbCov = StCovMat, Psi = Psi))
}
