#' Create lavaan syntax based on the blueprint matrix
#'
#' @param blueprint A character matrix which specifies which effects to estimate and
#'                  which effects to constrain to a non-zero value
#' @param S Sample covariance matrix
#'
#' @return A character vector which contains the lavaan syntax to specify the latent variables, variances, and covariances
#' @keywords internal
#'

lavaanEq <- function(blueprint, S){

  outcomes <- colnames(blueprint)

  LatentEq <- NULL
  VarEq <- NULL
  CovEq <- NULL

  # Create lavaan syntax to specify the phantom variables and their associated
  # autoregressive and cross-lagged effects, and to specify and label
  # variances and covariances

  for(i in 1: nrow(blueprint)){

    predictor <- rownames(blueprint)[i]

    for(j in 1:ncol(blueprint)){

      if(j == 1){

        eq <- paste0(predictor, "=~", blueprint[predictor, j], "*", outcomes[j])

      }else{

        eq <- paste0(eq, "+", blueprint[predictor, j], "*", outcomes[j])

      }

      if( i < j & j <= ncol(blueprint) ){

        CovEq <- c(CovEq , paste0(predictor, '~~ Cov', colnames(blueprint)[i], colnames(blueprint)[j], '*',
                                  rownames(blueprint)[j]))
      }


    } # j loop ends

    LatentEq <- c(LatentEq, eq)

    VarEq <- c(VarEq, paste0(predictor, '~~', 'Var',colnames(blueprint)[i], "*", predictor))

  } # i loop ends


  return( c(LatentEq, CovEq, VarEq) )
}



