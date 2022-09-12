#' Create a parameter table
#'
#' @param model An object with the model description for the cross-sectional model in lavaan syntax
#'
#' @return A list with information on the cross-lagged paths and the residual covariances. The cross-lagged effect table has
#'         information on which cross-lagged effects to estimate and which to constrain. Each
#'         row represents one effect and specifies which variable is the predictor and outcome of the effect.
#'         The name column contains information on either the name of the estimated effect (e.g., CLxy)
#'         or what value the unestimated effect should be constrained to (e.g., .3).
#'         The residual covariance list has the lavaan syntax to specify that specific residuals should be allowed to
#'         covary, and a table with information on which variables should have covarying residuals and what the name of
#'         that residual covariance parameter should be.
#'
#' @export
#'
#' @examples
#'
#' #estimate effect from X to Y
#' #constrain effect from Y to X to .3
#' #allow X and Y's residuals to covary
#' model <- c('Y ~  X
#'             X ~ .3 * Y
#'             X ~~ Y')
#'
#' effectTable(model)


effectTable <- function(model){

  # Get a data frame with all the cross-lagged effects
  FullEffectTable <- lavaan::lavaanify(model)
  ClEffectTable <- FullEffectTable[FullEffectTable$op ==  "~", ]

  effects <- data.frame(predictor = 0, outcome = 0, name = 0, estimate = 0)

  # Create an easier to interpret table with the same information as the
  # effects object

  for(i in 1:nrow(ClEffectTable)){

    effects[i , c("predictor", "outcome") ] <- ClEffectTable[i, c("rhs", "lhs")]

    if( is.na(ClEffectTable[i, "ustart"]) ){

      if( ClEffectTable[i, "label"] == "" ){

        effects[i, "name"] <- paste0("CL", effects[i, "predictor"],
                                     effects[i, "outcome"])
      }else{

        effects[i, "name"] <- ClEffectTable[i, "label"]
      }

      effects[i, "estimate"] <- "Yes"

    }else{

      effects[i, "name"] <- ClEffectTable[i, "ustart"]

      effects[i, "estimate"] <- "No"
    }
  } # for loop ends

  # Create a two objects. One with lavaan syntax to specify residual covariances
  # and the other with a data frame (similar to the effects object) with information
  # about the residual covariances

  ResidualCovariance <- FullEffectTable[FullEffectTable$op == "~~" &
                                          FullEffectTable$lhs != FullEffectTable$rhs,]

  ResidualCovarianceSyntax <- rep(0, nrow(ResidualCovariance))
  ResidualCovarianceDF <- data.frame(V1 = rep(0, nrow(ResidualCovariance)),
                                     V2 = rep(0, nrow(ResidualCovariance)),
                                     name = rep(0, nrow(ResidualCovariance)))

  if(nrow(ResidualCovariance) != 0){

    for(i in 1: nrow(ResidualCovariance)){

      CovLabel <- paste0("RCov", ResidualCovariance$lhs[i], ResidualCovariance$rhs[i])

      ResidualCovarianceSyntax[i] <- paste0(ResidualCovariance$lhs[i], "~~", CovLabel, "*", ResidualCovariance$rhs[i])
      ResidualCovarianceDF[i, ] <- c(ResidualCovariance$lhs[i], ResidualCovariance$rhs[i], CovLabel)

    }

  }else{

    ResidualCovarianceSyntax <- NULL
    ResidualCovarianceDF <- NULL

  }

  ResidualCovariance <- list(Syntax = ResidualCovarianceSyntax,
                             Variables = ResidualCovarianceDF )


  return(list(CLEffectTable = effects, ResidualCovariance = ResidualCovariance))
}
