#' Create a parameter table
#'
#' @param model An object with the model description for the cross-sectional model in lavaan syntax
#'
#' @return A data frame with information on which effects to estimate and which to constrain. Each
#'        row represents one effect and specifies which variable is the predictor and outcome of the effect.
#'        The name column contains information on whether the effect should be estimated--if the name is a
#'        character (e.g., CLxy)--or if the effect should be constrained to a value other than 0 (e.g., .3)
#'
#' @export
#'
#' @examples
#'
#' #estimate effect from X to Y
#' #constrain effect from Y to X to .3
#' model <- c('Y ~  X
#'             X ~ .3 * Y')
#'
#' effectTable(model)


effectTable <- function(model){

  FullEffectTable <- lavaan::lavaanify(model)
  ClEffectTable <- FullEffectTable[FullEffectTable$op ==  "~", ]

  effects <- data.frame(predictor = 0, outcome = 0, name = 0, estimate = 0)

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
