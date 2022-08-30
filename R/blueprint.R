#' Creates a character matrix which specifies which effects to estimate and
#' which effects to constrain to a non-zero value
#'
#' @param effects  A data frame that contains information on which cross-lagged effects to
#'                estimate or constrain to a value other than zero. Each row
#'                represents one effect. The `effects` object  must have four
#'                columns: column 1 has the variable names for the predictors,
#'                column 2 has the variable names for the outcomes, column
#'                3 has the names of the estimated effect or the value an unestimated
#'                effect should be constrained to, and column 4 has information
#'                on whether the effect should be estimated. This table can be
#'                created by using the `effectTable` function
#'
#' @param use     A vector with the variable names that will be used in the
#'                stability-informed model
#'
#' @return        A character matrix
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' effects <- data.frame(predictor = "X_0", outcome = "Y", name = "CLxy", estimate = "Yes")
#' use <- c("X", "Y")
#' blueprint(effects, use)
#'}
#'
blueprint <- function(effects, use){

  p <- length(use)

  blueprint <- matrix(0, nrow = p, ncol = p)

  # Rows are the predictors; The phantom Time 0 variables
  # Columns are the outcomes; The measured Time 1 variables
  colnames(blueprint) <-  use
  rownames(blueprint) <- paste0(colnames(blueprint), "_0")

  # AR effects on the diagonal
  diag(blueprint) <- paste0("AR", use)

  # Input CL effects along with information about whether they should be estimated
  #   or constrained to 0 or some other value

  for( i in 1:nrow(effects) ){

    blueprint[effects[i, "predictor"], effects[i, "outcome"]] <- effects[i, "name"]

  }

  return(blueprint)

}
