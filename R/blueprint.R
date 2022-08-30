#' Creates a character matrix which specifies which effects to estimate and
#' which effects to constrain to a non-zero value
#'
#' @param effects  A data frame that contains information on which effects to
#'                estimate or constrain to a value other than zero. Each row
#'                represents one effect. The `effects` object  must have three
#'                columns: column 1 has the variable names for the predictors,
#'                column 2 has the variable names for the outcomes, and column
#'                3 has the names of the effects (if the effect should be estimated)
#'                or the numeric value the effect should be set at (if the effect
#'                is not to be estimated but rather to be set to a value other
#'                than zero).
#' @param use     A vector with the variable names that will be used in the
#'                stability-informed model
#'
#' @return        A character matrix
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' effects <- data.frame(predictor = "X_0", outcome = "Y", name = "CLxy")
#' use <- c("X", "Y")
#' CreateBlueprint(effects, use)
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
