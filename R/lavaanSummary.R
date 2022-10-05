#' Outputs Lavaan Summary
#'
#' @param x a SIModel Object
#' @param subset Specify which model(s) you would like summarized. Default is to output all estimated models
#'
#' @return Lavaan summary table
#' @export
#'
#' @examples
#' model <- 'Y~X'
#' stability <- data.frame(X = c(.3, .4, .5), Y = c(.3, .5, .6))
#' dat <- data.frame(Y = rnorm(500, 0, 1), X = rnorm(500, 0, 1), Z = rnorm(500, 0, 1))
#'
#' output <- stim(data = dat, model = model, stability = stability)
#'
#' lavaanSummary(output, subset = c(1,2))

lavaanSummary <- function(x, subset = NULL){

  if(!inherits(x, "stim")){
    stop("`x` input must be a stim object")
  }

  if(is.null(subset)){
    for(i in 1:length(x$lavaanObjects)){
      lavaan::summary(x$lavaanObjects[[i]])
    }}else{

      for(i in 1:length(subset)){

        lavaan::summary(x$lavaanObjects[[subset[i]]])


      }

    }}




