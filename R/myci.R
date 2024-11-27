#' myci
#'
#' @param x numeric vector
#'
#' @return confidence interval
#' @examples
#' set.seed(23);x = rnorm(30,mean=10,sd=12)
#' MATH4753FALLfaug::myci(x)
#'
#' @export
#'
myci = function(x) {
  t = qt(1 - 0.05/2, length(x) - 1)
  mean(x) + c(-1, 1) * t * sd(x) / sqrt(length(x))
}
