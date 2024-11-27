#' mycltp
#'
#' @param n sample size
#' @param iter number of iterations
#' @param lambda rate parameter for poisson distribution
#' @param ... additional arguments
#'
#' @return a sophisticated graph
#' @examples
#' mycltp(n = 5, iter = 10000)
#'
#' @export
#'
mycltp = function(n, iter, lambda = 10, ...){
  y = rpois(n * iter, lambda = lambda)
  data = matrix(y, nr = n, nc = iter, byrow = TRUE)
  w = apply(data, 2, mean)
  param = hist(w, plot = FALSE)

  ymax = max(param$density)
  ymax = 1.1 * ymax

  layout(matrix(c(1, 1, 2, 3), nr = 2, nc = 2, byrow = TRUE))

  hist(w, freq = FALSE, ylim = c(0, ymax), col = rainbow(max(w)), main = paste("Histogram of sample mean", "\n", "sample size = ", n, " iter = ", iter, " lambda = ", lambda, sep = ""), xlab = "Sample mean", ...)

  curve(dnorm(x, mean = lambda, sd = sqrt(lambda / n)), add = TRUE, col = "Red", lty = 2, lwd = 3)
}

