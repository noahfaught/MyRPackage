#' @title Iteration Barplot
#'
#' @param iter "description"
#' @param n "description"
#' @param p "description"
#'
#' @importFrom grDevices rainbow
#' @importFrom graphics barplot
#'
#' @return A sophisticated barplot
#'
#' @examples
#' \dontrun{bin100 = mybin(iter = 100, n = 10, p = 0.7)}
#' @export
mybin = function(iter = 1000, n = 10, p = 0.5) {
  # make a matrix to hpld the samples initially filled with NA's
  sam.mat = matrix(NA, nrow = n, ncol = iter, byrow = TRUE)

  # make a vector to hold the numer of successes in each trial
  succ = c()

  # make a table for those successes
  for(i in 1:iter) {
    # fill each column with a new sample
    sam.mat[,i] = sample(c(1, 0), n, replace = TRUE, prob = c(p, 1 - p))

    # calculate a statistic from this sample
    succ[i] = sum(sam.mat[,i])
  }
  # make a table of successes again
  succ.tab = table(factor(succ, levels = 0:n))

  # now make a barplot or the proportions
  iter.lab = paste0("iter = ", iter)
  n.lab = paste0("n = ", n)
  p.lab = paste0("p = ", p)
  lab = paste(iter.lab, n.lab, p.lab, sep = ", ")
  barplot(succ.tab / (iter), col = rainbow(n+1), main = "Binomial simulation", sub = lab, xlab = "Number of successes")
  succ.tab / iter
}
