#' ntickets
#'
#' @param N number of seats on the flight
#' @param gamma probability a plane will truly be overbooked
#' @param p probability of a "show"
#'
#' @importFrom stats pbinom optimise
#'
#' @return A list of values and sophisticated graphs
#'
#' @export
#'
#' @examples
#' result <- ntickets(N = 200, gamma = 0.02, p = 0.95)
#' print(result)
ntickets <- function(N, gamma, p) {
  # Discrete distribution case (binomial)
  n_vals <- seq(N, floor(N + N / 10), by = 1)  # Generate sequence of n values

  # Compute the difference between 1 - gamma and the cumulative binomial probability
  tmp <- 1 - gamma - pbinom(q = N, size = n_vals, prob = p)

  # Find the index of the minimum absolute difference
  ind <- which.min(abs(tmp))

  # Get the optimal n (nd)
  nd <- n_vals[ind]

  # Continuous approximation (normal)
  f_continuous <- function(n) {
    mean <- n * p
    stddev <- sqrt(n * p * (1 - p))
    return(abs(1 - gamma - pnorm(N + 0.5, mean = mean, sd = stddev)))
  }

  # Optimize continuous case (nc)
  nc <- optimise(f_continuous, interval = c(N, N + N / 10))$minimum

  # Return named list of results
  return(list(nd = nd, nc = round(nc), N = N, p = p, gamma = gamma, tmp = tmp, n_vals = n_vals))
}

# Example usage
result <- ntickets(N = 200, gamma = 0.02, p = 0.95)
print(result)

# Plotting discrete case
plot(result$n_vals, abs(result$tmp), type = "b", col = "blue", pch = 19, lwd = 2,
     xlab = "Number of Tickets (n)", ylab = "Objective Function",
     main = paste("Objective Vs n to find optimal tickets sold\n",
                  paste("(", result$nd, ") gamma= ", result$gamma, " N=", result$N, " discrete", sep="")))
abline(v = result$nd, col = "red", lty = 2)

# Continuous approximation plot
n_vals_continuous <- seq(result$N, floor(result$N + result$N / 10), by = 1)
continuous_vals <- sapply(n_vals_continuous, function(n) {
  mean <- n * result$p
  stddev <- sqrt(n * result$p * (1 - result$p))
  return(abs(1 - result$gamma - pnorm(result$N + 0.5, mean = mean, sd = stddev)))
})

plot(n_vals_continuous, continuous_vals, type = "l", col = "green", pch = 19, lwd = 2,
     xlab = "Number of Tickets (n)", ylab = "Objective Function",
     main = paste("Objective Vs n to find optimal tickets sold\n",
                  paste("(", round(result$nc, 2), ") gamma= ", result$gamma, " N=", result$N, " continuous", sep="")))
abline(v = result$nc, col = "red", lty = 2)
