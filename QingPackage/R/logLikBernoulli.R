#' Log-likelihood of Bernoulli Distribution
#'
#' This function calculates the maximum likelihood estimate of probability
#' parameter for a Bernoulli distribution, given a vector of binary data.
#'
#' @param data A numeric vector containing binary data (0s and 1s).
#' @return The MLE of the probability parameter.
#' @export
#' @examples
#' data <- c(1, 0, 0, 0, 1, 1, 1)
#' p_max <- logLikBernoulli(data)
logLikBernoulli <- function(data) {
  p_grid <- seq(0, 1, by = 0.001)
  loglik <- sapply(p_grid, function(p) sum(log(p^data * (1-p)^(1-data))))
  p_grid[which.max(loglik)]
}
