#' Calculate Minimum Sample Size for Desired Power
#'
#' This function calculates the minimum sample size needed to achieve a desired
#' power for a one-sample or two-sample t-test, given the input data.
#'
#' @param x1 Numeric vector, data for one-sample or first group in two-sample t-test.
#' @param x2 Optional numeric vector, data for second group in two-sample t-test.
#' @return The minimum number of observations needed.
#' @export
#' @examples
#' x1 <- c(2.3, 1.9, 2.1, 2.4, 2.2)
#' min_n <- minimumN(x1)
#' print(min_n)
#' x2 <- c(1.8, 2.0, 1.9, 2.2, 2.1)
#' min_n <- minimumN(x1, x2)
#' print(min_n)
minimumN <- function(x1, x2 = NULL) {
  if (!is.numeric(x1)) {
    stop("x1 must be a numeric vector")
  }

  if (is.null(x2)) {
    effect_size <- abs(mean(x1) / sd(x1))
  } else {
    if (!is.numeric(x2)) {
      stop("x2 must be a numeric vector")
    }
    pooled_sd <- sqrt(((length(x1) - 1) * var(x1) + (length(x2) - 1) * var(x2)) / (length(x1) + length(x2) - 2))
    effect_size <- abs(mean(x1) - mean(x2)) / pooled_sd
  }

  min_n <- pwr::pwr.t.test(
    d = effect_size,
    sig.level = 0.05,
    power = 0.8,
    type = ifelse(is.null(x2), "one.sample", "two.sample"),
    alternative = "two.sided"
  )$n

  min_n <- ceiling(min_n)
  return(min_n)
}
