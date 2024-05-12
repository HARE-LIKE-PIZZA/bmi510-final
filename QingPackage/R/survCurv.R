#' Survival Curve Estimator
#'
#' This function calculates and plots the survival curve for right-censored survival data.
#' It requires a status indicator (1 if the event of interest occurred, 0 if censored) and
#' the corresponding times.
#'
#' @param status Numeric vector indicating the event status.
#' @param time Numeric vector of times at which events or censoring occurred.
#' @return Plots the survival curve.
#' @export
#' @examples
#' # Data is assumed to be loaded and preprocessed outside this function
#' survCurv(status, time)
survCurv <- function(status, time) {
  data <- data.frame(status, time)
  data <- data[order(data$time),]
  n_risk <- rep(0, nrow(data))
  n_event <- rep(0, nrow(data))
  n_risk[1] <- nrow(data)
  n_event[1] <- data$status[1]
  for (i in 2:nrow(data)) {
    n_risk[i] <- n_risk[i-1] - n_event[i-1]
    n_event[i] <- data$status[i]
  }
  surv_prob <- cumprod(1 - n_event / n_risk)
  surv_curve <- stepfun(data$time, c(1, surv_prob))

  # Plot the survival curve
  plot(surv_curve, main = "Survival Curve", xlab = "Time", ylab = "Survival Probability",
       xlim = c(0, max(data$time)), ylim = c(0, 1))
}
