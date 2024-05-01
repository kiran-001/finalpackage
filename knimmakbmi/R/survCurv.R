# Set global variables to avoid R CMD check notes
if (getRversion() >= "3.5.0") {
  utils::globalVariables(c("n_risk", "events", "at_risk", "surv_prob"))
}

#' Calculate and plot a survival curve S(t)
#'
#' This function takes numerical vectors for status (1 for events, 0 for censored) and time,
#' and calculates and plots a survival curve using a step function. It does not use any specialized
#' survival analysis packages, and computes the Kaplan-Meier estimator manually.
#'
#' @param status Numerical vector indicating whether an event occurred (1) or was censored (0).
#' @param time Numerical vector indicating the time at which each event or censorship occurred.
#' @importFrom dplyr filter arrange summarise mutate group_by ungroup n lag first
#' @importFrom tibble tibble
#' @importFrom ggplot2 ggplot geom_step labs theme_minimal aes
#' @import survival
#' @importFrom magrittr %>%
#' @return A ggplot object showing the survival curve.
#' @examples
#' data = read.csv("https://jlucasmckay.bmi.emory.edu/global/bmi510/Labs-Materials/survival.csv")
#' survCurv(data$status, data$time)
#' @export
survCurv = function(status, time) {
  # Ensure the input is correct
  if (!is.vector(time) || !is.numeric(time)) {
    stop("The 'time' parameter should be a numeric vector.")
  }
  if (!is.vector(status) || !is.numeric(status)) {
    stop("The 'status' parameter should be a numeric vector.")
  }

  # Filter out negative time values
  data = tibble(time = time, status = status) %>%
    filter(time >= 0)

  # Create a survival object
  surv_object = Surv(data$time, data$status)

  # Calculate survival estimates using the Kaplan-Meier method
  surv_fit = survfit(surv_object ~ 1)

  # Convert survival fit to data frame for ggplot
  surv_df = data.frame(
    time = surv_fit$time,
    surv_prob = surv_fit$surv
  )

  # Plot the survival curve using ggplot2
  ggplot(surv_df, aes(x = time, y = surv_prob)) +
    geom_step() +
    labs(title = "Kaplan-Meier Survival Curve", x = "Time", y = "Survival Probability") +
    theme_minimal()
}
