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
#' @importFrom magrittr %>%
#' @return A ggplot object showing the survival curve.
#' @examples
#' data <- read.csv("https://jlucasmckay.bmi.emory.edu/global/bmi510/Labs-Materials/survival.csv")
#' survCurv(data$status, data$time)
#' @export
survCurv = function(status, time) {
  if (!is.vector(time) || !is.numeric(time)) {
    stop("The 'time' parameter should be a numeric vector.")
  }
  if (!is.vector(status) || !is.numeric(status)) {
    stop("The 'status' parameter should be a numeric vector.")
  }
  data = tibble(time = time, status = status) %>%
    arrange(time) %>%
    group_by(time) %>%
    summarise(events = sum(status), n_risk = n(), .groups = 'drop') %>%
    mutate(at_risk = lag(cumsum(n_risk), default = first(n_risk)) - lag(cumsum(events), default = 0),
           surv_prob = 1 - events / at_risk) %>%
    mutate(surv_prob = cumprod(surv_prob))

  # Plot the survival curve
  ggplot(data, aes(x = time, y = surv_prob)) +
    geom_step() +
    labs(title = "Survival Curve", x = "Time", y = "Survival Probability") +
    theme_minimal()
}
