library(testthat)
library(ggplot2)
library(dplyr)
library(survival)

# Assuming the function survCurv is already sourced or part of your package

# Test that the function returns a ggplot object
test_that("survCurv returns a ggplot object", {
  status = c(1, 0, 1, 1, 0, 1, 0)
  time = c(1, 2, 3, 4, 5, 6, 7)
  result_plot = survCurv(status, time)
  expect_true(is.ggplot(result_plot), "The output should be a ggplot object.")
})

# Test the function with all censored data
test_that("survCurv handles all censored data", {
  status = rep(0, 7)  # All observations are censored
  time = c(1, 2, 3, 4, 5, 6, 7)
  result_plot = survCurv(status, time)
  expect_true(is.ggplot(result_plot), "The output should still be a ggplot object even if all data are censored.")
  # Here, you might also check for the content of the plot, such as number of steps being zero
})

# Test the function with incorrect inputs
test_that("survCurv handles incorrect inputs", {
  # Non-numeric time input
  status = c(1, 0, 1)
  time = c("one", "two", "three")
  expect_error(survCurv(status, time), "The 'time' parameter should be a numeric vector.")

  # Non-numeric status input
  status = c("event", "censored", "event")
  time = c(1, 2, 3)
  expect_error(survCurv(status, time), "The 'status' parameter should be a numeric vector.")
})
