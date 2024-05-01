library(testthat)
library(knimmakbmi)
library(ggplot2)

test_that("survCurv returns a ggplot object", {
  status = c(1, 0, 1, 1, 0, 1, 0)
  time = c(1, 2, 3, 4, 5, 6, 7)
  result_plot = survCurv(status, time)
  expect_true(is.ggplot(result_plot))
})
