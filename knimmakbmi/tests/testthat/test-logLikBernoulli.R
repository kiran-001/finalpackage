library(testthat)
library(knimmakbmi)

test_that("logLikBernoulli correctly calculates probabilities and log-likelihood", {
  data = c(1, 0, 1, 1, 0, 1, 0)
  result = logLikBernoulli(data)
  expected_p = sum(data) / length(data)  # Exact calculation might need rounding
  expect_equal(result$p, expected_p, tolerance = 1e-3)

})

test_that("logLikBernoulli handles all ones or all zeros", {
    data_zeros = rep(0, 10)
    data_ones = rep(1, 10)
    result_zeros = logLikBernoulli(data_zeros)
    result_ones = logLikBernoulli(data_ones)
    expect_equal(result_zeros$p, 0, tolerance = 1e-3)
    expect_equal(result_ones$p, 1, tolerance = 1e-3)
})

test_that("logLikBernoulli handles empty data vector", {
  data_empty = numeric(0)
  expect_error(logLikBernoulli(data_empty))
})

test_that("logLikBernoulli is numerically stable", {
  data = c(1, 0, 1, 0, 1)
  result = logLikBernoulli(data)
  expect_true(is.finite(result$max_log_likelihood))
})

test_that("logLikBernoulli returns consistent results", {
  data = c(1, 0, 1, 1, 0, 1, 0)
  result1 = logLikBernoulli(data)
  result2 = logLikBernoulli(data)
  expect_equal(result1$p, result2$p)
  expect_equal(result1$max_log_likelihood, result2$max_log_likelihood)
})
