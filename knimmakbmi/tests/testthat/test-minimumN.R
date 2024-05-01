library(testthat)
library(pwr)

test_that("minimumN calculates correct sample sizes", {
  # Create controlled samples
  set.seed(123)
  sample1 = rnorm(50, mean = 5, sd = 1)
  sample2 = rnorm(50, mean = 5.5, sd = 1)

  # Calculate expected values within the test
  d_one_sample = (mean(sample1) - 0) / sd(sample1)
  expected_one_sample = pwr.t.test(d = d_one_sample, power = 0.8, sig.level = 0.05, type = "one.sample")

  pooled_sd = sqrt(((length(sample1) - 1) * sd(sample1)^2 + (length(sample2) - 1) * sd(sample2)^2) / (length(sample1) + length(sample2) - 2))
  d_two_sample = abs(mean(sample1) - mean(sample2)) / pooled_sd
  expected_two_sample = pwr.t.test(d = d_two_sample, power = 0.8, sig.level = 0.05, type = "two.sample")

  # Test expected values against function output
  expect_equal(ceiling(expected_one_sample$n), minimumN(sample1))
  expect_equal(ceiling(expected_two_sample$n), minimumN(sample1, sample2))
})

# Test handling of invalid inputs
test_that("minimumN handles non-numeric inputs", {
  non_numeric_sample = c("a", "b", "c")

  expect_error(minimumN(non_numeric_sample), "Both inputs must be numeric vectors.")
  expect_error(minimumN(non_numeric_sample, non_numeric_sample), "Both inputs must be numeric vectors.")
})
