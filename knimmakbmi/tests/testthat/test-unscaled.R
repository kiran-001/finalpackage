library(testthat)

test_that("unscale reverses scaling correctly", {
  original = 1:10
  scaled = scale(original)
  unscaled = unscale(scaled)

  # Test if the unscaled data is nearly identical to the original
  expect_true(all.equal(unscaled, original, tolerance = 1e-8))
})

test_that("unscale handles non-scaled input", {
  non_scaled = 1:10  # This vector isn't scaled and has no attributes
  expect_error(unscale(non_scaled), "The input vector does not have scaling attributes.")
})

test_that("unscale handles incorrect input types", {
  non_numeric = "a string"
  expect_error(unscale(non_numeric), "Input must be a numeric vector.")
})

test_that("unscale works with NA values", {
  original_with_na = c(1, 2, NA, 4, 5)
  scaled_with_na = scale(original_with_na)  # scale without na.rm, which isn't available

  # Expect that NA handling in scale might propagate NA in output, adjust expectation
  unscaled_with_na = unscale(scaled_with_na)

  # Check if the result matches expected behavior considering NA handling
  expect_true(all.equal(unscaled_with_na, original_with_na, check.attributes = FALSE))
})
