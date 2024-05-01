library(testthat)

test_that("pcApprox returns a correctly approximated matrix", {
  set.seed(123)
  data_matrix = matrix(rnorm(100), nrow=10, ncol=10)
  approx_data = pcApprox(data_matrix, 5)  # Increase the number of PCs if 2 is not sufficient

  original_vector = as.vector(data_matrix)
  approx_vector = as.vector(approx_data)
  correlation_value = cor(original_vector, approx_vector)
  expect_gt(correlation_value, 0.8)
})


test_that("pcApprox handles incorrect inputs properly", {
  data_matrix = matrix(rnorm(100), nrow=10)

  expect_error(
    pcApprox(data_matrix, ncol(data_matrix) + 1),
    "Parameter npc must be a positive integer less than or equal to the number of columns in x",
    fixed = TRUE  # Ensures that the error message matches exactly
  )
})


test_that("pcApprox handles minimum and maximum npc correctly", {
  set.seed(123)
  data_matrix = matrix(rnorm(100), nrow=10)

  # Test with minimum and maximum possible npc values
  approx_data_min_npc = pcApprox(data_matrix, 1)
  approx_data_max_npc = pcApprox(data_matrix, ncol(data_matrix))

  # Check the results are matrices and match the dimensions of the original
  expect_true(is.matrix(approx_data_min_npc))
  expect_equal(dim(approx_data_min_npc), dim(data_matrix))
  expect_true(is.matrix(approx_data_max_npc))
  expect_equal(dim(approx_data_max_npc), dim(data_matrix))

  # When using all components, the reconstruction should be very close to the original
  # Calculate Mean Absolute Difference
  mad_max_npc = mean(abs(as.vector(approx_data_max_npc) - as.vector(data_matrix)))

  # Compute relative mean absolute difference
  relative_mad_max_npc = mad_max_npc / mean(abs(as.vector(data_matrix)))

  # Check if the relative MAD is below a reasonable threshold
  expect_lt(relative_mad_max_npc, 0.05)
})

