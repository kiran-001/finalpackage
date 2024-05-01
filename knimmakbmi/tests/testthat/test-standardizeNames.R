library(testthat)
library(tibble)

test_that("standardizeNames correctly standardizes names", {
  data <- tibble(`some data` = 1:5, `!!more data!!` = 5:1, `123start with number` = rnorm(5))
  standardized_data <- standardizeNames(data)

  expected_names <- c("someData", "moreData", "x123StartWithNumber")  # Adjusted expected names
  expect_equal(names(standardized_data), expected_names)
})


test_that("standardizeNames handles non-tibble input", {
  # Non-tibble input
  data <- data.frame(`some data` = 1:5, `!!more data!!` = 5:1)

  # Expect an error or a specific handling message
  expect_error(standardizeNames(data), "input must be a tibble")
})

