library(testthat)
library(httr)
library(readr)
library(dplyr)
library(webmockr)

webmockr::enable()  # Make sure webmockr is enabled

# Correct the stub to match the actual request being made
webmockr::stub_request("post", "https://fakeurl.com/api/") %>%
  webmockr::to_return(
    status = 200,
    body = "record_id,code\n101,1\n102,2",
    headers = list('Content-Type' = 'text/csv')
  )

test_that("Successful retrieval and processing of REDCap data", {
  # Assuming the REDCap token is correctly set in the environment for the test
  Sys.setenv(REDCAP_TOKEN = "valid_token")

  # Run the function with the mocked POST request
  result = downloadRedcapReport("REDCAP_TOKEN", "https://fakeurl.com/api/", "12345")

  # Expected tibble from the CSV content
  expected_tibble = read_csv("record_id,code\n101,1\n102,2", show_col_types = FALSE) %>%
    as_tibble()

  # Check if the function result matches the expected tibble
  expect_equal(result, expected_tibble)

  # Clean up the environment variable set for testing
  Sys.unsetenv("REDCAP_TOKEN")
})
