#' Download a report from REDCap
#'
#' This function downloads a specified report from REDCap using API credentials stored in an environment variable.
#' The result is returned as a tibble.
#'
#' @param redcapTokenName The name of the environment variable containing the REDCap API token.
#' @param redcapUrl The URL to the REDCap API endpoint.
#' @param redcapReportId The ID of the REDCap report to download.
#' @return A tibble containing the data from the REDCap report.
#' @importFrom httr POST content
#' @importFrom readr read_csv
#' @importFrom dplyr as_tibble
#' @export
downloadRedcapReport = function(redcapTokenName, redcapUrl, redcapReportId) {
  requireNamespace("httr", quietly = TRUE)
  requireNamespace("readr", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)

  # Fetch API token from environment
  token = Sys.getenv(redcapTokenName)
  if (token == "") {
    stop("API token not found in environment. Please check your .REnviron setup.")
  }

  # Set up the data for the POST request
  formData = list(
    token = token,
    content = 'report',
    format = 'csv',
    report_id = as.character(redcapReportId),
    csvDelimiter = ',',
    rawOrLabel = 'raw',
    rawOrLabelHeaders = 'raw',
    exportCheckboxLabel = 'false',
    returnFormat = 'csv'
  )

  # Send the POST request
  response = httr::POST(redcapUrl, body = formData, encode = "form")
  if (httr::status_code(response) != 200) {
    stop("Failed to fetch data from REDCap: ", httr::http_status(response)$message)
  }

  # Read the content of the response
  content = httr::content(response, "text", encoding = "UTF-8")

  # Convert CSV content to a tibble
  result = readr::read_csv(content)
  return(dplyr::as_tibble(result))
}
