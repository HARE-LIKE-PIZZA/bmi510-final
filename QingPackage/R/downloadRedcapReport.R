#' Download a Report from REDCap
#'
#' This function downloads a specified report from REDCap using an API token.
#' It requires the name of the environment variable holding the API token,
#' the URL to the REDCap project, and the report ID.
#'
#' @param redcapTokenName String, the name of the environment variable that contains the REDCap API token.
#' @param redcapUrl String, the base URL to the REDCap project.
#' @param redcapReportId Integer, the ID of the REDCap report to download.
#' @return A tibble containing the report data.
#' @export
#' @examples
#' reportData <- downloadRedcapReport("MY_REDCAP_TOKEN", "https://redcap.example.com", 1234)
#' print(reportData)
downloadRedcapReport <- function(redcapTokenName, redcapUrl, redcapReportId) {
  redcapToken <- Sys.getenv(redcapTokenName)

  if (nchar(redcapToken) == 0) {
    stop("API token not found in the .REnviron file")
  }

  apiEndpoint <- paste0(redcapUrl, "/api/")
  params <- list(
    token = redcapToken,
    content = "report",
    format = "json",
    report_id = redcapReportId
  )

  response <- httr::POST(apiEndpoint, body = params, encode = "form")
  if (httr::status_code(response) != 200) {
    stop("Failed to retrieve the REDCap report")
  }

  jsonData <- httr::content(response, "text", encoding = "UTF-8")
  data <- jsonlite::fromJSON(jsonData)
  reportData <- tibble::as_tibble(data)
  return(reportData)
}
