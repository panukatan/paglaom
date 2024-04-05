#'
#' Retrieve download links for yearly cyclones reports
#' 
#' @param url
#' 
#' @returns A vector of URL links to the yearly cyclones reports
#' 
#'

cyclones_get_report_links <- function(url = "https://www.pagasa.dost.gov.ph/tropical-cyclone/publications/annual-report") {
  httr::config(ssl_verifypeer = 0L) |>
    httr::set_config()
  
  url_session <- rvest::session(url)
  
  url_session |>
    rvest::html_elements(css = ".panel .panel-body li a") |>
    rvest::html_attr(name = "href")
}


#'
#' Download cyclones reports
#' 
#' @param url_link
#' @param directory
#'
#' @returns File downloaded to specified path (invisible). A character value for
#'   path to downloaded file
#'

cyclones_download_report <- function(url_link, directory) {
  ## Check if directory exists; if not create ----
  if (!dir.exists(directory))
    dir.create(directory, showWarnings = FALSE, recursive = TRUE)
  
  ## Create file path to download ----
  path <- file.path(directory, basename(url_link))

  ## Download file/s ----
  Map(
    f = download.file,
    url = as.list(url_link),
    destfile = as.list(path)
  )
  
  ## Return path/s ----
  path
}
