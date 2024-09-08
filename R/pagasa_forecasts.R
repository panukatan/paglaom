#'
#' Create PAGASA links to regional weather forecasts PDF
#' 
#' @returns A vector of URLs of PAGASA weather forecasts PDFs
#' 
#' @examples
#' forecasts_create_urls()
#' 
#' @rdname forecasts_create
#' @export
#' 

forecasts_create_urls <- function() {
  base_url <- "https://src.panahon.gov.ph/pubfiles/prsd"
 
  regions <- c(
    "mindanao", "national-capital-region", "northern-luzon",
    "southern-luzon", "visayas"
  )

  file.path(base_url, regions, "regional_forecast.pdf")
}

#'
#' Download a PAGASA regional weather forecast PDF
#' 
#' @param url A URL to a PAGASA regional weather forecasts PDF.
#' @param directory A direcotry path to download the PAGASA regional weather 
#'   forecasts PDF to. Default is to current working directory.
#' @param overwrite Logical. Should an existing file with the same file path be
#'   overwritten? Default is FALSE.
#' 
#' @returns A file path to the downloaded PAGASA regional weather forecast PDF.
#' 
#' @examples
#' forecasts_download(
#'   url = "https://src.panahon.gov.ph/pubfiles/prsd/mindanao/regional_forecast.pdf"
#' ) 
#' 
#' @rdname forecasts_download
#' @export
#' 

forecasts_download <- function(url, 
                               directory = "data-raw/forecasts",
                               overwrite = FALSE) {
  ## Quiet down ssl verification ----
  h <- curl::new_handle()
  curl::handle_setopt(h, .list = list(ssl_verifypeer = 0L))

  ## Create file path to download ----
  destfile <- file.path(
    directory, Sys.Date(),
    stringr::str_remove(
      string = url, 
      pattern = "https://src.panahon.gov.ph/pubfiles/prsd/"
    )
  )

  ## Full directory path ----
  full_directory <- stringr::str_remove(
    string = destfile,
    pattern = "/regional_forecast.pdf"
  )

  ## Create directories as needed ----
  if (!dir.exists(directory)) dir.create(directory)
  if (!dir.exists(file.path(directory, Sys.Date()))) 
    dir.create(file.path(directory, Sys.Date()))
  if (!dir.exists(full_directory)) dir.create(full_directory)

  ## Download PDF ----
  if (
    !destfile %in% list.files(full_directory, full.names = TRUE) | 
      length(list.files(full_directory, full.names = TRUE)) == 0
  ) {
      ## Download file ----
      #download.file(url = url, destfile = destfile)
      curl::curl_download(url = url, destfile = destfile, handle = h)
  } else {
      if (overwrite) {
        ## Download file ----
        #download.file(url = url, destfile = destfile)
        curl::curl_download(url = url, destfile = destfile, handle = h)
      }
    }

  ## Return path to downloaded file ----
  destfile
}
