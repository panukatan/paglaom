#'
#' Download PAGASA daily agriculture forecasts
#' 
#' @param directory Path to directory to download the daily agriculture
#'   forecast PDF to.
#' @param overwrite Logical. Should an existing file with the same file path be
#'   overwritten? Default is FALSE.
#' 
#' @returns The path to the downloaded daily agriculture forecast PDF.
#' 
#' @examples
#' forecasts_agriculture_download()
#' 
#' @rdname forecasts_agriculture
#' @export
#'  

forecasts_agriculture_download <- function(directory = "data-raw/forecasts_agriculture",
                                           overwrite = FALSE) {
  if (weekdays(Sys.Date()) %in% c("Saturday", "Sunday")) {
    .url <- "https://pubfiles.pagasa.dost.gov.ph/pagasaweb/files/agriculture/weekend_special_farm_forecast/weekend_special_farm_weather_forecast.pdf"
  } else {
    .url = "https://pubfiles.pagasa.dost.gov.ph/pagasaweb/files/agriculture/farm_weather_forecast/farm_weather_forecast.pdf"
  }
    
  ## Quiet down ssl verification ----
  #h <- curl::new_handle()
  #curl::handle_setopt(h, .list = list(ssl_verifypeer = 0L))
  
  ## Create file path to download ----
  destfile <- file.path(directory, paste0(Sys.Date(), ".pdf"))
  
  ## Create directories as needed ----
  if (!dir.exists(directory)) dir.create(directory)

  ## Download PDF ----
  if (
    !destfile %in% list.files(directory, full.names = TRUE) | 
      length(list.files(directory, full.names = TRUE)) == 0
  ) {
      ## Download file ----
      download.file(url = .url, destfile = destfile)
      #curl::curl_download(url = url, destfile = destfile, handle = h)
  } else {
      if (overwrite) {
        ## Download file ----
        download.file(url = .url, destfile = destfile)
        #curl::curl_download(url = url, destfile = destfile, handle = h)
      }
    }

  ## Return path to downloaded file ----
  destfile
}