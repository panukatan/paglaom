#'
#' Get heat index images URLs
#' 
#' @param .url The URL for the heat index files from PAGASA. Currently, this is
#'   at https://pubfiles.pagasa.dost.gov.ph/iaas/heat_index/.
#' 
#' @returns A tibble containing information on URLs for known filenames for heat 
#'   index images files (usually in PNG format) found in the PAGASA pubfiles
#'   URL and the date at which URL information was extracted.
#'   
#' @examples
#' heat_index_get_image_urls()
#'
#' @export
#'

heat_index_get_image_urls <- function(.url = "https://pubfiles.pagasa.dost.gov.ph/iaas/heat_index/") {
  pagasa_session <- rvest::session(.url)
  
  data.frame(
    date = Sys.Date(),
    links = pagasa_session |>
      rvest::html_elements(css = "pre a") |>
      rvest::html_attr("href") |>
      (\(x) paste0(.url, x))() |>
      (\(x) x[stringr::str_detect(x, "HeatIndexObserved|HeatObserved|Observed%20Table")])()
  ) |>
    tibble::tibble()
}


#'
#' Download head index image files
#' 
#' @param heat_index_url A URL to a PAGASA heat index pufiles heat index image. 
#'   This can be drawn from tibble produced by `get_heat_index_image_urls()`.
#' @param .date A date associated with the `heat_index_url` on when it was
#'   retrieved. This can be drawn from tibble produced by 
#'   `get_heat_index_image_urls()`.
#' @param directory A character value for name of directory to store downloaded
#'   files to. Default is *"data-raw"*.
#' @param overwrite Should a file with the same name in `directory` be
#'   overwritten? Default to FALSE.
#'   
#' @returns
#' 
#' @examples
#' 
#' @export
#'
#'

heat_index_download_image_files <- function(heat_index_url,
                                            .date,
                                            directory = "data-raw", 
                                            overwrite = FALSE) {
  if (!dir.exists(directory)) dir.create(directory)
  
  if (!dir.exists(file.path(directory, "heat_index"))) 
    dir.create(file.path(directory, "heat_index"))
  
  if (!dir.exists(file.path(directory, "heat_index", .date)))
    dir.create(file.path(directory, "heat_index", .date))
  
  download_dir <- file.path(directory, "heat_index", .date)
  
  ## Check if files to download are already found in download_dir ----
  dir_files <- list.files(download_dir)
  
  if (overwrite | !basename(heat_index_url) %in% dir_files) {
    download.file(
      url = heat_index_url,
      destfile = file.path(download_dir, basename(heat_index_url))
    )
  }

  file.path(download_dir, basename(heat_index_url))
}


  
#'
#' Read heat index data from PNG
#'
#'
#'

heat_index_read_png_file <- function(png_file) {
  magick::image_read(path = png_file) |>
    magick::image_write(
      path = stringr::str_replace(
        string = png_file, pattern = "png", replacement = "pdf"
      ),
      format = "pdf"
    )
  
  
  png_data <- magick::image_read(path = png_file) |>
    magick::image_convert(type = "Grayscale") |>
    magick::image_deskew() |>
    magick::image_resize() |> 
    tesseract::ocr()

  df <- data.table::fread(text = png_data)
    
}




  