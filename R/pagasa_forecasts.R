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
#' @param .url A URL to a PAGASA regional weather forecasts PDF.
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

forecasts_download <- function(.url, 
                               directory = "data-raw/forecasts",
                               overwrite = FALSE) {
  ## Quiet down ssl verification ----
  h <- curl::new_handle()
  curl::handle_setopt(h, .list = list(ssl_verifypeer = 0L))

  ## Create file path to download ----
  destfile <- file.path(
    directory, Sys.Date(),
    stringr::str_remove(
      string = .url, 
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
      curl::curl_download(url = .url, destfile = destfile, handle = h)
  } else {
      if (overwrite) {
        ## Download file ----
        #download.file(url = url, destfile = destfile)
        curl::curl_download(url = .url, destfile = destfile, handle = h)
      }
    }

  ## Return path to downloaded file ----
  destfile
}

#'
#' Process PAGASA forecasts data
#' 
#' @param path_to_pdf A file path to PAGASA regional forecasts PDF.
#' @param .text A vector of text values retrieved from PAGASA regional
#'   forecasts PDF.
#' 
#' @returns A tibble of raw PAGASA regional forecasts data.
#' 
#' @examples
#' forecasts_get_data("data-raw/2024-09-08/mindanao/regional_forecast.pdf")
#' 
#' @rdname forecasts_get
#' @export
#' 

forecasts_get_data <- function(path_to_pdf) {
  ## Get text from PDF ----
  .text <- pdftools::pdf_text(pdf = path_to_pdf) |>
    stringr::str_split(pattern = "\n") |>
    (\(x) x[[1]])()

  ## Get various identifying information for current data ----
  regional_group <- forecasts_get_group(.text)
  date_issued <- forecasts_get_date_issued(.text)
  validity <- forecasts_get_validity(.text)
  regional_group_summary <- forecasts_get_regional_summary(.text)

  ## Concatenate identifiers to single data.frame ----
  df <- data.frame(
    regional_group, date_issued, validity, regional_group_summary,
    geograhic_unit = which(
      pagasa_forecast_regions$regional_grouping == regional_group$regional_group
    ) |>
      (\(x) pagasa_forecast_regions$geographic_unit[x])()
  )

  ## Add forecasts data ----
  df <- df |>
    data.frame(forecasts_get_weather(.text))

  ## Special case for National Capital Region wind data ----
  if ("National Capital Region" %in% regional_group$regional_group) {
    df <- df |>
      data.frame(
        forecasts_get_wind(.text) |>
          (\(x)
            {
              x[10, 2] <- x[11, 1]
              x[c(1:10, 12:13), ]
          }
          )()
      )
  } else {
    df <- df |>
      data.frame(forecasts_get_wind(.text))
  }

  ## Special case for Northern Luzon coastal data ----
  if ("Northern Luzon" %in% regional_group$regional_group) {
    df <- df |>
      data.frame(
        forecasts_get_coastal(.text) |>
          (\(x)
            {
              rbind(
                x[1:7, ],
                data.frame(
                  coastal_today = rep(NA_character_, 8),
                  coastal_tomorrow = rep(NA_character_, 8)
                ),
                x[8, ]
              )
            }
          )()
      )
  } else {
    ## Special case for National Capital Region coastal data ----
    if ("National Capital Region" %in% regional_group$regional_group) {
      df <- df |>
        data.frame(
          forecasts_get_coastal(.text) |>
            (\(x)
              {
                rbind(
                  x[1, ],
                  data.frame(
                    coastal_today = rep(NA_character_, 2),
                    coastal_tomorrow = rep(NA_character_, 2)
                  ),
                  x[4:5, ],
                  data.frame(
                    coastal_today = rep(NA_character_, 2),
                    coastal_tomorrow = rep(NA_character_, 2)
                  ),
                  x[8, ],
                  data.frame(
                    coastal_today = NA_character_,
                    coastal_tomorrow = NA_character_
                  ),
                  x[10, ],
                  data.frame(
                    coastal_today = NA_character_,
                    coastal_tomorrow = NA_character_
                  ),
                  x[12, ]
                )
              }
            )()
        )
    } else {
      df <- df |>
        data.frame(forecasts_get_coastal(.text))
    }
  }

  ## Add temperature data ----
  df <- df |>
    data.frame(forecasts_get_temperature(.text))

  ## Remove row names and convert to tibble ----
  row.names(df) <- NULL
  df <- tibble::tibble(df)

  ## Return df ----
  df
}

#'
#' @rdname forecasts_get
#' @export
#' 

forecasts_get_group <- function(.text) {
  pagasa_division <- which(
    stringr::str_detect(string = .text, pattern = "REGIONAL WEATHER")
  ) |>
    (\(x) .text[x])() |>
    stringr::word(-1)

  regional_group <- pagasa_division |>
    (\(x)
      {
        ifelse(
          x == "MINPRSD", "Mindanao",
          ifelse(
            x == "VISPRSD", "Visayas",
            ifelse(
              x == "SLPRSD", "Southern Luzon",
              ifelse(
                x == "NCR-PRSD", "National Capital Region",
                "Northern Luzon"
              )
            )
          )
        )
      }
    )()
  
  ## Concatenate into a data.frame ----
  data.frame(cbind(pagasa_division, regional_group))
}

#'
#' @rdname forecasts_get
#' @export
#' 

forecasts_get_date_issued <- function(.text) {
  stringr::str_detect(
    string = .text, pattern = "Issued At|Issued at"
  ) |>
    (\(x) .text[x])() |>
    stringr::str_remove(pattern = "Issued At: |Issued at: ") #|>
    #strptime(format = "%I:%M %p, %d %B, %Y", tz = "PST")
}

#'
#' @rdname forecasts_get
#' @export
#' 

forecasts_get_validity <- function(.text) {
  stringr::str_detect(
    string = .text, pattern = "Valid Beginning"
  ) |>
    (\(x) .text[x])() |>
    stringr::str_remove(pattern = "Valid Beginning: ")
}

#'
#' @rdname forecasts_get
#' @export
#' 

forecasts_get_regional_summary <- function(.text) {
  .text |>
    (\(x)
      {
        from <- which(stringr::str_detect(string = x, pattern = "Weather:"))[1]
        to <- which(stringr::str_detect(string = x, pattern = "Provinces")) - 1
        x[from:to]
    }
    )() |>
    paste(collapse = " ") |>
    stringr::str_remove(pattern = "Weather: ")
}

#'
#' @rdname forecasts_get
#' @export
#' 

forecasts_get_weather <- function(.text) {
  which(stringr::str_detect(string = .text, pattern = "Weather: "))[-1] |>
    (\(x)
      {
        cbind(
          .text[x] |>
            stringr::str_remove_all(
              pattern = stringr::str_split(
                string = pagasa_forecast_regions$geographic_unit,
                pattern = " "
              ) |>
                unlist() |>
                unique() |>
                paste(collapse = "|")
            ) |>
            stringr::str_remove_all(pattern = "Weather: ") |>
            stringr::str_split(pattern = "\\s{2,}", simplify = TRUE), 
          .text[x + 1] |>
            stringr::str_remove_all(
              pattern = stringr::str_split(
                string = pagasa_forecast_regions$geographic_unit,
                pattern = stringr::boundary("word")
              ) |>
                unlist() |>
                unique() |>
                (\(x) paste0("\\b", x) |> paste(collapse = "|"))()
            ) |>
            stringr::str_split(pattern = "\\s{2,}", simplify = TRUE)
        )
      }
    )() |>
    (\(x)
      {
        data.frame(
          weather_today = paste(x[ , 2], x[ , 5]),
          weather_tomorrow = paste(x[ , 3], x[ , 6])
        )
      }
    )()
}

#'
#' @rdname forecasts_get
#' @export
#' 

forecasts_get_wind <- function(.text) {
  which(stringr::str_detect(string = .text, pattern = "Wind:")) |>
    (\(x) .text[x])() |>
    stringr::str_remove_all(
      pattern = stringr::str_split(
        string = pagasa_forecast_regions$geographic_unit,
        pattern = stringr::boundary("word")
      ) |>
        unlist() |>
        unique() |>
        (\(x) paste0("\\b", x) |> paste(collapse = "|"))()
    ) |>
    stringr::str_remove_all(pattern = "[0-9]{2}-[0-9]{2}°C|[0-9]{2}\\s-\\s[0-9]{2}°C") |>
    stringr::str_remove_all(pattern = "Wind:") |>
    stringr::str_split(pattern = "\\s{2,}", simplify = TRUE) |>
    (\(x)
      {
        data.frame(
          wind_today = x[ , 2],
          wind_tomorrow = x[ , 3]
        )
      }
    )()
}

#'
#' @rdname forecasts_get
#' @export
#' 

forecasts_get_coastal <- function(.text) {
  which(stringr::str_detect(string = .text, pattern = "Coastal:")) |>
    (\(x) .text[x])() |>
    stringr::str_remove_all(pattern = "[0-9]{2}-[0-9]{2}°C|[0-9]{2}\\s-\\s[0-9]{2}°C") |>
    stringr::str_remove_all(pattern = "Coastal:") |>
    stringr::str_split(pattern = "\\s{2,}", simplify = TRUE) |>
    (\(x)
      {
        data.frame(
          coastal_today = x[ , 2],
          coastal_tomorrow = x[ , 3]
        )
      }
    )()
}

#'
#' @rdname forecasts_get
#' @export
#' 

forecasts_get_temperature <- function(.text) {
  stringr::str_extract_all(
    string = .text, pattern = "[0-9]{2}-[0-9]{2}°C|[0-9]{2}\\s-\\s[0-9]{2}°C"
  ) |>
    (\(x) x[lapply(X = x, FUN = length) != 0])() |>
    (\(x)
      {
        lapply(
          X = x,
          FUN = function(x) {
            data.frame(rbind(x)) |>
              (\(x) 
                { 
                  names(x) <- c("temperature_today", "temperature_tomorrow")
                  x
                }
              )()
          }
        )
      }  
    )() |>
    dplyr::bind_rows() |>
    (\(x) { row.names(x) <- NULL; x })()
}