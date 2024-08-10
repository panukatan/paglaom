#'
#' Get URLS for climate directories in PAGASA pubfiles
#' 
#' @param .url URL for PAGASA climate directories. Default is currently
#'   https://pubfiles.pagasa.dost.gov.ph/pagasaweb/files/cad/
#'
#' @returns A character vector of URLs for the various directories for PAGASA
#'   climate data
#'   
#' @examples
#' climate_get_pdf_directory_urls()
#'
#' @export
#'

climate_get_pdf_directory_urls <- function(.url = "https://pubfiles.pagasa.dost.gov.ph/pagasaweb/files/cad/") {
  pagasa_session <- rvest::session(.url)
  
  rvest::html_elements(pagasa_session, css = "pre a") |>
    rvest::html_text() |>
    (\(x) x[stringr::str_detect(string = x, pattern = "CLIMATOLOGICAL")])() |>
    (\(x) paste0(.url, x))() |>
    stringr::str_replace_all(pattern = " ", replacement = "%20")
}


#'
#' Get URLs for all PDF files within the PAGASA climate data directories
#' 
#' @param url_dir A URL for a specific PAGASA climate data directory
#' 
#' @returns A character vector of URLs of PDFs from specified PAGASA
#'   climate data directory
#'
#' @examples
#' climate_get_pdf_urls(url_dir = "https://pubfiles.pagasa.dost.gov.ph/pagasaweb/files/cad/CLIMATOLOGICAL%20NORMALS%20(1991-2020)/")
#'
#' @export
#'

climate_get_pdf_urls <- function(url_dir) {
  pagasa_session <- rvest::session(url_dir)
  
  rvest::html_elements(pagasa_session, css = "pre a") |>
    rvest::html_attr(name = "href") |>
    (\(x) x[stringr::str_detect(string = x, pattern = "pdf")])() |>
    (\(x) x[stringr::str_detect(string = x, pattern = "Climatological%20Extremes%20%28as%20of%202020%29.pdf", negate = TRUE)])() |>
    (\(x) paste0(url_dir, x))()
}


#'
#' Download climate data PDFs from PAGASA pubfiles URL
#' 
#' @param pdf_url A URL for a specific climate data PDF from PAGASA pubfiles
#' @param directory A character value for name of directory to store downloaded
#'   files to. Default is *"data-raw"*.
#' @param overwrite Should a file with the same name in `directory` be
#'   overwritten? Default to FALSE.
#'   
#' @returns A  file path or a vector of file paths to downloaded climate data 
#'   PDFs
#' 
#' @examples
#' climate_download_pdf()
#'
#' @rdname climate_download
#' @export
#'

climate_download_pdf <- function(pdf_url, 
                                 directory = "data-raw", 
                                 overwrite = FALSE) {
  ## Get year ----
  ref_year <- stringr::str_remove_all(string = pdf_url, pattern = "%20") |>
    stringr::str_extract(pattern = "[0-9]{4}")
  
  download_dir <- file.path(directory, "climate", ref_year)
  
  if (!dir.exists(download_dir)) dir.create(download_dir, recursive = TRUE)
  
  file_name <- basename(pdf_url) |>
    stringr::str_replace_all(pattern = "%20", replacement = "_") |>
    stringr::str_remove_all(pattern = "%28|%29")
  
  if (overwrite | !file_name %in% list.files(download_dir))
    download.file(
      url = pdf_url,
      destfile = file.path(download_dir, file_name)
    )
  
  ## Return file path ----
  file.path(download_dir, file_name)
}


#'
#' @rdname climate_download
#' @export
#'

climate_download_pdfs <- function(pdf_url, 
                                  directory = "data-raw", 
                                  overwrite = FALSE) {
  lapply(
    X = pdf_url,
    FUN = climate_download_pdf,
    directory = directory,
    overwrite = overwrite
  ) |>
    unlist()
}

#'
#' Process climate data normals and extremes from PAGASA
#' 
#' @param climate_download_files A vector of paths to the downloaded climate
#'   data PDFs from PAGASA
#'   
#' @returns A tibble of climate data normals or extremes
#' 
#' @examples
#' climate_process_1991(climate_download_files)
#' climate_process_2020(climate_download_files)
#' 
#' @rdname climate_process
#' @export
#' 

climate_process_1991 <- function(climate_download_files) {
  ## Get station information ----
  station_df <- get_weather_station_info(
    climate_download_files, period = "1991"
  )
  
  ## Get paths to climate normal data PDFs ----
  pdf_path <- climate_download_files |>
    (\(x) x[stringr::str_detect(string = x, pattern = "1991")])()

  ## Read and extract table information from PDFs ----
  climate_dfs <- lapply(
    X = pdf_path,
    FUN = function(x) {
      suppressWarnings(
        pdftools::pdf_text(x) |>
          stringr::str_split(pattern = "\n") |>
          unlist() |>
          (\(x) x[stringr::str_detect(string = x, pattern = paste(c(paste0(" ", toupper(month.abb), " "), "ANNUAL"), collapse = "|"))])() |>
          stringr::str_split(pattern = "\\s+") |>
          lapply(
            FUN = function(x) {
              x[x != ""] |> 
                stringr::str_remove_all(pattern = ",")
            }
          ) |>
          (\(x) do.call(rbind, x))()
      )
    }
  ) |>
    (\(x)
      {
        names(x) <- paste0(station_df$station, "_", station_df$period) 
        x
      }
    )()

  ## Process tables with multiple rows per month ----  
  climate_dfs[[9]] <- climate_dfs[[9]] |>
    (\(x)
      {
        x[1:12, 7:15]  <- x[1:12, 4:12]
        x[1:12, 4:6]   <- NA
        x[1:12, 16:17] <- NA
        x[13, 4:6]     <- NA 
        x
      }
    )()
 
  ## Final processing/structuring/concatenating of climate data ----
  lapply(
    X = climate_dfs,
    FUN = function(x) {
      data.frame(x) |>
        (\(x)
          {
            names(x) <- c(
              "time_name", "rainfall_amount", "rainfall_days", 
              "temperature_max", "temperature_min", "temperature_mean",
              "temperature_dry", "temperature_wet", "temperature_dew",
              "vapor_pressure", "humidity_relative", "mean_sea_level_pressure",
              "wind_direction", "wind_speed", "cloud_amount", 
              "thunderstorm_days", "lightning_days"
            )
            x
          }
        )() |>
        dplyr::mutate(
          time_unit = c(rep("month", 12), "year"), .after = time_name
        )
    }
  ) |>
    dplyr::bind_rows(.id = "station_period") |>
    dplyr::mutate(
      station = stringr::str_extract_all(
        string = station_period, pattern = "^[^_]*", simplify = TRUE
      ) |>
        (\(x) x[ , 1])(),
      time_period = stringr::str_remove_all(
        string = station_period, pattern = "^[^_]*|_"
      ),
      time_name = stringr::str_to_title(time_name) |>
        lapply(
          FUN = function(x) ifelse(
            x == "Annual", x, month.name[month.abb == x]
          )
        ) |>
        unlist(),
      dplyr::across(
        .cols = c(rainfall_amount, temperature_max:mean_sea_level_pressure),
        .fns = ~as.numeric(.x)
      ),
      dplyr::across(
        .cols = c(
          rainfall_days, wind_speed, cloud_amount, thunderstorm_days, 
          lightning_days
        ),
        .fns = ~as.integer(.x)
      )
    ) |>
    dplyr::select(-station_period) |>
    dplyr::relocate(station:time_period, .before = time_name) |>
    tibble::tibble()
}

#'
#' @rdname climate_process
#' @export
#'

climate_process_2020 <- function(climate_download_files) {
  ## Get files for up to year 2020 ----
  pdf_path <- climate_download_files |>
    (\(x) x[stringr::str_detect(string = x, pattern = "2020")])()

  ## Get weather station information ----
  station_df <- get_weather_station_info(
    climate_download_files, period = "2020"
  )

  ## Read text from PDFs and extract tables data ----
  pdfs <- lapply(
    X = pdf_path,
    FUN = function(x) {
      pdftools::pdf_text(x) |>
        stringr::str_split(pattern = "\n") |>
        unlist() |>
        (\(x)
          {
            index <- seq_len(length(x))
            index <- index[stringr::str_detect(string = x, pattern = "MONTH|Period")]
            index <- c(index[1] + 2, index[2] - 1)
            x[index[1]:index[2]]
          }
        )() |>
        stringr::str_trim() |>
        stringr::str_split_fixed(pattern = "\\s+", n = 14)
    }
  ) |>
    (\(x)
      {
        names(x) <- paste0(station_df$station, "_", station_df$period)
        x
      }
    )()
  
  ## Process each PDF table that has multiple rows per month ----
  pdfs[[1]] <- pdfs[[1]] |>
    (\(x)
      {
        x[9, 2:14]   <- x[9, 1:13]
        x[9, 1]      <- x[10, 1]
        x[10, 2:12]  <- x[9, 2:12]
        x[10, 13:14] <- x[11, 1:2]
        x[c(1:10, 12:15), ]
      }
    )()
  
  pdfs[[2]] <- pdfs[[2]] |>
    (\(x)
      {
        x[1, 2:14]  <- x[1, 1:13]
        x[1, 1]     <- x[2, 1]
        x[2, 2:14]  <- x[1, 2:14]
        x[2, 4:5]   <- x[3, 1:2]
        
        x[4, 2:14]  <- x[4, 1:13]
        x[4, 1]     <- x[5, 1]
        x[5, 2:14]  <- x[4, 2:14]
        x[5, 11:12] <- x[6, 1:2]
        
        x[11, 2:14] <- x[11, 1:13]
        x[11, 1]    <- x[12, 1]
        x[12, 2:14] <- x[11, 2:14]
        x[12, 2:3]  <- x[13, 1:2]

        x[17, 2:14] <- x[17, 1:13]
        x[17, 1]    <- x[18, 1]
        x[18, 2:14] <- x[17, 2:14]
        x[18, 4:5]  <- x[19, 1:2]
        
        x[21, 2:14] <- x[21, 1:13]
        x[21, 1]    <- x[22, 1]
        x[22, 2:14] <- x[21, 2:14]
        x[22, 4:5]  <- x[23, 1:2]
        
        x[c(1:2, 4:5, 7:12, 14:18, 20:22), ]
      }
    )()
  
  pdfs[[3]] <- pdfs[[3]] |>
    (\(x)
      {
        x[14, 11:12] <- x[14, 1:2]
        x[14, 1:10]  <- x[13, 1:10]
        x[14, 13:14] <- x[13, 13:14]
        x
      }
    )()
  
  pdfs[[4]] <- pdfs[[4]] |>
    (\(x)
      {
        x[6, 2:14] <- x[6, 1:13]
        x[6, 1]    <- x[7, 1]
        x[7, 2:14] <- x[6, 2:14]
        x[7, 5]    <- x[8, 1]
        x[c(1:7, 9:15), ]
      }
    )()
    
  pdfs[[5]] <- pdfs[[5]] |>
    (\(x)
      {
        x[5, 2:14]  <- x[5, 1:13]
        x[5, 1]     <- x[6, 1]
        x[6, 2:14]  <- x[5, 2:14]
        x[6, 11:12] <- x[7, 1:2]
        
        x[10, 2:14] <- x[10, 1:13]
        x[10, 1]    <- x[11, 1]
        x[11, 2:14] <- x[10, 2:14]
        x[11, 8:10] <- x[12, 1:3]
        
        x[14, 2:14] <- x[14, 1:13]
        x[14, 1]    <- x[15, 1]
        x[15, 2:14] <- x[14, 2:14]
        x[15, 2:3]  <- x[16, 1:2]
        
        x[c(1:6, 8:11, 13:15, 17:19), ]
      }
    )()
  
  pdfs[[6]] <- pdfs[[6]] |>
    (\(x)
      {
        x[7, 2:14]  <- x[7, 1:13]
        x[7, 1]     <- x[8, 1]
        x[8, 2:14]  <- x[7, 2:14]
        x[8, 11:12] <- x[9, 1:2]
        
        x[c(1:8, 10:15), ]
      }
    )()
    
  pdfs[[7]] <- pdfs[[7]] |>
    (\(x)
      {
        x[2, 2:14]  <- x[2, 1:13]
        x[2, 1]      <- x[3, 1]
        x[3, 8:10]   <- x[3, 2:4]
        x[3, 2:7]    <- x[2, 2:7]
        x[3, 11:14]  <- x[2, 11:14]
        x[4, 8:10]   <- x[4, 1:3]
        x[4, 1:7]    <- x[3, 1:7]
        x[4, 11:14]  <- x[3, 11:14]
        
        x[5, 2:14]   <- x[5, 1:13]
        x[5, 1]      <- x[6, 1]
        x[6, 2:14]   <- x[5, 2:14]
        x[6, 8:10]   <- x[7, 1:3]
        
        x[12, 2:14]  <- x[12, 1:13]
        x[12, 1]     <- x[13, 1]
        x[13, 2:14]  <- x[12, 2:14]
        x[13, 11:12] <- x[14, 1:2] 
        
        x[19, 2:14]  <- x[19, 1:13]
        x[19, 1]     <- x[20, 1]
        x[20, 2:14]  <- x[19, 2:14]
        x[20, 2:3]   <- x[21, 1:2]
        
        x[c(1:6, 8:13, 15:20), ]
      }
    )()
  
  pdfs[[8]] <- pdfs[[8]] |>
    (\(x)
      {
        x[6, 2:14]  <- x[6, 1:13]
        x[6, 1]     <- x[7, 1]
        x[7, 2:14]  <- x[6, 2:14]
        x[7, 8:10]  <- x[8, 1:3]
        
        x[15, 2:14] <- x[15, 1:13]
        x[15, 1]    <- x[16, 1]
        x[16, 2:14] <- x[15, 2:14]
        x[16, 2:5]  <- x[17, 1:4]
        x[16, 8:10] <- x[17, 5:7]
        
        x[c(1:7, 9:16), ]
      } 
    )()
  
  pdfs[[9]] <- pdfs[[9]] |>
    (\(x)
      {
        x[2, 2:14]  <- x[2, 1:13]
        x[2, 1]     <- x[3, 1]
        x[3, 2:14]  <- x[2, 2:14]
        x[3, 13:14] <- x[4, 1:2]
        
        x[10, 2:14] <- x[10, 1:13]
        x[10, 1]    <- x[11, 1]
        x[11, 2:14] <- x[10, 2:14]
        x[11, 2:3]  <- x[12, 1:2]
        
        x[17, 2:14] <- x[17, 1:13]
        x[17, 1]    <- x[18, 1]
        x[18, 2:14] <- x[17, 2:14]
        x[18, 8:10] <- x[19, 1:3]
        
        x[c(1:3, 5:11, 13:18), ]
      } 
    )()
  
  pdfs[[10]] <- pdfs[[10]] |>
    (\(x)
      {
        x[13, 2:14]  <- x[13, 1:13]
        x[13, 1]     <- x[14, 1]
        x[14, 2:14]  <- x[13, 2:14]
        x[14, 11:12] <- x[15, 1:2]
        
        x[1:14, ]
      }
    )()
  
  pdfs[[11]] <- pdfs[[11]] |>
    (\(x)
      {
        x[4, 2:14]  <- x[4, 1:13]
        x[4, 1]     <- x[5, 1]
        x[5, 2:14]  <- x[4, 2:14]
        x[5, 8:10]  <- x[6, 1:3]
        
        x[14, 2:14] <- x[14, 1:13]
        x[14, 1]    <- x[15, 1]
        x[15, 2:14] <- x[14, 2:14]
        x[15, 8:10] <- x[16, 1:3]
        
        x[c(1:5, 7:15, 17), ]
      }
    )()
  
  pdfs[[13]] <- pdfs[[13]] |>
    (\(x)
      {
        x[7, 2:14]  <- x[7, 1:13]
        x[7, 1]     <- x[8, 1]
        x[8, 2:14]  <- x[7, 2:14]
        x[8, 11:12] <- x[9, 1:2]
        
        x[c(1:8, 10:15), ]
      }
    )()
  
  pdfs[[14]] <- pdfs[[14]] |>
    (\(x)
      {
        x[1, 2:14]  <- x[1, 1:13]
        x[1, 1]     <- x[2, 1]
        x[2, 2:14]  <- x[1, 2:14]
        x[2, 8:10]  <- x[3, 1:3]
        
        x[6, 2:14]  <- x[6, 1:13]
        x[6, 1]     <- x[7, 1]
        x[7, 2:14]  <- x[6, 2:14]
        x[7, 2:3]   <- x[8, 1:2]   
        
        x[12, 2:14]  <- x[12, 1:13]
        x[12, 1]     <- x[13, 1]
        x[13, 4:14]  <- x[12, 4:14]
        x[14, 2:3]   <- x[14, 1:2]
        x[14, 4:14]  <- x[12, 4:14]
        x[14, 1]     <- x[12, 1]
        
        x[17, 2:14]  <- x[17, 1:13]
        x[17, 1]     <- x[18, 1]
        x[18, 2:14]  <- x[17, 2:14]
        x[18, 2:3]   <- x[19, 1:2]
        
        x[21, 2:14]  <- x[21, 1:13]
        x[21, 1]     <- x[22, 1]
        x[22, 2:14]  <- x[21, 2:14]
        x[22, 11:12] <- x[23, 1:2]
        
        x[c(1:2, 4:7, 9:18, 20:22), ]
      }
    )()
  
  pdfs[[15]] <- pdfs[[15]] |>
    (\(x)
      {
        x[9, 2:14]  <- x[9, 1:13]
        x[9, 1]     <- x[10, 1]
        x[10, 2:14] <- x[9, 2:14]
        x[10, 2:3]  <- x[11, 1:2]
        
        x[c(1:10, 12:15), ]
      }
    )()
  
  pdfs[[16]] <- pdfs[[16]] |>
    (\(x)
      {
        x[7, 2:14]  <- x[7, 1:13]
        x[7, 1]     <- x[8, 1]
        x[8, 8:10]  <- x[8, 2:4]
        x[8, 2:7]   <- x[7, 2:7]
        x[8, 11:14] <- x[7, 11:14]
        x[9, 8:10]  <- x[9, 1:3]
        x[9, 1:7]   <- x[7, 1:7]
        x[9, 11:14] <- x[7, 11:14]
        
        x[10, 2:14] <- x[10, 1:13]
        x[10, 1]    <- x[11, 1]
        x[11, 2:14] <- x[10, 2:14]
        x[11, 8:10] <- x[12, 1:3]
        
        x[16, 2:14] <- x[16, 1:13]
        x[16, 1]    <- x[17, 1]
        x[17, 2:14] <- x[16, 2:14]
        x[17, 2:3]  <- x[18, 1:2]
        
        x[19, 2:14] <- x[19, 1:13]
        x[19, 1]    <- x[20, 1]
        x[20, 2:14] <- x[19, 2:14]
        x[20, 2:3]  <- x[21, 1:2]
        
        x[c(1:11, 13:17, 19:20), ]
      } 
    )()
  
  pdfs[[17]] <- pdfs[[17]] |>
    (\(x)
      {
        x[3, 2:14]   <- x[3, 1:13]
        x[3, 1]      <- x[4, 1]
        x[4, 2:14]   <- x[3, 2:14]
        x[4, 2:3]    <- x[5, 1:2]
        
        x[13, 2:14]  <- x[13, 1:13]
        x[13, 1]     <- x[14, 1]
        x[14, 2:14]  <- x[3, 2:14]
        x[14, 2:3]   <- x[15, 1:2]
        
        x[16, 2:14]  <- x[16, 1:13]
        x[16, 1]     <- x[17, 1]
        x[17, 2:14]  <- x[16, 2:14]
        x[17, 2:3]   <- x[18, 1:2]
        
        x[19, 2:14]  <- x[19, 1:13]
        x[19, 1]     <- x[20, 1]
        x[20, 8:10]  <- x[20, 4:6]
        x[20, 4:7]   <- x[19, 4:7]
        x[20, 11:14] <- x[19, 11:14]
        x[21, 8:10]  <- x[21, 1:3]
        x[21, 1:7]   <- x[19, 1:7]
        x[21, 11:14] <- x[19, 11:14]
        
        x[c(1:4, 6:14, 16:17, 19:21), ]
      } 
    )()
  
  pdfs[[19]] <- pdfs[[19]] |>
    (\(x)
      {
        x[1, 2:14]   <- x[1, 1:13]
        x[1, 1]      <- x[2, 1]
        x[2, 2:14]   <- x[1, 2:14]
        x[2, 2:3]    <- x[3, 1:2]
        
        x[15, 2:14]  <- x[15, 1:13]
        x[15, 1]     <- x[16, 1]
        x[16, 8:10]  <- x[16, 4:6]
        x[16, 4:7]   <- x[15, 4:7]
        x[16, 11:14] <- x[15, 11:14]
        x[17, 8:10]  <- x[17, 1:3]
        x[17, 1:7]   <- x[15, 1:7]
        x[17, 11:14] <- x[15, 11:14]
        
        x[c(1:2, 4:17), ]
      }
    )()
  
  pdfs[[20]] <- pdfs[[20]] |>
    (\(x)
      {
        x[1, 2:14]   <- x[1, 1:13]
        x[1, 1]      <- x[2, 1]
        x[2, 2:14]   <- x[1, 2:14]
        x[2, 8:10]   <- x[3, 1:3]
        
        x[4, 2:14]   <- x[4, 1:13]
        x[4, 1]      <- x[5, 1]
        x[5, 2:14]   <- x[4, 2:14]
        x[5, 8:10]   <- x[6, 1:3]
        
        x[11, 2:14]  <- x[11, 1:13]
        x[11, 1]     <- x[12, 1]
        x[12, 2:14]  <- x[11, 2:14]
        x[12, 8:10]  <- x[13, 1:3]
        
        x[14, 2:14]  <- x[14, 1:13]
        x[14, 1]     <- x[15, 1]
        x[15, 2:14]  <- x[14, 2:14]
        x[15, 4:5]   <- x[16, 1:2]
        
        x[18, 3]     <- paste0(x[18, 3], x[18, 4])
        x[18, 4:12]  <- x[18, 5:13]
        x[18, 13:14] <- stringr::str_split(x[18, 14], pattern = "\\s+") |> unlist()
        
        x[c(1:2, 4:5, 7:12, 14:15, 17:21), ]
      } 
    )()
  
  pdfs[[22]] <- pdfs[[22]] |>
    (\(x)
      {
        x[1, 2:14]   <- x[1, 1:13]
        x[1, 1]      <- x[2, 1]
        x[2, 2:14]   <- x[1, 2:14]
        x[2, 2:3]    <- x[3, 1:2]
        
        x[10, 2:14]  <- x[10, 1:13]
        x[10, 1]     <- x[11, 1]
        x[11, 2:14]  <- x[10, 2:14]
        x[11, 8:10]  <- x[12, 1:3]
        
        x[16, 2:14]  <- x[16, 1:13]
        x[16, 1]     <- x[17, 1]
        x[17, 2:14]  <- x[16, 2:14]
        x[17, 2:3]   <- x[18, 1:2]

        x[c(1:2, 4:11, 13:17, 19), ]
      }
    )()
  
  pdfs[[23]] <- pdfs[[23]] |>
    (\(x)
      {
        x[13, 2:14]   <- x[13, 1:13]
        x[13, 1]      <- x[14, 1]
        x[14, 2:14]   <- x[13, 2:14]
        x[14, 8:10]   <- x[15, 1:3]
        
        x[1:14, ]
      }
    )()
  
  pdfs[[24]] <- pdfs[[24]] |>
    (\(x)
      {
        x[2, 2:14]   <- x[2, 1:13]
        x[2, 1]      <- x[3, 1]
        x[3, 2:14]   <- x[2, 2:14]
        x[3, 4:5]    <- x[4, 1:2]
        
        x[c(1:3, 5:15), ]
      }
    )()
    
  pdfs[[25]] <- pdfs[[25]] |>
    (\(x)
      {
        x[2, 2:14]   <- x[2, 1:13]
        x[2, 1]      <- x[3, 1]
        x[3, 2:14]   <- x[1, 2:14]
        x[3, 2:3]    <- x[4, 1:2]
        
        x[8, 2:14]   <- x[8, 1:13]
        x[8, 1]      <- x[9, 1]
        x[9, 2:14]   <- x[8, 2:14]
        x[9, 2:3]    <- x[10, 1:2]
        
        x[c(1:3, 5:9, 11:17), ]
      }
    )()
  
  pdfs[[26]] <- pdfs[[26]] |>
    (\(x)
      {
        x[1, 2:14]   <- x[1, 1:13]
        x[1, 1]      <- x[2, 1]
        x[2, 2:14]   <- x[1, 2:14]
        x[2, 13:14]  <- x[3, 1:2]
        
        x[4, 2:14]   <- x[4, 1:13]
        x[4, 1]      <- x[5, 1]
        x[5, 2:14]   <- x[4, 2:14]
        x[5, 4:5]    <- x[6, 1:2]
        
        x[7, 2:14]   <- x[7, 1:13]
        x[7, 1]      <- x[8, 1]
        x[8, 2:14]   <- x[7, 2:14]
        x[8, 8:10]   <- x[9, 1:3]
        
        x[10, 2:14]  <- x[10, 1:13]
        x[10, 1]     <- x[11, 1]
        x[11, 2:14]  <- x[10, 2:14]
        x[11, 2:3]   <- x[12, 1:2]
        
        x[15, 2:14]  <- x[15, 1:13]
        x[15, 1]     <- x[16, 1]
        x[16, 2:14]  <- x[15, 2:14]
        x[16, 4:14]  <- x[15, 4:14]
        x[17, 2:3]   <- x[17, 1:2]
        x[17, 1]     <- x[15, 1]
        x[17, 4:14]  <- x[15, 4:14]

        x[18, 2:14]  <- x[18, 1:13]
        x[18, 1]     <- x[19, 1]
        x[19, 2:14]  <- x[18, 2:14]
        x[19, 8:10]  <- x[20, 1:3]
        
        x[c(1:2, 4:5, 7:8, 10:11, 13:19, 21:25), ]
      }
    )()
  
  pdfs[[27]] <- pdfs[[27]] |>
    (\(x)
      {
        x[2, 2:14]   <- x[2, 1:13]
        x[2, 1]      <- x[3, 1]
        x[3, 2:14]   <- x[2, 2:14]
        x[3, 8:10]   <- x[4, 1:3]
        
        x[5, 2:14]   <- x[5, 1:13]
        x[5, 1]      <- x[6, 1]
        x[6, 2:14]   <- x[5, 2:14]
        x[6, 2:3]    <- x[7, 1:2]
        
        x[12, 2:14]  <- x[12, 1:13]
        x[12, 1]     <- x[13, 1]
        x[13, 2:14]  <- x[12, 2:14]
        x[13, 8:10]  <- x[14, 1:3]
        
        x[16, 2:14]  <- x[16, 1:13]
        x[16, 1]     <- x[17, 1]
        x[17, 2:14]  <- x[16, 2:14]
        x[17, 2:3]   <- x[18, 1:2]
        
        x[c(1:3, 5:6, 8:13, 15:17, 19:21), ]
      }
    )()
  
  pdfs[[28]] <- pdfs[[28]] |>
    (\(x)
      {
        x[8, 2:14]   <- x[8, 1:13]
        x[8, 1]      <- x[9, 1]
        x[9, 4:5]    <- x[9, 2:3]
        x[9, 2:3]    <- x[8, 2:3]
        x[9, 6:14]   <- x[8, 6:14]
        x[10, 4:5]   <- x[10, 1:2]
        x[10, 1:3]   <- x[8, 1:3]
        x[10, 6:14]  <- x[8, 6:14]
        
        x
      }
    )()
  
  pdfs[[31]] <- pdfs[[31]] |>
    (\(x)
      {
        x[1:9, 12:14] <- x[1:9, 11:13]
        x[10, 11:13]  <- x[10, 10:12]
        
        x[10, 2:14]   <- x[10, 1:13]
        x[10, 1]      <- x[11, 1]
        x[11, 2:14]   <- x[10, 2:14]
        x[11, 11:12]  <- x[12, 1:2]
        
        x[1:10, 11]   <- stringr::str_remove_all(
          string = x[1:10, 10], pattern = "[0-9]{2}-[0-9]{2}-[0-9]{4}"
        )
        
        x[1:11, 10]   <- stringr::str_extract_all(
          string = x[1:11, 10], pattern = "[0-9]{2}-[0-9]{2}-[0-9]{4}", 
          simplify = TRUE
        )
        
        x[c(1:11, 13:15), ]
      }
    )()
  
  pdfs[[33]] <- pdfs[[33]] |>
    (\(x)
      {
        x[1, 2:14]   <- x[1, 1:13]
        x[1, 1]      <- x[2, 1]
        x[2, 2:14]   <- x[1, 2:14]
        x[2, 4:5]    <- x[3, 1:2]
        
        x[c(1:2, 4:15), ]
      }
    )()
  
  pdfs[[34]] <- pdfs[[34]] |>
    (\(x)
      {
        x[1, 2:14]   <- x[1, 1:13]
        x[1, 1]      <- x[2, 1]
        x[2, 2:14]   <- x[1, 2:14]
        x[2, 8:10]   <- x[3, 1:3]
        
        x[4, 2:14]   <- x[4, 1:13]
        x[4, 1]      <- x[5, 1]
        x[5, 8:10]   <- x[5, 2:4]
        x[5, 2:7]    <- x[4, 2:7]
        x[5, 11:14]  <- x[4, 11:14]
        x[6, 8:10]   <- x[6, 1:3]
        x[6, 1:7]    <- x[4, 1:7]
        x[6, 11:14]  <- x[4, 11:14]
        
        x[11, 2:14]  <- x[11, 1:13]
        x[11, 1]     <- x[12, 1]
        x[12, 2:14]  <- x[11, 2:14]
        x[12, 4:5]   <- x[13, 1:2]
        
        x[16, 2:14]  <- x[16, 1:13]
        x[16, 1]     <- x[17, 1]
        x[17, 2:14]  <- x[16, 2:14]
        x[17, 4:5]   <- x[18, 1:2]
        
        x[c(1:2, 4:12, 14:17, 19:21), ]
      } 
    )()
  
  pdfs[[36]] <- pdfs[[36]] |>
    (\(x)
      {
        x[7, 2:14]   <- x[7, 1:13]
        x[7, 1]      <- x[8, 1]
        x[8, 2:14]   <- x[7, 2:14]
        x[8, 8:10]   <- x[9, 1:3]
        
        x[13, 2:14]  <- x[13, 1:13]
        x[13, 1]     <- x[14, 1]
        x[14, 2:14]  <- x[13, 2:14]
        x[14, 2:3]   <- x[15, 1:2]
        
        x[17, 2:14]  <- x[17, 1:13]
        x[17, 1]     <- x[18, 1]
        x[18, 2:14]  <- x[17, 2:14]
        x[18, 11:12] <- x[19, 1:2]
        
        x[c(1:8, 10:14, 16:18), ]
        
      }
    )()
  
  pdfs[[37]] <- pdfs[[37]] |>
    structure_table_double(row_index = 7:9, col_index = 2:3) |>
    structure_table_triple(row_index = 10:12, col_index = 4:5) |>
    structure_table_double(row_index = 17:19, col_index = 8:10) |>
    remove_table_rows()
  
  pdfs[[38]] <- pdfs[[38]] |>
    structure_table_double(row_index = 4:6, col_index = 4:5) |>
    remove_table_rows()
  
  pdfs[[40]] <- pdfs[[40]] |>
    (\(x)
      {
        x[14, 8:10]   <- x[14, 1:3]
        x[14, 1:7]    <- x[13, 1:7]
        x[14, 11:14]  <- x[13, 11:14]
        
        x
      }
    )()
  
  pdfs[[42]] <- pdfs[[42]] |>
    structure_table_double(row_index = 8:10, col_index = c(2:3, 8:10)) |>
    structure_table_double(row_index = 14:16, col_index = 2:3) |>
    remove_table_rows()
  
  pdfs[[43]] <- pdfs[[43]] |>
    structure_table_double(row_index = 13:15, col_index = 2:3) |>
    remove_table_rows()
  
  pdfs[[44]] <- pdfs[[44]] |>
    structure_table_double(row_index = 8:10, col_index = 11:12) |>
    remove_table_rows()
  
  pdfs[[45]] <- pdfs[[45]] |>
    structure_table_double(row_index = 4:6, col_index = 2:3) |>
    structure_table_triple(row_index = 7:9, col_index = 2:3) |>
    structure_table_double(row_index = 14:16, col_index = 4:5) |>
    remove_table_rows()
  
  pdfs[[46]] <- pdfs[[46]] |>
    structure_table_double(row_index = 9:11, col_index = 2:3) |>
    remove_table_rows()
  
  pdfs[[47]] <- pdfs[[47]] |>
    structure_table_double(row_index = 4:6, col_index = 11:12) |>
    (\(x)
      {
        x[16, 14]   <- x[15, 1]
        x[17, 14]   <- x[17, 1]
        x[17, 1:13] <- x[16, 1:13]
        
        x
      }
    )() |>
    remove_table_rows()
  
  pdfs[[49]] <- pdfs[[49]] |>
    structure_table_double(row_index = 12:14, col_index = 8:10) |>
    structure_table_double(row_index = 15:17, col_index = 2:3) |>
    remove_table_rows()
  
  pdfs[[51]] <- pdfs[[51]] |>
    structure_table_double(row_index = 1:3, col_index = 11:12) |>
    structure_table_double(row_index = 5:7, col_index = 2:3) |>
    structure_table_triple(row_index = 8:10, col_index = 4:5) |>
    structure_table_double(row_index = 12:14, col_index = 3) |>
    structure_table_triple(row_index = 19:21, col_index = 2:5) |>
    structure_table_double(row_index = 23:25, col_index = 4:5) |>
    (\(x)
      {
        x[20, 5] <- "11-18-2005"
        x
      }
    )() |>
    remove_table_rows()
    
  pdfs[[52]] <- pdfs[[52]] |>
    structure_table_double(row_index = 1:3, col_index = 2:3) |>
    structure_table_double(row_index = 8:10, col_index = 2:5) |>
    structure_table_double(row_index = 12:14, col_index = 2:3) |>
    structure_table_double(row_index = 19:21, col_index = 2:3) |>
    remove_table_rows()
  
  pdfs[[53]] <- pdfs[[53]] |>
    structure_table_double(row_index = 13:15, col_index = 2:5) |>
    remove_table_rows()
  
  pdfs[[54]] <- pdfs[[54]] |>
    (\(x)
      {
        x[14, 11:12] <- x[14, 1:2]
        x[14, 1:10]  <- x[13, 1:10]
        x[14, 13:14] <- x[13, 13:14]
        
        x
      }
    )()
  
  pdfs[[57]] <- pdfs[[57]] |>
    structure_table_double(row_index = 5:7, col_index = 11:12) |>
    structure_table_double(row_index = 12:14, col_index = 11:12) |>
    structure_table_double(row_index = 17:19, col_index = 8:10) |>
    remove_table_rows()
  
  ## Final processing/restructuring/concatenating of climate extremes data ----
  structure_climate_data(pdfs)
}


#'
#' @rdname climate_process
#' @export
#'

climate_process_2021 <- function(climate_download_files) {
  ## Get vector of file paths for 2021 PDFS ----
  pdf_path <- climate_download_files |>
    (\(x) x[stringr::str_detect(string = x, pattern = "2021")])()
  
  ## Get station information ----
  station_df <- get_weather_station_info(
    climate_download_files, period = "2021"
  )
  
  ## Read text data from tables from each PDF ----
  pdfs <- lapply(
    X = pdf_path,
    FUN = function(x) {
      pdftools::pdf_text(x) |>
        stringr::str_split(pattern = "\n") |>
        unlist() |>
        (\(x)
         {
           index <- seq_len(length(x))
           index <- index[stringr::str_detect(string = x, pattern = "MONTH|Period")]
           index <- c(index[1] + 2, index[2] - 1)
           x[index[1]:index[2]]
        }
        )() |>
        stringr::str_trim() |>
        stringr::str_split_fixed(pattern = "\\s+", n = 14)
    }
  ) |>
    (\(x)
     {
       names(x) <- paste0(station_df$station, "_", station_df$period)
       x
    }
    )()
  
  ## Process each PDF table with multiple rows per month ----
  pdfs[[1]] <- pdfs[[1]] |>
    structure_table_double(row_index = 9:11, col_index = 13:14) |>
    remove_table_rows()
  
  pdfs[[2]] <- pdfs[[2]] |>
    structure_table_double(row_index = 1:3, col_index = 4:5) |>
    structure_table_double(row_index = 4:6, col_index = 11:12) |>
    structure_table_double(row_index = 11:13, col_index = 2:3) |>
    structure_table_double(row_index = 17:19, col_index = 4:5) |>
    structure_table_double(row_index = 21:23, col_index = 4:5) |>
    remove_table_rows()
    
  pdfs[[3]] <- pdfs[[3]] |>
    structure_table_single(row_index = 13:14, col_index = 11:12)
  
  pdfs[[4]] <- pdfs[[4]] |>
    structure_table_double(row_index = 6:8, col_index = 5) |>
    remove_table_rows()

  pdfs[[5]] <- pdfs[[5]] |>
    structure_table_double(row_index = 5:7, col_index = 11:12) |>
    structure_table_double(row_index = 10:12, col_index = 8:10) |>
    structure_table_double(row_index = 14:16, col_index = 2:3) |>
    remove_table_rows()
  
  pdfs[[6]] <- pdfs[[6]] |>
    structure_table_double(row_index = 7:9, col_index = 11:12) |>
    remove_table_rows()
  
  pdfs[[7]] <- pdfs[[7]] |>
    structure_table_triple(row_index = 2:4, col_index = 8:10) |>
    structure_table_double(row_index = 5:7, col_index = 8:10) |>
    structure_table_double(row_index = 12:14, col_index = 11:12) |>
    structure_table_double(row_index = 19:21, col_index = 2:3) |>
    remove_table_rows()

  
  pdfs[[8]] <- pdfs[[8]] |>
    structure_table_double(row_index = 6:8, col_index = 8:10) |>
    structure_table_double(row_index = 15:17, col_index = c(2:5, 8:10)) |>
    remove_table_rows()

  
  pdfs[[9]] <- pdfs[[9]] |>
    structure_table_double(row_index = 2:4, col_index = 13:14) |>
    structure_table_double(row_index = 10:12, col_index = 2:3) |>
    structure_table_double(row_index = 17:19, col_index = 8:10) |>
    remove_table_rows()

  
  pdfs[[10]] <- pdfs[[10]] |>
    structure_table_double(row_index = 13:15, col_index = 11:12) |>
    remove_table_rows()

  
  pdfs[[11]] <- pdfs[[11]] |>
    structure_table_triple(row_index = 4:6, col_index = 8:10) |>
    structure_table_triple(row_index = 14:16, col_index = 8:10) |>
    remove_table_rows()
    
  pdfs[[13]] <- pdfs[[13]] |>
    structure_table_double(row_index = 7:9, col_index = 11:12) |>
    remove_table_rows()
    
  pdfs[[14]] <- pdfs[[14]] |>
    structure_table_double(row_index = 1:3, col_index = 8:10) |>
    structure_table_double(row_index = 6:8, col_index = 2:3) |>
    structure_table_quadruple(row_index = 12:16, col_index = 2:3) |>
    structure_table_double(row_index = 19:21, col_index = 2:3) |>
    structure_table_double(row_index = 23:25, col_index = 11:12) |>
    remove_table_rows()
    
  pdfs[[15]] <- pdfs[[15]] |>
    structure_table_single(row_index = 13:14, col_index = 2:3) |>
    remove_table_rows()

  pdfs[[16]] <- pdfs[[16]] |>
    structure_table_triple(row_index = 7:9, col_index = 8:10) |>
    structure_table_double(row_index = 10:12, col_index = 8:10) |>
    structure_table_double(row_index = 16:18, col_index = 2:3) |>
    structure_table_double(row_index = 19:21, col_index = 2:3) |>
    remove_table_rows()

  pdfs[[17]] <- pdfs[[17]] |>
    structure_table_double(row_index = 3:5, col_index = 2:3) |>
    structure_table_double(row_index = 12:14, col_index = 2:3) |>
    structure_table_double(row_index = 15:17, col_index = 2:3) |>
    structure_table_double(row_index = 18:20, col_index = 2:3) |>
    (\(x)
      {
        x[21, 2:14]   <- x[21, 1:13]
        x[21, 1]      <- x[22, 1]
        x[22, 8:10]   <- x[22, 4:6]
        x[22, 4:7]    <- x[21, 4:7]
        x[22, 11:14]  <- x[21, 11:14]
        x[23, 8:10]   <- x[23, 1:3]
        x[23, 1:7]    <- x[21, 1:7]
        x[23, 11:14]  <- x[21, 11:14]
        
        x
      }
    )() |>
    remove_table_rows()
    
  pdfs[[19]] <- pdfs[[19]] |>
    structure_table_double(row_index = 1:3, col_index = 2:3) |>
    (\(x)
      {
        x[15, 2:14]   <- x[15, 1:13]
        x[15, 1]      <- x[16, 1]
        x[16, 8:10]   <- x[16, 4:6]
        x[16, 4:5]    <- x[16, 2:3]
        x[16, 2:3]    <- x[15, 2:3]
        x[16, 6:7]    <- x[15, 6:7]
        x[16, 11:14]  <- x[15, 11:14]
        x[17, 8:10]   <- x[17, 1:3]
        x[17, 1:7]    <- x[15, 1:7]
        x[17, 11:14]  <- x[15, 11:14]
        
        x
      }
    )() |>
    remove_table_rows()
  
  pdfs[[20]] <- pdfs[[20]] |>
    structure_table_double(row_index = 1:3, col_index = 8:10) |>
    structure_table_double(row_index = 4:6, col_index = 8:10) |>
    structure_table_double(row_index = 11:13, col_index = 8:10) |>
    structure_table_double(row_index = 14:16, col_index = 4:5) |>
    (\(x)
      {
        x[18, 3]     <- paste0(x[18, 3], x[18, 4])
        x[18, 4:12]  <- x[18, 5:13]
        x[18, 13:14] <- stringr::str_split(x[18, 14], pattern = "\\s+", simplify = TRUE)
        
        x
      }
    )() |>
    remove_table_rows()
    
  pdfs[[22]] <- pdfs[[22]] |>
    structure_table_double(row_index = 1:3, col_index = 2:3) |>
    structure_table_double(row_index = 10:12, col_index = 8:10) |>
    structure_table_double(row_index = 16:18, col_index = 2:3) |>
    remove_table_rows()

  pdfs[[23]] <- pdfs[[23]] |>
    structure_table_double(row_index = 13:15, col_index = 8:10) |>
    remove_table_rows()
    
  pdfs[[24]] <- pdfs[[24]] |>
    structure_table_double(row_index = 2:4, col_index = 4:5) |>
    structure_table_single(row_index = 15:16, col_index = 8:10) |>
    remove_table_rows()
  
  pdfs[[25]] <- pdfs[[25]] |>
    structure_table_double(row_index = 2:4, col_index = 2:3) |>
    structure_table_double(row_index = 7:9, col_index = 8:10) |>
    structure_table_double(row_index = 10:12, col_index = 2:3) |>
    remove_table_rows()
    
  pdfs[[26]] <- pdfs[[26]] |>
    structure_table_double(row_index = 1:3, col_index = 13:14) |>
    structure_table_double(row_index = 4:6, col_index = 4:5) |>
    structure_table_double(row_index = 7:9, col_index = 8:10) |>
    structure_table_double(row_index = 10:12, col_index = 2:3) |>
    structure_table_triple(row_index = 15:17, col_index = 2:3) |>
    structure_table_double(row_index = 18:20, col_index = 8:10) |>
    remove_table_rows()

  
  pdfs[[27]] <- pdfs[[27]] |>
    structure_table_double(row_index = 2:4, col_index = 8:10) |>
    structure_table_double(row_index = 5:7, col_index = 2:3) |>
    structure_table_double(row_index = 11:13, col_index = 2:3) |>
    structure_table_double(row_index = 14:16, col_index = 8:10) |>
    structure_table_single(row_index = 21:22, col_index = 2:3) |>
    remove_table_rows()

  pdfs[[28]] <- pdfs[[28]] |>
    structure_table_triple(row_index = 8:10, col_index = 4:5)
    
  pdfs[[31]] <- pdfs[[31]] |>
    structure_table_triple(row_index = 10:12, col_index = c(2:3, 11:12)) |>
    (\(x)
      {
        x[12, 11:12] <- x[10, 11:12]
        x
      } 
    )()

  pdfs[[33]] <- pdfs[[33]] |>
    structure_table_double(row_index = 1:3, col_index = 4:5) |>
    remove_table_rows()
  
  pdfs[[34]] <- pdfs[[34]] |>
    structure_table_double(row_index = 1:3, col_index = 8:10) |>
    structure_table_triple(row_index = 4:6, col_index = 8:10) |>
    structure_table_double(row_index = 11:13, col_index = 4:5) |>
    structure_table_double(row_index = 14:16, col_index = 2:3) |>
    structure_table_double(row_index = 18:20, col_index = 4:5) |>
    remove_table_rows()
    
  pdfs[[36]] <- pdfs[[36]] |>
    structure_table_triple(row_index = 7:9, col_index = 8:10) |>
    structure_table_double(row_index = 13:15, col_index = 2:3) |>
    structure_table_double(row_index = 17:19, col_index = 11:12) |>
    remove_table_rows()

  pdfs[[37]] <- pdfs[[37]] |>
    structure_table_triple(row_index = 8:10, col_index = 4:5) |>
    structure_table_double(row_index = 15:17, col_index = 8:10) |>
    remove_table_rows()
  
  pdfs[[38]] <- pdfs[[38]] |>
    structure_table_double(row_index = 4:6, col_index = 4:5) |>
    remove_table_rows()
  
  pdfs[[40]] <- pdfs[[40]] |>
    structure_table_single(row_index = 13:14, col_index = 8:10)
  
  pdfs[[42]] <- pdfs[[42]] |>
    structure_table_double(row_index = 8:10, col_index = c(2:3, 8:10)) |>
    structure_table_double(row_index = 14:16, col_index = 2:3) |>
    remove_table_rows()
  
  pdfs[[43]] <- pdfs[[43]] |>
    structure_table_double(row_index = 13:15, col_index = 2:3) |>
    remove_table_rows()
  
  pdfs[[44]] <- pdfs[[44]] |>
    structure_table_double(row_index = 8:10, col_index = 11:12) |>
    remove_table_rows()
  
  pdfs[[45]] <- pdfs[[45]] |>
    structure_table_double(row_index = 1:3, col_index = 8:10) |>
    structure_table_quadruple(row_index = 5:9, col_index = 4:5) |>
    structure_table_double(row_index = 10:12, col_index = 2:3) |>
    structure_table_triple(row_index = 13:15, col_index = 2:3) |>
    structure_table_double(row_index = 20:22, col_index = 4:5) |>
    structure_table_double(row_index = 24:26, col_index = 2:3) |>
    remove_table_rows()
  
  pdfs[[46]] <- pdfs[[46]] |>
    structure_table_double(row_index = 9:11, col_index = 2:3) |>
    remove_table_rows()
  
  pdfs[[47]] <- pdfs[[47]] |>
    structure_table_double(row_index = 4:6, col_index = 11:12) |>
    structure_table_double(row_index = 12:14, col_index = 11:12) |>
    (\(x)
     {
       x[17, 14]   <- x[17, 1]
       x[17, 1:13] <- x[18, 1:13]
       x[18, 14]   <- x[19, 1]
       
       x
    }
    )() |>
    remove_table_rows()
  
  pdfs[[48]] <- pdfs[[48]] |>
    structure_table_double(row_index = 7:9, col_index = 4:5) |>
    structure_table_single(row_index = 15:16, col_index = 4:5) |>
    remove_table_rows()
  
  pdfs[[49]] <- pdfs[[49]] |>
    structure_table_double(row_index = 12:14, col_index = 8:10) |>
    structure_table_double(row_index = 15:17, col_index = 2:3) |>
    remove_table_rows()
  
  pdfs[[51]] <- pdfs[[51]] |>
    structure_table_double(row_index = 1:3, col_index = 11:12) |>
    structure_table_double(row_index = 5:7, col_index = 2:3) |>
    structure_table_triple(row_index = 8:10, col_index = 4:5) |>
    structure_table_double(row_index = 12:14, col_index = 3) |>
    structure_table_triple(row_index = 19:21, col_index = 2:5) |>
    structure_table_double(row_index = 23:25, col_index = 4:5) |>
    (\(x)
     {
       x[21, 4:5] <- c("15.0", "11-18-2005")
       x
    }
    )() |>
    remove_table_rows()
  
  pdfs[[52]] <- pdfs[[52]] |>
    structure_table_double(row_index = 6:8, col_index = 2:5) |>
    structure_table_double(row_index = 10:12, col_index = 2:3) |>
    structure_table_double(row_index = 17:19, col_index = 2:3) |>
    remove_table_rows()
  
  pdfs[[53]] <- pdfs[[53]] |>
    structure_table_double(row_index = 13:15, col_index = 2:5) |>
    remove_table_rows()
  
  pdfs[[54]] <- pdfs[[54]] |>
    structure_table_single(row_index = 13:14, col_index = 11:12)

  pdfs[[57]] <- pdfs[[57]] |>
    structure_table_double(row_index = 5:7, col_index = 2:3) |>
    structure_table_double(row_index = 9:11, col_index = 2:3) |>
    structure_table_double(row_index = 12:14, col_index = 2:3) |>
    structure_table_double(row_index = 16:18, col_index = 11:12) |>
    (\(x)
      {
        x[20, 2:3]  <- x[20, 1:2]
        x[20, 1]    <- x[19, 1]
        x[20, 4:14] <- x[19, 4:14]
        x[21, 2:3]  <- x[21, 1:2]
        x[21, 1]    <- x[19, 1]
        x[21, 4:14] <- x[19, 4:14]
        
        x
      }
    )() |>
    structure_table_double(row_index = 23:25, col_index = 8:10) |>
    remove_table_rows()
  
  ## Final processing/structuring/concatenating of climate data ----
  structure_climate_data(pdfs)
}


#'
#' @rdname climate_process
#' @export
#'

climate_process_2022 <- function(climate_download_files) {
  ## Get vector of file paths for 2021 PDFS ----
  pdf_path <- climate_download_files |>
    (\(x) x[stringr::str_detect(string = x, pattern = "2022")])()
  
  ## Get weather station information ----
  station_df <- get_weather_station_info(
    climate_download_files, period = "2022"
  )
  
  ## Read text data from tables from each PDF ----
  pdfs <- lapply(
    X = pdf_path,
    FUN = function(x) {
      pdftools::pdf_text(x) |>
        stringr::str_split(pattern = "\n") |>
        unlist() |>
        (\(x)
         {
           index <- seq_len(length(x))
           index <- index[stringr::str_detect(string = x, pattern = "MONTH|Period")]
           index <- c(index[1] + 2, index[2] - 1)
           x[index[1]:index[2]]
        }
        )() |>
        stringr::str_trim() |>
        stringr::str_split_fixed(pattern = "\\s+", n = 14)
    }
  ) |>
    (\(x)
     {
       names(x) <- paste0(station_df$station, "_", station_df$period)
       x
    }
    )()
  
  ## Process each PDF table with multiple rows per month ----
  pdfs[[1]] <- pdfs[[1]] |>
    structure_table_double(row_index = 9:11, col_index = 13:14) |>
    remove_table_rows()
  
  pdfs[[2]] <- pdfs[[2]] |>
    structure_table_double(row_index = 1:3, col_index = 4:5) |>
    structure_table_double(row_index = 4:6, col_index = 11:12) |>
    structure_table_double(row_index = 11:13, col_index = 2:3) |>
    structure_table_double(row_index = 17:19, col_index = 4:5) |>
    structure_table_double(row_index = 21:23, col_index = 4:5) |>
    remove_table_rows()
  
  pdfs[[3]] <- pdfs[[3]] |>
    structure_table_single(row_index = 13:14, col_index = 11:12)
  
  pdfs[[4]] <- pdfs[[4]] |>
    structure_table_double(row_index = 6:8, col_index = 5) |>
    remove_table_rows()
  
  pdfs[[5]] <- pdfs[[5]] |>
    structure_table_double(row_index = 5:7, col_index = 11:12) |>
    structure_table_double(row_index = 10:12, col_index = 8:10) |>
    structure_table_double(row_index = 14:16, col_index = 2:3) |>
    remove_table_rows()
  
  pdfs[[6]] <- pdfs[[6]] |>
    structure_table_double(row_index = 7:9, col_index = 11:12) |>
    remove_table_rows()
  
  pdfs[[7]] <- pdfs[[7]] |>
    structure_table_triple(row_index = 2:4, col_index = 8:10) |>
    structure_table_double(row_index = 5:7, col_index = 8:10) |>
    structure_table_double(row_index = 12:14, col_index = 11:12) |>
    structure_table_double(row_index = 19:21, col_index = 2:3) |>
    remove_table_rows()
  
  
  pdfs[[8]] <- pdfs[[8]] |>
    structure_table_double(row_index = 6:8, col_index = 8:10) |>
    structure_table_double(row_index = 15:17, col_index = c(2:5, 8:10)) |>
    remove_table_rows()
  
  
  pdfs[[9]] <- pdfs[[9]] |>
    structure_table_double(row_index = 2:4, col_index = 13:14) |>
    structure_table_double(row_index = 10:12, col_index = 2:3) |>
    structure_table_double(row_index = 17:19, col_index = 8:10) |>
    remove_table_rows()
  
  
  pdfs[[10]] <- pdfs[[10]] |>
    structure_table_double(row_index = 13:15, col_index = 11:12) |>
    remove_table_rows()
  
  
  pdfs[[11]] <- pdfs[[11]] |>
    structure_table_triple(row_index = 12:14, col_index = 8:10) |>
    remove_table_rows()
  
  pdfs[[13]] <- pdfs[[13]] |>
    structure_table_double(row_index = 7:9, col_index = 11:12) |>
    remove_table_rows()
  
  pdfs[[14]] <- pdfs[[14]] |>
    structure_table_double(row_index = 1:3, col_index = 8:10) |>
    structure_table_double(row_index = 6:8, col_index = 2:3) |>
    structure_table_quadruple(row_index = 12:16, col_index = 2:3) |>
    structure_table_double(row_index = 19:21, col_index = 2:3) |>
    structure_table_double(row_index = 23:25, col_index = 11:12) |>
    remove_table_rows()
  
  pdfs[[15]] <- pdfs[[15]] |>
    structure_table_single(row_index = 13:14, col_index = 2:3) |>
    remove_table_rows()
  
  pdfs[[16]] <- pdfs[[16]] |>
    structure_table_double(row_index = 8:10, col_index = 8:10) |>
    structure_table_double(row_index = 14:16, col_index = 2:3) |>
    structure_table_double(row_index = 17:19, col_index = 2:3) |>
    remove_table_rows()
  
  pdfs[[17]] <- pdfs[[17]] |>
    structure_table_double(row_index = 3:5, col_index = 2:3) |>
    structure_table_double(row_index = 6:8, col_index = 8:10) |>
    structure_table_double(row_index = 14:16, col_index = 2:3) |>
    structure_table_double(row_index = 17:19, col_index = 2:3) |>
    structure_table_double(row_index = 20:22, col_index = 2:3) |>
    (\(x)
     {
       x[23, 2:14]   <- x[23, 1:13]
       x[23, 1]      <- x[24, 1]
       x[24, 8:10]   <- x[24, 4:6]
       x[24, 4:7]    <- x[23, 4:7]
       x[24, 11:14]  <- x[23, 11:14]
       x[25, 8:10]   <- x[25, 1:3]
       x[25, 1:7]    <- x[23, 1:7]
       x[25, 11:14]  <- x[23, 11:14]
       
       x
    }
    )() |>
    remove_table_rows()
  
  pdfs[[19]] <- pdfs[[19]] |>
    structure_table_double(row_index = 1:3, col_index = 2:3) |>
    (\(x)
     {
       x[15, 2:14]   <- x[15, 1:13]
       x[15, 1]      <- x[16, 1]
       x[16, 8:10]   <- x[16, 4:6]
       x[16, 4:5]    <- x[16, 2:3]
       x[16, 2:3]    <- x[15, 2:3]
       x[16, 6:7]    <- x[15, 6:7]
       x[16, 11:14]  <- x[15, 11:14]
       x[17, 8:10]   <- x[17, 1:3]
       x[17, 1:7]    <- x[15, 1:7]
       x[17, 11:14]  <- x[15, 11:14]
       
       x
    }
    )() |>
    remove_table_rows()
  
  pdfs[[20]] <- pdfs[[20]] |>
    structure_table_double(row_index = 1:3, col_index = 8:10) |>
    structure_table_double(row_index = 4:6, col_index = 8:10) |>
    structure_table_double(row_index = 11:13, col_index = 8:10) |>
    structure_table_double(row_index = 14:16, col_index = 4:5) |>
    (\(x)
     {
       x[18, 3]     <- paste0(x[18, 3], x[18, 4])
       x[18, 4:12]  <- x[18, 5:13]
       x[18, 13:14] <- stringr::str_split(x[18, 14], pattern = "\\s+", simplify = TRUE)
       
       x
    }
    )() |>
    remove_table_rows()
  
  pdfs[[22]] <- pdfs[[22]] |>
    structure_table_double(row_index = 1:3, col_index = 2:3) |>
    structure_table_double(row_index = 10:12, col_index = 8:10) |>
    structure_table_double(row_index = 16:18, col_index = 2:3) |>
    remove_table_rows()
  
  pdfs[[23]] <- pdfs[[23]] |>
    structure_table_double(row_index = 13:15, col_index = 8:10) |>
    remove_table_rows()
  
  pdfs[[24]] <- pdfs[[24]] |>
    structure_table_double(row_index = 2:4, col_index = 4:5) |>
    structure_table_single(row_index = 15:16, col_index = 8:10) |>
    remove_table_rows()
  
  pdfs[[25]] <- pdfs[[25]] |>
    structure_table_double(row_index = 2:4, col_index = 2:3) |>
    structure_table_double(row_index = 7:9, col_index = 8:10) |>
    structure_table_double(row_index = 10:12, col_index = 2:3) |>
    remove_table_rows()
  
  pdfs[[26]] <- pdfs[[26]] |>
    structure_table_double(row_index = 1:3, col_index = 13:14) |>
    structure_table_double(row_index = 4:6, col_index = 4:5) |>
    structure_table_double(row_index = 7:9, col_index = 8:10) |>
    structure_table_double(row_index = 10:12, col_index = 2:3) |>
    structure_table_triple(row_index = 15:17, col_index = 2:3) |>
    structure_table_double(row_index = 18:20, col_index = 8:10) |>
    remove_table_rows()
  
  pdfs[[27]] <- pdfs[[27]] |>
    structure_table_double(row_index = 2:4, col_index = 8:10) |>
    structure_table_double(row_index = 5:7, col_index = 2:3) |>
    structure_table_double(row_index = 11:13, col_index = 2:3) |>
    structure_table_double(row_index = 14:16, col_index = 8:10) |>
    structure_table_single(row_index = 21:22, col_index = 2:3) |>
    remove_table_rows()
  
  pdfs[[28]] <- pdfs[[28]] |>
    structure_table_triple(row_index = 8:10, col_index = 4:5)
  
  pdfs[[31]] <- pdfs[[31]] |>
    structure_table_triple(row_index = 10:12, col_index = c(2:3, 11:12)) |>
    (\(x)
     {
       x[12, 11:12] <- x[10, 11:12]
       x
    } 
    )()
  
  pdfs[[33]] <- pdfs[[33]] |>
    structure_table_double(row_index = 1:3, col_index = 2:5) |>
    remove_table_rows()
  
  pdfs[[34]] <- pdfs[[34]] |>
    structure_table_double(row_index = 7:9, col_index = 4:5) |>
    structure_table_double(row_index = 10:12, col_index = 2:3) |>
    structure_table_double(row_index = 14:16, col_index = 2:3) |>
    remove_table_rows()
  
  pdfs[[36]] <- pdfs[[36]] |>
    structure_table_triple(row_index = 7:9, col_index = 8:10) |>
    structure_table_double(row_index = 13:15, col_index = 2:3) |>
    structure_table_double(row_index = 17:19, col_index = 11:12) |>
    remove_table_rows()
  
  pdfs[[37]] <- pdfs[[37]] |>
    structure_table_double(row_index = 1:3, col_index = 3) |>
    structure_table_triple(row_index = 10:12, col_index = 4:5) |>
    structure_table_double(row_index = 17:19, col_index = 8:10) |>
    remove_table_rows()
  
  pdfs[[38]] <- pdfs[[38]] |>
    structure_table_double(row_index = 4:6, col_index = 4:5) |>
    remove_table_rows()
  
  pdfs[[40]] <- pdfs[[40]] |>
    structure_table_single(row_index = 13:14, col_index = 8:10)
  
  pdfs[[42]] <- pdfs[[42]] |>
    structure_table_double(row_index = 8:10, col_index = c(2:3, 8:10)) |>
    structure_table_double(row_index = 14:16, col_index = 2:3) |>
    remove_table_rows()
  
  pdfs[[43]] <- pdfs[[43]] |>
    structure_table_double(row_index = 13:15, col_index = 2:3) |>
    remove_table_rows()
  
  pdfs[[44]] <- pdfs[[44]] |>
    structure_table_double(row_index = 8:10, col_index = 11:12) |>
    remove_table_rows()
  
  pdfs[[45]] <- pdfs[[45]] |>
    structure_table_double(row_index = 1:3, col_index = 8:10) |>
    structure_table_quadruple(row_index = 5:9, col_index = 4:5) |>
    structure_table_double(row_index = 10:12, col_index = 2:3) |>
    structure_table_triple(row_index = 13:15, col_index = 2:3) |>
    structure_table_double(row_index = 17:19, col_index = 4:5) |>
    structure_table_double(row_index = 21:23, col_index = 4:5) |>
    structure_table_double(row_index = 24:26, col_index = 4:5) |>
    structure_table_double(row_index = 28:30, col_index = 2:3) |>
    remove_table_rows()
  
  pdfs[[46]] <- pdfs[[46]] |>
    structure_table_double(row_index = 9:11, col_index = 2:3) |>
    remove_table_rows()
  
  pdfs[[47]] <- pdfs[[47]] |>
    structure_table_double(row_index = 4:6, col_index = 11:12) |>
    structure_table_double(row_index = 12:14, col_index = 11:12) |>
    (\(x)
     {
       x[17, 14]   <- x[17, 1]
       x[17, 1:13] <- x[18, 1:13]
       x[18, 14]   <- x[19, 1]
       
       x
    }
    )() |>
    remove_table_rows()
  
  pdfs[[48]] <- pdfs[[48]] |>
    structure_table_double(row_index = 7:9, col_index = 4:5) |>
    structure_table_single(row_index = 15:16, col_index = 4:5) |>
    remove_table_rows()
  
  pdfs[[49]] <- pdfs[[49]] |>
    structure_table_double(row_index = 12:14, col_index = 8:10) |>
    structure_table_double(row_index = 15:17, col_index = 2:3) |>
    remove_table_rows()
  
  pdfs[[51]] <- pdfs[[51]] |>
    structure_table_double(row_index = 1:3, col_index = 11:12) |>
    structure_table_double(row_index = 5:7, col_index = 2:3) |>
    structure_table_triple(row_index = 8:10, col_index = 4:5) |>
    structure_table_double(row_index = 12:14, col_index = 3) |>
    structure_table_triple(row_index = 19:21, col_index = 2:5) |>
    structure_table_double(row_index = 23:25, col_index = 4:5) |>
    (\(x)
     {
       x[21, 4:5] <- c("15.0", "11-18-2005")
       x
    }
    )() |>
    remove_table_rows()
  
  pdfs[[52]] <- pdfs[[52]] |>
    structure_table_double(row_index = 6:8, col_index = 2:5) |>
    structure_table_double(row_index = 10:12, col_index = 2:3) |>
    structure_table_double(row_index = 17:19, col_index = 2:3) |>
    remove_table_rows()
  
  pdfs[[53]] <- pdfs[[53]] |>
    structure_table_double(row_index = 13:15, col_index = 2:5) |>
    remove_table_rows()
  
  pdfs[[54]] <- pdfs[[54]] |>
    structure_table_single(row_index = 13:14, col_index = 11:12)
  
  pdfs[[57]] <- pdfs[[57]] |>
    structure_table_double(row_index = 5:7, col_index = 2:3) |>
    structure_table_double(row_index = 9:11, col_index = 2:3) |>
    structure_table_double(row_index = 12:14, col_index = 2:3) |>
    structure_table_double(row_index = 16:18, col_index = 11:12) |>
    structure_table_double(row_index = 21:23, col_index = 8:10) |>
    remove_table_rows()
  
  ## Final processing/structuring/concatenating of climate data ----
  structure_climate_data(pdfs)
}

#'
#' @rdname climate_process
#' @export
#'

climate_process_2023 <- function(climate_download_files) {
  ## Get vector of file paths for 2023 PDFs ----
  pdf_path <- climate_download_files |>
    (\(x) x[stringr::str_detect(string = x, pattern = "2023")])()
  
  ## Get weather station information ----
  station_df <- get_weather_station_info(
    climate_download_files, period = "2023"
  )
  
  ## Read text data from tables from each PDF ----
  pdfs <- lapply(
    X = pdf_path,
    FUN = function(x) {
      pdftools::pdf_text(x) |>
        stringr::str_split(pattern = "\n") |>
        unlist() |>
        (\(x)
         {
           index <- seq_len(length(x))
           index <- index[stringr::str_detect(string = x, pattern = "MONTH|Period")]
           index <- c(index[1] + 2, index[2] - 1)
           x[index[1]:index[2]]
        }
        )() |>
        stringr::str_trim() |>
        stringr::str_split_fixed(pattern = "\\s+", n = 14)
    }
  ) |>
    (\(x)
     {
       names(x) <- paste0(station_df$station, "_", station_df$period)
       x
    }
    )()
  
  ## Process each PDF table with multiple rows per month ----
  pdfs[[1]] <- pdfs[[1]] |>
    structure_table_double(row_index = 9:11, col_index = 13:14) |>
    remove_table_rows()
  
  pdfs[[2]] <- pdfs[[2]] |>
    structure_table_double(row_index = 1:3, col_index = 4:5) |>
    structure_table_double(row_index = 4:6, col_index = 11:12) |>
    structure_table_double(row_index = 11:13, col_index = 2:3) |>
    structure_table_double(row_index = 17:19, col_index = 4:5) |>
    structure_table_double(row_index = 21:23, col_index = 4:5) |>
    remove_table_rows()
  
  pdfs[[3]] <- pdfs[[3]] |>
    structure_table_single(row_index = 13:14, col_index = 11:12)
  
  pdfs[[4]] <- pdfs[[4]] |>
    structure_table_double(row_index = 6:8, col_index = 5) |>
    remove_table_rows()
  
  pdfs[[5]] <- pdfs[[5]] |>
    structure_table_double(row_index = 5:7, col_index = 11:12) |>
    structure_table_double(row_index = 10:12, col_index = 8:10) |>
    structure_table_double(row_index = 14:16, col_index = 2:3) |>
    remove_table_rows()
  
  pdfs[[6]] <- pdfs[[6]] |>
    structure_table_double(row_index = 7:9, col_index = 11:12) |>
    remove_table_rows()
  
  pdfs[[7]] <- pdfs[[7]] |>
    structure_table_triple(row_index = 2:4, col_index = 8:10) |>
    structure_table_double(row_index = 5:7, col_index = 8:10) |>
    structure_table_double(row_index = 12:14, col_index = 11:12) |>
    structure_table_double(row_index = 19:21, col_index = 2:3) |>
    remove_table_rows()
  
  pdfs[[8]] <- pdfs[[8]] |>
    structure_table_double(row_index = 5:7, col_index = 8:10) |>
    structure_table_double(row_index = 8:10, col_index = 8:10) |>
    structure_table_double(row_index = 15:17, col_index = 2:3) |>
    structure_table_double(row_index = 19:21, col_index = c(2:5, 8:10)) |>
    remove_table_rows()
  
  pdfs[[9]] <- pdfs[[9]] |>
    structure_table_double(row_index = 2:4, col_index = 13:14) |>
    structure_table_double(row_index = 10:12, col_index = 2:3) |>
    structure_table_double(row_index = 17:19, col_index = 8:10) |>
    remove_table_rows()
  
  pdfs[[10]] <- pdfs[[10]] |>
    structure_table_double(row_index = 13:15, col_index = 11:12) |>
    remove_table_rows()
  
  pdfs[[13]] <- pdfs[[13]] |>
    structure_table_double(row_index = 7:9, col_index = 11:12) |>
    remove_table_rows()
  
  pdfs[[14]] <- pdfs[[14]] |>
    structure_table_double(row_index = 1:3, col_index = 8:10) |>
    structure_table_double(row_index = 6:8, col_index = 2:3) |>
    structure_table_quadruple(row_index = 12:16, col_index = 2:3) |>
    structure_table_double(row_index = 19:21, col_index = 2:3) |>
    structure_table_double(row_index = 22:24, col_index = 2:3) |>
    structure_table_double(row_index = 25:27, col_index = 11:12) |>
    remove_table_rows()
  
  pdfs[[15]] <- pdfs[[15]] |>
    structure_table_single(row_index = 13:14, col_index = 2:3) |>
    remove_table_rows()
  
  pdfs[[16]] <- pdfs[[16]] |>
    structure_table_double(row_index = 7:9, col_index = 8:10) |>
    structure_table_double(row_index = 10:12, col_index = 8:10) |>
    structure_table_double(row_index = 16:18, col_index = 2:3) |>
    structure_table_double(row_index = 19:21, col_index = 2:3) |>
    remove_table_rows()
  
  pdfs[[17]] <- pdfs[[17]] |>
    structure_table_double(row_index = 3:5, col_index = 2:3) |>
    structure_table_double(row_index = 6:8, col_index = 8:10) |>
    structure_table_double(row_index = 14:16, col_index = 2:3) |>
    structure_table_double(row_index = 17:19, col_index = 2:3) |>
    structure_table_double(row_index = 20:22, col_index = 2:3) |>
    (\(x)
     {
       x[23, 2:14]   <- x[23, 1:13]
       x[23, 1]      <- x[24, 1]
       x[24, 8:10]   <- x[24, 4:6]
       x[24, 4:7]    <- x[23, 4:7]
       x[24, 11:14]  <- x[23, 11:14]
       x[25, 8:10]   <- x[25, 1:3]
       x[25, 1:7]    <- x[23, 1:7]
       x[25, 11:14]  <- x[23, 11:14]
       
       x
    }
    )() |>
    remove_table_rows()
  
  pdfs[[19]] <- pdfs[[19]] |>
    structure_table_double(row_index = 1:3, col_index = 2:3) |>
    (\(x)
     {
       x[15, 2:14]   <- x[15, 1:13]
       x[15, 1]      <- x[16, 1]
       x[16, 8:10]   <- x[16, 4:6]
       x[16, 4:5]    <- x[16, 2:3]
       x[16, 2:3]    <- x[15, 2:3]
       x[16, 6:7]    <- x[15, 6:7]
       x[16, 11:14]  <- x[15, 11:14]
       x[17, 8:10]   <- x[17, 1:3]
       x[17, 1:7]    <- x[15, 1:7]
       x[17, 11:14]  <- x[15, 11:14]
       
       x
    }
    )() |>
    remove_table_rows()
  
  pdfs[[20]] <- pdfs[[20]] |>
    structure_table_double(row_index = 1:3, col_index = 8:10) |>
    structure_table_double(row_index = 4:6, col_index = 8:10) |>
    structure_table_double(row_index = 9:11, col_index = 2:3) |>
    structure_table_double(row_index = 13:15, col_index = 8:10) |>
    structure_table_double(row_index = 16:18, col_index = 4:5) |>
    (\(x)
     {
       x[20, 3]     <- paste0(x[20, 3], x[20, 4])
       x[20, 4:12]  <- x[20, 5:13]
       x[20, 13:14] <- stringr::str_split(x[20, 14], pattern = "\\s+", simplify = TRUE)
       
       x
    }
    )() |>
    remove_table_rows()
  
  pdfs[[22]] <- pdfs[[22]] |>
    structure_table_double(row_index = 1:3, col_index = 2:3) |>
    structure_table_double(row_index = 10:12, col_index = 8:10) |>
    structure_table_double(row_index = 14:16, col_index = 8:10) |>
    structure_table_double(row_index = 18:20, col_index = 2:3) |>
    remove_table_rows()
  
  pdfs[[23]] <- pdfs[[23]] |>
    structure_table_double(row_index = 13:15, col_index = 8:10) |>
    remove_table_rows()
  
  pdfs[[24]] <- pdfs[[24]] |>
    structure_table_double(row_index = 2:4, col_index = 4:5) |>
    structure_table_double(row_index = 10:12, col_index = 11:12) |>
    structure_table_single(row_index = 17:18, col_index = 8:10) |>
    remove_table_rows()
  
  pdfs[[25]] <- pdfs[[25]] |>
    structure_table_double(row_index = 2:4, col_index = 2:3) |>
    structure_table_double(row_index = 7:9, col_index = 8:10) |>
    structure_table_double(row_index = 10:12, col_index = 2:3) |>
    structure_table_double(row_index = 14:16, col_index = 11:12) |>
    remove_table_rows()
  
  pdfs[[26]] <- pdfs[[26]] |>
    structure_table_double(row_index = 1:3, col_index = 13:14) |>
    structure_table_double(row_index = 4:6, col_index = 4:5) |>
    structure_table_double(row_index = 7:9, col_index = 8:10) |>
    structure_table_double(row_index = 10:12, col_index = 2:3) |>
    structure_table_triple(row_index = 15:17, col_index = 2:3) |>
    structure_table_double(row_index = 18:20, col_index = 8:10) |>
    remove_table_rows()
  
  pdfs[[27]] <- pdfs[[27]] |>
    structure_table_double(row_index = 2:4, col_index = 8:10) |>
    structure_table_double(row_index = 5:7, col_index = 2:3) |>
    structure_table_double(row_index = 11:13, col_index = 2:3) |>
    structure_table_double(row_index = 14:16, col_index = 8:10) |>
    structure_table_single(row_index = 21:22, col_index = 2:3) |>
    remove_table_rows()
  
  pdfs[[28]] <- pdfs[[28]] |>
    structure_table_triple(row_index = 8:10, col_index = 4:5)
  
  pdfs[[31]] <- pdfs[[31]] |>
    structure_table_triple(row_index = 10:12, col_index = c(2:3, 11:12)) |>
    (\(x)
     {
       x[12, 11:12] <- x[10, 11:12]
       x
    } 
    )()
  
  pdfs[[32]] <- pdfs[[32]] |>
    structure_table_double(row_index = 3:5, col_index = 11:12) |>
    remove_table_rows()
  
  pdfs[[33]] <- pdfs[[33]] |>
    structure_table_double(row_index = 1:3, col_index = 2:5) |>
    remove_table_rows()

  pdfs[[34]] <- pdfs[[34]] |>
    structure_table_double(row_index = 1:3, col_index = 8:10) |>
    structure_table_double(row_index = 9:11, col_index = 4:5) |>
    structure_table_double(row_index = 12:14, col_index = 2:3) |>
    structure_table_double(row_index = 16:18, col_index = 4:5) |>
    remove_table_rows()
  
  pdfs[[36]] <- pdfs[[36]] |>
    structure_table_triple(row_index = 7:9, col_index = 8:10) |>
    structure_table_double(row_index = 13:15, col_index = 2:3) |>
    structure_table_double(row_index = 17:19, col_index = 11:12) |>
    remove_table_rows()
  
  pdfs[[37]] <- pdfs[[37]] |>
    structure_table_double(row_index = 1:3, col_index = 3) |>
    structure_table_triple(row_index = 10:12, col_index = 4:5) |>
    structure_table_double(row_index = 17:19, col_index = 8:10) |>
    remove_table_rows()
  
  pdfs[[38]] <- pdfs[[38]] |>
    structure_table_double(row_index = 4:6, col_index = 4:5) |>
    remove_table_rows()
  
  pdfs[[40]] <- pdfs[[40]] |>
    structure_table_single(row_index = 13:14, col_index = 8:10)
  
  pdfs[[43]] <- pdfs[[43]] |>
    structure_table_double(row_index = 8:10, col_index = c(2:3, 8:10)) |>
    structure_table_double(row_index = 14:16, col_index = 2:3) |>
    remove_table_rows()
  
  pdfs[[44]] <- pdfs[[44]] |>
    structure_table_double(row_index = 13:15, col_index = 2:3) |>
    remove_table_rows()
  
  pdfs[[45]] <- pdfs[[45]] |>
    structure_table_double(row_index = 8:10, col_index = 11:12) |>
    remove_table_rows()
  
  pdfs[[46]] <- pdfs[[46]] |>
    structure_table_double(row_index = 1:3, col_index = 8:10) |>
    structure_table_quadruple(row_index = 5:9, col_index = 4:5) |>
    structure_table_double(row_index = 10:12, col_index = 2:3) |>
    structure_table_triple(row_index = 13:15, col_index = 2:3) |>
    structure_table_double(row_index = 17:19, col_index = 4:5) |>
    structure_table_double(row_index = 21:23, col_index = 4:5) |>
    structure_table_double(row_index = 24:26, col_index = 4:5) |>
    remove_table_rows()
  
  pdfs[[47]] <- pdfs[[47]] |>
    structure_table_double(row_index = 9:11, col_index = 2:3) |>
    remove_table_rows()
  
  pdfs[[48]] <- pdfs[[48]] |>
    structure_table_double(row_index = 4:6, col_index = 11:12) |>
    structure_table_double(row_index = 12:14, col_index = 11:12) |>
    (\(x)
      {
        x[17, 14]   <- x[17, 1]
        x[17, 1:13] <- x[18, 1:13]
        x[18, 14]   <- x[19, 1]
        x
      }
    )() |>
    remove_table_rows()
  
  pdfs[[49]] <- pdfs[[49]] |>
    structure_table_double(row_index = 7:9, col_index = 4:5) |>
    structure_table_single(row_index = 15:16, col_index = 4:5) |>
    remove_table_rows()
  
  pdfs[[50]] <- pdfs[[50]] |>
    structure_table_double(row_index = 12:14, col_index = 8:10) |>
    structure_table_double(row_index = 15:17, col_index = 2:3) |>
    remove_table_rows()
  
  pdfs[[51]] <- pdfs[[51]] |>
    structure_table_double(row_index = 1:3, col_index = 11:12) |>
    structure_table_double(row_index = 5:7, col_index = 2:3) |>
    structure_table_triple(row_index = 8:10, col_index = 4:5) |>
    structure_table_double(row_index = 12:14, col_index = 3) |>
    structure_table_triple(row_index = 19:21, col_index = 2:5) |>
    structure_table_double(row_index = 23:25, col_index = 4:5) |>
    (\(x)
     {
       x[21, 4:5] <- c("15.0", "11-18-2005")
       x
    }
    )() |>
    remove_table_rows()
  
  pdfs[[52]] <- pdfs[[52]] |>
    structure_table_double(row_index = 6:8, col_index = 2:5) |>
    structure_table_double(row_index = 10:12, col_index = 2:3) |>
    structure_table_double(row_index = 17:19, col_index = 2:3) |>
    remove_table_rows()
  
  pdfs[[53]] <- pdfs[[53]] |>
    structure_table_double(row_index = 13:15, col_index = 2:5) |>
    remove_table_rows()
  
  pdfs[[54]] <- pdfs[[54]] |>
    structure_table_single(row_index = 13:14, col_index = 11:12)
  
  pdfs[[57]] <- pdfs[[57]] |>
    structure_table_double(row_index = 6:8, col_index = 2:3) |>
    structure_table_double(row_index = 12:14, col_index = 11:12) |>
    structure_table_double(row_index = 17:19, col_index = 8:10) |>
    remove_table_rows()
  
  ## Final processing/structuring/concatenating of climate data ----
  structure_climate_data(pdfs)
}

