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
#' Read and process PDFs for climatological extremes between 1991-2020
#'

climate_process_1991 <- function(pdf_path) {
  ## Read pdfs ----
  pdfs <- lapply(
    X = pdf_path,
    FUN = function(x) {
      pdftools::pdf_text(x) |>
        stringr::str_split(pattern = "\n") |>
        unlist()
    }
  )
  
  ## Get station information ----
  station <- lapply(
    X = pdfs,
    FUN = function(x) {
      x[stringr::str_detect(string = x, pattern = "STATION")] |>
        stringr::str_split(pattern = "\\s{2,100}", simplify = TRUE) |>
        (\(x) x[ , 1])() |>
        stringr::str_remove_all(pattern = "STATION: ") |>
        stringr::str_to_title() |>
        stringr::str_replace_all(pattern = "Cubi Pt.", replacement = "Cubi Point") |>
        stringr::str_replace_all(pattern = "Naia", replacement = "NAIA") |>
        stringr::str_replace_all(pattern = "Mia", replacement = "MIA") |>
        stringr::str_replace_all(pattern = "Mco", replacement = "MCO") |>
        stringr::str_replace_all(pattern = "Del", replacement = "del") |>
        stringr::str_replace_all(pattern = "Former Vigan Station", replacement = "former Vigan Station") |>
        stringr::str_replace_all(pattern = "Synop", replacement = "SYNOP")
    }
  ) |>
    unlist()
  
  ## Get period information ----
  period <- lapply(
    X = pdfs,
    FUN = function(x) {
      x[stringr::str_detect(string = x, pattern = "PERIOD:")] |>
        stringr::str_split(pattern = "\\s{2,100}", simplify = TRUE) |>
        (\(x) x[ , 1])() |>
        stringr::str_remove_all(pattern = "PERIOD: ") |>
        stringr::str_replace_all(pattern = " - ", replacement = "-") |>
        stringr::str_to_title()
    }
  ) |>
    unlist()
  
  ## Get latitude and longitude information ----
  latitude <- lapply(
    X = pdfs,
    FUN = function(x) {
      x[stringr::str_detect(string = x, pattern = "LATITUDE")] |>
        stringr::str_split(pattern = "\\s{2,100}", simplify = TRUE) |>
        (\(x) x[ , 2])() |>
        stringr::str_replace_all(pattern = "14.76N", replacement = "14.76\"N") |>
        stringr::str_replace_all(pattern = "13.20N", replacement = "13.20\"N") |>
        stringr::str_remove_all("LATITUDE: ") |>
        stringr::str_split(pattern = "o|°|'|\"", simplify = TRUE) |>
        (\(x) x[x != ""])() |>
        rbind() |>
        data.frame() |>
        (\(x)
          {
            names(x) <- c("degrees", "minutes", "seconds", "direction")
            x
          }
        )()
    }
  ) |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      degrees = as.numeric(degrees),
      minutes = as.numeric(minutes),
      seconds = as.numeric(seconds),
      latitude = degrees + (minutes / 60) + (seconds / 3600)
    ) |>
    dplyr::pull(latitude)
  
  longitude <- lapply(
    X = pdfs,
    FUN = function(x) {
      x[stringr::str_detect(string = x, pattern = "LONGITUDE:")] |>
        stringr::str_split(pattern = "\\s{2,100}", simplify = TRUE) |>
        (\(x) x[ , 2])() |>
        stringr::str_replace_all(pattern = "56.76E", replacement = "56.76\"E") |>
        stringr::str_replace_all(pattern = "08.10E", replacement = "0.810\"E") |>
        stringr::str_replace_all(pattern = "57.53E", replacement = "14.76\"E") |>
        stringr::str_remove_all("LONGITUDE: ") |>
        stringr::str_split(pattern = "o|°|'|\"", simplify = TRUE) |>
        (\(x) x[x != ""])() |>
        rbind() |>
        data.frame() |>
        (\(x)
         {
           names(x) <- c("degrees", "minutes", "seconds", "direction")
           x
        }
        )()
    }
  ) |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      degrees = as.numeric(degrees),
      minutes = as.numeric(minutes),
      seconds = as.numeric(seconds),
      longitude = degrees + (minutes / 60) + (seconds / 3600)
    ) |>
    dplyr::pull(longitude)
    
  elevation <- lapply(
    X = pdfs,
    FUN = function(x) {
      x[stringr::str_detect(string = x, pattern = "ELEVATION:")] |>
        stringr::str_split(pattern = "\\s{2,1000}", simplify = TRUE) |>
        (\(x) x[x != ""])() |>
        stringr::str_remove_all(pattern = "ELEVATION: | m|m") |>
        as.numeric()
    }
  ) |>
    unlist()
  
  station_df <- tibble::tibble(
    station = station,
    latitude = latitude,
    longitude = longitude,
    elevation = elevation
  )
  
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
        names(x) <- paste0(station, "_", period) 
        x
      }
    )()
  
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
 
  climate_df <- lapply(
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
        unlist()
    ) |>
    dplyr::select(-station_period) |>
    dplyr::relocate(station:time_period, .before = time_name) |>
    tibble::tibble()
  
  climate_df
}



climate_process_2020 <- function(pdf_path) {
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
  )
  
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
        
        x
      }
    )()
}


