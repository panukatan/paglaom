#' 
#' Collect all targets and lists of targets in the environment
#' 
#' 
all_targets <- function(env = parent.env(environment()), 
                        type = "tar_target", 
                        add_list_names = TRUE) {
  
  ## Function to determine if an object is a type (a target), 
  ## or a list on only that type
  rfn <- function(obj) 
    inherits(obj, type) || (is.list(obj) && all(vapply(obj, rfn, logical(1))))
  
  ## Get the names of everything in the environment 
  ## (e.g. sourced in the _targets.R file)
  objs <- ls(env)
  
  out <- list()
  for (o in objs) {
    obj <- get(o, envir = env)      ## Get each top-level object in turn
    if (rfn(obj)) {                 ## For targets and lists of targets
      out[[length(out) + 1]] <- obj ## Add them to the output
      
      ## If the object is a list of targets, add a vector of the target names 
      ## to the environment So that one can call `tar_make(list_name)` to make 
      ## all the targets in that list
      if (add_list_names && is.list(obj)) {
        target_names <- vapply(obj, \(x) x$settings$name, character(1))
        assign(o, target_names, envir = env)
      }
    }
  }
  return(out)
}



#'
#' Helper function/s to process climate tables
#'
#'

structure_table_single <- function(pdf, row_index, col_index) {
  pdf |>
    (\(x)
     {
       x[row_index[2], col_index]             <- x[row_index[2], ][x[row_index[2], ] != ""]
       x[row_index[2], 1:(col_index[1] - 1)]  <- x[row_index[1], 1:(col_index[1] - 1)]
       x[row_index[2], (col_index[2] + 1):14] <- x[row_index[1], (col_index[2] + 1):14]
       x
    }
    )()
}


structure_table_double <- function(pdf, row_index, col_index) {
  pdf |>
    (\(x)
      {
        x[row_index[1], 2:14]      <- x[row_index[1], 1:13]
        x[row_index[1], 1]         <- x[row_index[2], 1]
        x[row_index[2], 2:14]      <- x[row_index[1], 2:14]
        x[row_index[2], col_index] <- x[row_index[3], ][x[row_index[3], ] != ""]
        
        x
      }
    )()
}


structure_table_triple <- function(pdf, row_index, col_index) {
  pdf |>
    (\(x)
      {
        x[row_index[1], 2:14]      <- x[row_index[1], 1:13]
        x[row_index[1], 1]         <- x[row_index[2], 1]
        
        x[row_index[2], col_index] <- x[row_index[2], ][stringr::str_detect(x[row_index[2], ], paste(toupper(month.abb), collapse = "|"), negate = TRUE)] |> (\(x) x[x != ""])()
        x[row_index[2], which(!1:14 %in% col_index)]  <- x[row_index[1], which(!1:14 %in% col_index)]
        
        x[row_index[3], col_index]             <- x[row_index[3], 1:length(col_index)]
        x[row_index[3], which(!1:14 %in% col_index)]  <- x[row_index[1], which(!1:14 %in% col_index)]
        
        x
      }
    )()
}


structure_table_quadruple <- function(pdf, row_index, col_index) {
  pdf |>
    (\(x)
      {
        x[row_index[1], 2:14]      <- x[row_index[1], 1:13]
        x[row_index[1], 1]         <- x[row_index[3], 1]
       
        x[row_index[2], col_index]             <- x[row_index[2], ][x[row_index[2], ] != ""]
        x[row_index[2], 1:(col_index[1] - 1)]  <- x[row_index[1], 1:(col_index[1] - 1)]
        x[row_index[2], (col_index[2] + 1):14] <- x[row_index[1], (col_index[2] + 1):14]
       
        x[row_index[3], 2:14]                  <- x[row_index[1], 2:14]
        x[row_index[3], col_index]             <- x[row_index[4], ][x[row_index[4], ] != ""]

        x[row_index[4], ]                      <- x[row_index[1], ]
        x[row_index[4], col_index]             <- x[row_index[5], ][x[row_index[5], ] != ""]
       
        x
      }
    )()
}

remove_table_rows <- function(pdf_tab) {

  stringr::str_detect(pdf_tab[ , 1], pattern = "[A-Z]") |>
    (\(x) pdf_tab[x, ])()
  
}


#'
#' Retrieve weather station information from set of climate PDF datasets
#' 
#'

get_weather_station_info <- function(climate_download_files, 
                                     period = NULL) {
  ## Get vector of file paths for specified period ----
  if (is.null(period)) {
    pdf_path <- climate_download_files
  } else {
    pdf_path <- climate_download_files |>
      (\(x) x[stringr::str_detect(string = x, pattern = period)])()
  }

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
      x[stringr::str_detect(string = x, pattern = "PERIOD:|YEAR:")] |>
        stringr::str_split(pattern = "\\s{2,100}", simplify = TRUE) |>
        (\(x) x[ , 1])() |>
        stringr::str_remove_all(pattern = "PERIOD: |YEAR: ") |>
        stringr::str_replace_all(pattern = " - ", replacement = "-") |>
        stringr::str_to_title()
    }
  ) |>
    unlist()
  
  ## Get station latitude information ----
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
  
  ## Get station longitude information ----
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
  
  ## Get station elevation information ----
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
  
  ## Create station information data.frame ----
  station_df <- tibble::tibble(
    station = station,
    period = period,
    latitude = latitude,
    longitude = longitude,
    elevation = elevation
  )
  
  ## Return station_df
  station_df
}

