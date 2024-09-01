#'
#' @param .year 
#' @param major
#'

paglaom_create_release_list <- function(.year = lubridate::year(Sys.Date()),
                                        major) {
  jan_first <- as.Date(paste0(year, "-01-01"))
  first_sunday <- jan_first + (7 - as.numeric(format(jan_first, "%u"))) %% 7
  
  sunday_dates <- seq.Date(
    from = first_sunday, to = as.Date(paste0(year, "-12-31")), by = "week"
  )
  
  month_labels <- lubridate::month(sunday_dates)
  
  week_labels <- lubridate::week(sunday_dates)
  
  version <- paste(major, month_labels, week_labels, sep = ".")
  
  version
}



#'
#' Create a github data release
#'

paglaom_create_weekly_release <- function(repo = "panukatan/paglaom",
                                          major) {
  ## Get release names from GitHub ----
  release_names <- piggyback::pb_releases()$release_name

  ## Assign release name for this release ----
  repeat {
    release_name <- bagyo::get_bagyo()$name
    if (!release_name %in% release_names) break 
  }
    
  ## Create tag ----
  tag <- Sys.Date() |>
    (\(x) x - as.numeric(format(x, "%u")))() |>
    (\(x)
      {
        month_tag <- lubridate::month(x)
        week_tag  <- lubridate::week(x)
        tag <- paste(major, month_tag, week_tag, sep = ".")
        tag
      }
    )()
    
  
  ## Create release ----
  piggyback::pb_release_create(
    repo = repo, tag = tag, name = release_name
  )
  
  ## Return tag ----
  tag
}


#'
#' Create data upload to GitHub
#'

paglaom_upload_weekly_release <- function(repo = "panukatan/paglaom",
                                          tag) {
  zipdir <- tempdir()
  zip_climate <- file.path(zipdir, "climate.zip")
  zip_cyclones <- file.path(zipdir, "cyclones.zip")
  zip_dam <- file.path(zipdir, "dam.zip")
  zip_heat <- file.path(zipdir, "heat_index.zip")
  
  ## zip climate files ----
  zip(
    zip_climate, 
    files = list.files(
      path = "data-raw/climate", full.names = TRUE, recursive = TRUE)
  )
  
  ## zip cyclones files ----
  zip(
    zip_cyclones, 
    files = list.files(
      path = "data-raw/cyclones", full.names = TRUE, recursive = TRUE
    )
  )
  
  ## zip dam files ----
  zip(
    zip_dam, 
    files = list.files(
      path = "data-raw/dam", full.names = TRUE, recursive = TRUE
    )
  )
  
  ## zip heat files ----
  zip(
    zip_heat, 
    files = list.files(
      path = "data-raw/heat_index", full.names = TRUE, recursive = TRUE
    )
  )
  
  lapply(
    X = c(zip_climate, zip_cyclones, zip_dam, zip_heat),
    FUN = piggyback::pb_upload,
    tag = tag
  )
}


#'
#' Get filenames of data from current release
#' 
#'

get_data_release_filenames <- function(dataset = NULL) {
  if (is.null(dataset)) {
    urls <- piggyback::pb_download_url()
  } else {
    urls <- piggyback::pb_download_url(file = paste0(dataset, ".zip"))
  }
  
  destfile <- file.path(tempdir(), basename(urls))
  
  Map(
    f = download.file,
    url = urls, 
    destfile = destfile, 
    mode = "wb"
  )
  
  lapply(
    X = destfile,
    FUN = unzip,
    list = TRUE
  ) |>
    dplyr::bind_rows() |>
    dplyr::pull(Name) |>
    c()
  
  unlink(destfile)
}
