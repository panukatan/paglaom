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
                                          tag) {
  release_names <- piggyback::pb_releases()$release_name
  
  repeat {
    release_name <- bagyo::get_bagyo()$name
    if (!release_name %in% release_names) break 
  }
  
  piggyback::pb_release_create(
    repo = repo, tag = tag, name = release_name
  )
}