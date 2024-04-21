#'
#' Get dam level information from PAGASA website
#' 
#' @param .url The URL for the dam level information from PAGASA. Currently, 
#'   this is at https://www.pagasa.dost.gov.ph/flood#dam-information.
#'   
#' @returns A tibble of dam level information
#' 
#' @examples
#' dam_get_level()
#' 
#' @export
#'

dam_get_level <- function(.url = "https://www.pagasa.dost.gov.ph/flood#dam-information") {
  ## Quiet down error on SSL ----
  httr::config(ssl_verifypeer = 0L) |>
    httr::set_config()
  
  ## Initiate an HTML session ----
  pagasa_session <- rvest::session(.url)
  
  ## Retrieve data ----
  dam_tab <- pagasa_session |>
    rvest::html_elements(css = ".dam-table") |>
    rvest::html_table()
  
  dam_tab <- dam_tab[[1]]
  
  first_row_names <- names(dam_tab)
  second_row_names <- dam_tab[1, ] |> unlist()
  names(second_row_names) <- NULL
  
  tab_names <- ifelse(
    first_row_names == second_row_names, 
    first_row_names,
    paste(first_row_names, second_row_names)
  )
  
  names(dam_tab) <- tab_names
  
  dam_tab <- dam_tab |>
    dplyr::slice(2:nrow(dam_tab)) |>
    dplyr::mutate(
      group = paste(`Dam Name`, ceiling(dplyr::row_number() / 2)), 
      `Date Retrieved` = Sys.Date(),
      .before = `Dam Name`
    ) |>
    dplyr::group_by(group) |>
    dplyr::summarise(
      `Date Retrieved` = unique(`Date Retrieved`),
      `Dam Name` = unique(`Dam Name`),
      `Observation Time & Date` = paste(
        paste(`Observation Time & Date`[2], lubridate::year(Sys.Date()), sep = "-"), 
        `Observation Time & Date`[1]
      ),
      `Reservoir Water Level (RWL) (m)` = unique(`Reservoir Water Level (RWL) (m)`),
      `Water Level Deviation Hr` = unique(`Water Level Deviation Hr`),
      `Water Level Deviation Amount` = unique(`Water Level Deviation Amount`),
      `Normal High Water Level (NHWL) (m)` = unique(`Normal High Water Level (NHWL) (m)`),
      `Deviation from NHWL (m)` = unique(`Deviation from NHWL (m)`),
      `Rule Curve Elevation (m)` = unique(`Rule Curve Elevation (m)`),
      `Deviation from Rule Curve (m)` = unique(`Deviation from Rule Curve (m)`),
      `Gate Opening Gates` = unique(`Gate Opening Gates`),                 
      `Gate Opening Meters` = unique(`Gate Opening Meters`),
      `Estimated (cms) Inflow` = unique(`Estimated (cms) Inflow`),            
      `Estimated (cms) Outflow` = unique(`Estimated (cms) Outflow`),
      .groups = "drop"
    ) |>
    dplyr::select(-group) |>
    dplyr::mutate(
      `Observation Time & Date` = strptime(
        `Observation Time & Date`, format = "%b-%d-%Y %H:%M %p"
      ),
      `Reservoir Water Level (RWL) (m)` = as.numeric(`Reservoir Water Level (RWL) (m)`),
      `Water Level Deviation Hr` = as.integer(`Water Level Deviation Hr`),
      `Water Level Deviation Amount` = as.numeric(`Water Level Deviation Amount`),
      `Normal High Water Level (NHWL) (m)` = as.numeric(`Normal High Water Level (NHWL) (m)`),
      `Deviation from NHWL (m)` = as.numeric(`Deviation from NHWL (m)`),
      `Rule Curve Elevation (m)` = as.numeric(`Rule Curve Elevation (m)`),
      `Deviation from Rule Curve (m)` = as.numeric(`Deviation from Rule Curve (m)`),
      `Gate Opening Gates` = as.numeric(`Gate Opening Gates`),                 
      `Gate Opening Meters` = as.numeric(`Gate Opening Meters`),
      `Estimated (cms) Inflow` = as.numeric(`Estimated (cms) Inflow`),            
      `Estimated (cms) Outflow` = as.numeric(`Estimated (cms) Outflow`),
    )
    
  ## Return dam_tab
  dam_tab
}


#'
#' Archive scraped dam level data
#'
#'

dam_archive_raw <- function(dam_level_data, directory = "data-raw") {
  archive_dir <- file.path(directory, "dam")
  
  if (!dir.exists(archive_dir)) dir.create(archive_dir, recursive = TRUE)
  
  write.csv(
    dam_level_data, 
    file = paste0(archive_dir, "/dam_level_", Sys.Date(), ".csv"), 
    row.names = FALSE
  )
  
  paste0(archive_dir, "/dam_level_", Sys.Date(), ".csv")
}
