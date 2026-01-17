#'
#' Retrieve download links for yearly cyclones reports
#' 
#' @param url PAGASA URL where reports download links can be found
#' 
#' @returns A vector of URL links to the yearly cyclones reports
#' 
#' @examples
#' cyclones_get_report_links()
#' 
#' @rdname cyclones_get
#' @export
#' 
#'

cyclones_get_report_links <- function(.url = "https://pubfiles.pagasa.dost.gov.ph/pagasaweb/files/tamss/weather/tcsummary/") {
  ## Initiate an HTML session ----
  pagasa_session <- rvest::session(.url)
  
  ## Retrieve links ----
  links <- pagasa_session |>
    rvest::html_elements(css = "pre a") |>
    rvest::html_attr(name = "href") |>
    (\(x) x[stringr::str_detect(string = x, pattern = "PAGASA_ARTC")])() |>
    (\(x) paste0(.url, x))()

  links |>
    (\(x) x[grep(pattern = "compressed", x = x, invert = TRUE)])()
}


#'
#' Download cyclones reports
#' 
#' @param url_link URL link to a cyclones report
#' @param directory Name of directory where report is to be downloaded
#'
#' @returns File downloaded to specified path (invisible). A character value for
#'   path to downloaded file
#' 
#' @examples
#' cyclones_download_report(
#'   url_link = "https://pubfiles.pagasa.dost.gov.ph/pagasaweb/files/tamss/weather/tcsummary/PAGASA_ARTC_2017.pdf",
#'   directory = "data-raw"
#' )
#'   
#' @rdname cyclones_download
#' @export
#'

cyclones_download_report <- function(url_link, directory) {
  ## Create directory path to cyclones ----
  dir_path <- file.path(directory, "cyclones")
  
  ## Check if directory exists; if not create ----
  if (!dir.exists(dir_path))
    dir.create(dir_path, showWarnings = FALSE, recursive = TRUE)
  
  ## Create file path to download ----
  path <- file.path(dir_path, basename(url_link))

  ## Download file/s ----
  Map(
    f = download.file,
    url = as.list(url_link),
    destfile = as.list(path)
  )
  
  ## Return path/s ----
  path
}


#'
#' Process cyclones data from reports
#' 
#' @param path_to_report File path to downloaded report
#' @param year Year of report
#' 
#' @returns Cyclones data.frame
#'
#' @examples 
#' 
#' @rdname cyclones_process
#' @export
#'

cyclones_process_peak_2017 <- function(path_to_report) {
  df_2017 <- pdftools::pdf_data(path_to_report)[[24]]
  
  df_2017 <- df_2017 |>
    dplyr::filter(y %in% c(125, 136, 148)) |>
    dplyr::pull(text) |>
    (\(x)
     {
       cbind(
         c(x[1:3], NA_character_, x[4:11], NA_character_,
           x[12:19], NA_character_, x[20:24]) |>
           (\(x) ifelse(x == "-", NA_character_, x))() |>
           matrix(ncol = 9, byrow = TRUE) |>
           (\(x) x[ , c(1:6, 8:9)])(),
         x[25:length(x)] |>
           matrix(ncol = 2, byrow = TRUE)
       )
    }
    )() |>
    rbind(
      df_2017 |>
        dplyr::filter(y %in% 159:366) |>
        dplyr::pull(text) |>
        (\(x)
         {
           cbind(
             x[1:170] |>
               (\(x) ifelse(x == "-", NA_character_, x))() |>
               (\(x) c(x[1:93], NA_character_, x[94:170]))() |>
               matrix(ncol = 9, byrow = TRUE) |>
               (\(x) x[ , c(1:6, 8:9)])(),
             x[171:length(x)] |>
               matrix(ncol = 2, byrow = TRUE)
           )
        }
        )()
    ) |>
    data.frame() |>
    tibble::tibble() |>
    dplyr::rename_with(
      .fn = function(x) c(
        "category", "domestic_name",
        "international_code", "international_name",
        "warning_start_date", "warning_start_time",
        "warning_end_date", "warning_end_time",
        "peak_pressure", "peak_speed"
      )
    ) |>
    dplyr::mutate(
      category_code = factor(
        category,
        levels = c("TD", "TS", "STS", "TY", "STY")
      ),
      category_name = factor(
        category,
        levels = c("TD", "TS", "STS", "TY", "STY"),
        labels = c(
          "Tropical Depression", "Tropical Storm", "Severe Tropical Storm",
          "Typhoon", "Super Typhoon"
        )
      ),
      domestic_name = stringr::str_to_title(domestic_name),
      international_name = stringr::str_to_title(international_name),
      warning_start = paste0(warning_start_date, "/2017") |>
        paste(warning_start_time) |>
        strptime(format = "%m/%d/%Y %I%p", tz = "PST"),
      warning_end = paste0(warning_end_date, "/2017") |>
        paste(warning_end_time) |>
        strptime(format = "%m/%d/%Y %I%p", tz = "PST"),
      dplyr::across(.cols = dplyr::starts_with("peak"), .fns = ~as.integer(.x))
    ) |>
    dplyr::select(
      category_code, category_name, domestic_name, international_name,
      warning_start, warning_end, peak_pressure, peak_speed
    )
  
  df_2017
}


#'
#' @rdname cyclones_process
#' @export
#'
cyclones_process_peak_2018 <- function(path_to_report) {
  df_2018 <- pdftools::pdf_data(path_to_report)[[33]]
  
  set1_2018 <- df_2018 |>
    dplyr::filter(y %in% 134:364) |>
    dplyr::pull(text) |>
    (\(x) x[x != "!"])() |>
    (\(x) x[x != "to"])() |>
    matrix(ncol = 11, byrow = TRUE) |>
    (\(x) x[ , c(1:6, 9:11)])() |>
    data.frame() |>
    tibble::tibble() |>
    dplyr::rename_with(
      .fn = function(x)
        c("domestic_name", "international_name", "warning_start_date",
          "warning_start_time", "warning_end_date", "warning_end_time",
          "peak_pressure", "peak_speed", "category_code")
    ) |>
    dplyr::mutate(
      international_name = ifelse(
        international_name == "-", NA_character_, international_name
      ),
      warning_start = paste0(warning_start_date, "/2018 ", warning_start_time) |>
        strptime(format = "%m/%d/%Y %H", tz = "UTC"),
      warning_end = paste0(warning_end_date, "/2018 ", warning_end_time) |>
        strptime(format = "%m/%d/%Y %H", tz = "UTC"),
      dplyr::across(.cols = dplyr::starts_with("peak"), .fns = ~as.integer(.x)),
      category_code = factor(
        category_code,
        levels = c("TD", "TS", "STS", "TY", "STY")
      ),
      category_name = factor(
        category_code,
        levels = c("TD", "TS", "STS", "TY", "STY"),
        labels = c(
          "Tropical Depression", "Tropical Storm", "Severe Tropical Storm",
          "Typhoon", "Super Typhoon"
        )
      )
    )
  
  set2_2018 <- df_2018 |>
    dplyr::filter(y %in% 479:709) |>
    dplyr::pull(text) |>
    (\(x) x[x != "!"])() |>
    (\(x) x[x != "to"])() |>
    (\(x) c(x[1:151], paste(x[152:153], collapse = ""), x[154:length(x)]))() |>
    matrix(ncol = 12, byrow = TRUE) |>
    data.frame() |>
    tibble::tibble() |>
    dplyr::rename_with(
      .fn = function(x)
        c("domestic_name", "international_name", "warning_start_date",
          "warning_start_time", "warning_end_date", "warning_end_time",
          "duration_days", "duration_hours", "TCU", "TCA", "SWB", "IWS")
    ) |>
    dplyr::mutate(
      international_name = ifelse(
        international_name == "-", NA_character_, international_name
      ),
      warning_start = paste0(warning_start_date, "/2018 ", warning_start_time) |>
        strptime(format = "%m/%d/%Y %H", tz = "PST"),
      warning_end = paste0(warning_end_date, "/2018 ", warning_end_time) |>
        strptime(format = "%m/%d/%Y %H", tz = "PST")
    )
  
  df_2018 <- set1_2018 |>
    dplyr::mutate(
      warning_start = set2_2018$warning_start,
      warning_end = set2_2018$warning_end
    ) |>
    dplyr::select(
      category_code, category_name, domestic_name, international_name,
      warning_start, warning_end, peak_pressure, peak_speed
    )
  
  df_2018
}

#'
#' @rdname cyclones_process
#' @export
#'
cyclones_process_peak_2019 <- function(path_to_report) {
  df_2019 <- pdftools::pdf_data(path_to_report)[[37]]
  
  set1_2019 <- df_2019 |>
    dplyr::filter(y %in% 142:382) |>
    dplyr::pull(text) |>
    (\(x) x[x != "to"])() |>
    (\(x) c(
      x[1:2], NA_character_, x[3:23], NA_character_,
      x[24:44], NA_character_, x[45:65], NA_character_,
      x[66:130], NA_character_, x[131:length(x)]
    ))() |>
    matrix(ncol = 11, byrow = TRUE) |>
    data.frame() |>
    tibble::tibble() |>
    dplyr::rename_with(
      .fn = function(x)
        c("domestic_name", "international_name", "international_code",
          "warning_start_date", "warning_start_time", "warning_end_date",
          "warning_end_time", "peak_pressure", "peak_speed", "peak_date",
          "peak_time")
    ) |>
    dplyr::mutate(
      international_name = ifelse(
        international_name == "Unnamed", NA_character_, international_name
      ),
      international_code = stringr::str_remove_all(
        string = international_code, pattern = "\\(|\\)"
      ),
      warning_start = paste0(warning_start_date, "/2019 ", warning_start_time) |>
        strptime(format = "%m/%d/%Y %H", tz = "UTC"),
      warning_end = paste0(warning_end_date, "/2019 ", warning_end_time) |>
        strptime(format = "%m/%d/%Y %H", tz = "UTC"),
      dplyr::across(.cols = peak_pressure:peak_speed, .fns = ~as.integer(.x)),
      peak_time = paste0(peak_date, "/2019 ", peak_time) |>
        strptime(format = "%m/%d/%Y %H", tz = "UTC")
    )
  
  set2_2019 <- df_2019 |>
    dplyr::filter(y %in% 488:728) |>
    dplyr::pull(text) |>
    (\(x) x[x != "to"])() |>
    (\(x) c(
      x[1:2], NA_character_, x[3:23], NA_character_,
      x[24:44], NA_character_, x[45:65], NA_character_,
      x[66:130], NA_character_, x[131:length(x)]
    ))() |>
    matrix(ncol = 11, byrow = TRUE) |>
    data.frame() |>
    tibble::tibble() |>
    dplyr::rename_with(
      .fn = function(x)
        c("domestic_name", "international_name", "international_code",
          "warning_start_date", "warning_start_time", "warning_end_date",
          "warning_end_time", "duration_days", "duration_hours", "category_code",
          "landfall")
    ) |>
    dplyr::mutate(
      international_name = ifelse(
        international_name == "Unnamed", NA_character_, international_name
      ),
      international_code = stringr::str_remove_all(
        string = international_code, pattern = "\\(|\\)"
      ),
      warning_start = paste0(warning_start_date, "/2019 ", warning_start_time) |>
        strptime(format = "%m/%d/%Y %H", tz = "UTC"),
      warning_end = paste0(warning_end_date, "/2019 ", warning_end_time) |>
        strptime(format = "%m/%d/%Y %H", tz = "UTC"),
      category_code = factor(
        category_code,
        levels = c("TD", "TS", "STS", "TY", "STY")
      ),
      category_name = factor(
        category_code,
        levels = c("TD", "TS", "STS", "TY", "STY"),
        labels = c(
          "Tropical Depression", "Tropical Storm", "Severe Tropical Storm",
          "Typhoon", "Super Typhoon"
        )
      )
    )
  
  df_2019 <- set1_2019 |>
    dplyr::mutate(
      warning_start = set2_2019$warning_start,
      warning_end = set2_2019$warning_end,
      category_code = set2_2019$category_code,
      category_name = set2_2019$category_name
    ) |>
    dplyr::select(
      category_code, category_name, domestic_name, international_name,
      warning_start, warning_end, peak_pressure, peak_speed
    )
  
  df_2019
}

#'
#' @rdname cyclones_process
#' @export
#'
cyclones_process_peak_2020 <- function(path_to_report) {
  df_2020 <- pdftools::pdf_data(path_to_report)[[35]]
  
  set1_2020 <- df_2020 |>
    dplyr::filter(y %in% 158:400) |>
    dplyr::pull(text) |>
    (\(x) x[x != "to"])() |>
    (\(x) c(
      x[1:24], NA_character_, x[25:67], NA_character_,
      x[68:154], NA_character_, x[155:length(x)]
    ))() |>
    matrix(ncol = 11, byrow = TRUE) |>
    data.frame() |>
    tibble::tibble() |>
    dplyr::rename_with(
      .fn = function(x)
        c("domestic_name", "international_name", "international_code",
          "warning_start_date", "warning_start_time", "warning_end_date",
          "warning_end_time", "peak_speed", "peak_pressure", "peak_date",
          "peak_time")
    ) |>
    dplyr::mutate(
      international_name = ifelse(
        international_name == "Unnamed", NA_character_, international_name
      ),
      international_code = stringr::str_remove_all(
        string = international_code, pattern = "\\(|\\)"
      ),
      warning_start = paste0(warning_start_date, "/2020 ", warning_start_time) |>
        strptime(format = "%m/%d/%Y %H", tz = "UTC"),
      warning_end = paste0(warning_end_date, "/2020 ", warning_end_time) |>
        strptime(format = "%m/%d/%Y %H", tz = "UTC"),
      dplyr::across(.cols = peak_pressure:peak_speed, .fns = ~as.integer(.x)),
      peak_time = paste0(peak_date, "/2020 ", peak_time) |>
        strptime(format = "%m/%d/%Y %H", tz = "UTC")
    )
  
  set2_2020 <- df_2020 |>
    dplyr::filter(y %in% 495:737) |>
    dplyr::pull(text) |>
    (\(x) x[x != "to"])() |>
    (\(x) c(
      x[1:71], NA_character_, x[72:81], NA_character_,
      x[82:146], NA_character_, x[147:length(x)]
    ))() |>
    (\(x) c(
      x[1:24], NA_character_, x[25:67], NA_character_,
      x[68:154], NA_character_, x[155:length(x)]
    ))() |>
    matrix(ncol = 11, byrow = TRUE) |>
    data.frame() |>
    tibble::tibble() |>
    dplyr::rename_with(
      .fn = function(x)
        c("domestic_name", "international_name", "international_code",
          "warning_start_date", "warning_start_time", "warning_end_date",
          "warning_end_time", "duration_days", "duration_hours", "category_code",
          "landfall")
    ) |>
    dplyr::mutate(
      international_name = ifelse(
        international_name == "Unnamed", NA_character_, international_name
      ),
      international_code = stringr::str_remove_all(
        string = international_code, pattern = "\\(|\\)"
      ),
      warning_start = paste0(warning_start_date, "/2020 ", warning_start_time) |>
        strptime(format = "%m/%d/%Y %H", tz = "UTC"),
      warning_end = paste0(warning_end_date, "/2020 ", warning_end_time) |>
        strptime(format = "%m/%d/%Y %H", tz = "UTC"),
      category_code = factor(
        category_code,
        levels = c("TD", "TS", "STS", "TY", "STY")
      ),
      category_name = factor(
        category_code,
        levels = c("TD", "TS", "STS", "TY", "STY"),
        labels = c(
          "Tropical Depression", "Tropical Storm", "Severe Tropical Storm",
          "Typhoon", "Super Typhoon"
        )
      )
    )
  
  df_2020 <- set1_2020 |>
    dplyr::mutate(
      warning_start = set2_2020$warning_start,
      warning_end = set2_2020$warning_end,
      category_code = set2_2020$category_code,
      category_name = set2_2020$category_name
    ) |>
    dplyr::select(
      category_code, category_name, domestic_name, international_name,
      warning_start, warning_end, peak_pressure, peak_speed
    )
  
  df_2020
}

#'
#' @rdname cyclones_process
#' @export
#'

cyclones_process_peak_2021 <- function(path_to_report) {
  df_2021 <- pdftools::pdf_data(path_to_report)
  
  set1_2021 <- df_2021[[41]] |>
    dplyr::filter(y %in% 409:616) |>
    dplyr::pull(text) |>
    (\(x) x[x != "to"])() |>
    (\(x) c(
      x[1:24], NA_character_, x[25:43], x[46:47], NA_character_,
      x[48:88], x[91:112], x[115:149], NA_character_, x[150:168]
    ))() |>
    matrix(ncol = 11, byrow = TRUE) |>
    data.frame() |>
    tibble::tibble() |>
    dplyr::rename_with(
      .fn = function(x)
        c("domestic_name", "international_name", "international_code",
          "warning_start_date", "warning_start_time", "warning_end_date",
          "warning_end_time", "peak_speed", "peak_pressure", "peak_date",
          "peak_time")
    ) |>
    dplyr::mutate(
      international_name = ifelse(
        international_name == "Unnamed", NA_character_, international_name
      ),
      international_code = stringr::str_remove_all(
        string = international_code, pattern = "\\(|\\)"
      ),
      warning_start = paste0(warning_start_date, "/2020 ", warning_start_time) |>
        strptime(format = "%m/%d/%Y %H", tz = "UTC"),
      warning_end = paste0(warning_end_date, "/2020 ", warning_end_time) |>
        strptime(format = "%m/%d/%Y %H", tz = "UTC"),
      dplyr::across(.cols = peak_pressure:peak_speed, .fns = ~as.integer(.x)),
      peak_time = paste0(peak_date, "/2020 ", peak_time) |>
        strptime(format = "%m/%d/%Y %H", tz = "UTC")
    )
  
  set2_2021 <- df_2021[[42]] |>
    dplyr::filter(y %in% 168:328) |>
    dplyr::pull(text) |>
    (\(x) x[x != "to"])() |>
    (\(x) c(
      x[1:19], NA_character_, x[20:23], NA_character_,
      x[24:28], NA_character_, x[29:43], NA_character_,
      x[44:69], NA_character_, x[70:79], NA_character_,
      x[80:112], paste(x[113:114], collapse = ""), x[115:140], NA_character_,
      x[141:length(x)]
    ))() |>
    matrix(ncol = 11, byrow = TRUE) |>
    data.frame() |>
    tibble::tibble() |>
    dplyr::rename_with(
      .fn = function(x)
        c("domestic_name", "international_name", "international_code",
          "warning_start_date", "warning_start_time", "warning_end_date",
          "warning_end_time", "duration_days", "duration_hours", "category_code",
          "landfall")
    ) |>
    dplyr::mutate(
      international_name = ifelse(
        international_name == "Unnamed", NA_character_, international_name
      ),
      international_code = stringr::str_remove_all(
        string = international_code, pattern = "\\(|\\)"
      ),
      warning_start = paste0(warning_start_date, "/2021 ", warning_start_time) |>
        strptime(format = "%m/%d/%Y %H", tz = "UTC"),
      warning_end = paste0(warning_end_date, "/2021 ", warning_end_time) |>
        strptime(format = "%m/%d/%Y %H", tz = "UTC"),
      category_code = factor(
        category_code,
        levels = c("TD", "TS", "STS", "TY", "STY")
      ),
      category_name = factor(
        category_code,
        levels = c("TD", "TS", "STS", "TY", "STY"),
        labels = c(
          "Tropical Depression", "Tropical Storm", "Severe Tropical Storm",
          "Typhoon", "Super Typhoon"
        )
      )
    )
  
  df_2021 <- set1_2021 |>
    dplyr::mutate(
      warning_start = set2_2021$warning_start,
      warning_end = set2_2021$warning_end,
      category_code = set2_2021$category_code,
      category_name = set2_2021$category_name
    ) |>
    dplyr::select(
      category_code, category_name, domestic_name, international_name,
      warning_start, warning_end, peak_pressure, peak_speed
    ) |>
    dplyr::mutate(
      domestic_name = stringr::str_to_title(domestic_name),
      international_name = stringr::str_to_title(international_name)
    )
  
  df_2021
}

#'
#' @rdname cyclones_process
#' @export
#'

cyclones_process_peak_2022 <- function(path_to_report) {
  df_2022 <- pdftools::pdf_data(path_to_report)
  
  x <- df_2022[[32]] |>
    dplyr::filter(y %in% 419:582) |>
    dplyr::pull(text) |>
    (\(x) x[x != "to"])() |>
    grepv(pattern = "\\(", x = _, invert = TRUE) |>
    (\(x) ifelse(x == "Unnamed", NA_character_, x))() |>
    (\(x) c(x[1:77], "18", x[78:length(x)]))() |>
    (\(x) ifelse (x == "0921/18", "09/21", x))()

  x_names <- x |>
    (\(x) matrix(data = x[1:36], ncol = 2, byrow = FALSE))()

  x_dates <- x |>
    (\(x) matrix(data = x[37:108], ncol = 4, byrow = TRUE))()

  x_values <- x |>
    (\(x) matrix(data = x[109:144], ncol = 2, byrow = FALSE))()

  y <- df_2022[[33]] |>
    dplyr::filter(y %in% 149:312) |>
    dplyr::pull(text) |>
    (\(x) x[x != "to"])() |>
    grepv(pattern = "\\(|[0-9]{1,2}d|[0-9]{1,2}h|[0-9]{1,2}.[0-9]{1,2}h", x = _, invert = TRUE) |>
    (\(x) ifelse(x == "Unnamed", NA_character_, x))() |>
    (\(x) c(x[1:92], "18", x[93:length(x)]))() |>
    (\(x) c(x[1:42], "04/12", x[43:length(x)]))() |>
    (\(x) ifelse(x == "10/18/18", "10/18", x))()

  y_names <- y |>
    (\(x) matrix(data = x[1:36], ncol = 2, byrow = FALSE))()

  y_dates <- y |>
    (\(x) matrix(data = x[37:108], ncol = 4, byrow = TRUE))()
  
  y_start <- paste0(
    y_dates[ , 1], "/2022 ",
    y_dates[ , 2] |>
      (\(x) ifelse(grepl(pattern = "[0-9]{2}", x = x), paste0(x, "00"), x))()
  ) |>
    strptime(format = "%m/%d/%Y %H%M", tz = "UTC")

  y_end <- paste0(
    y_dates[ , 3], "/2022 ",
    y_dates[ , 4] |>
      (\(x) ifelse(grepl(pattern = "[0-9]{2}", x = x), paste0(x, "00"), x))()
  ) |>
    strptime(format = "%m/%d/%Y %H%M", tz = "UTC")

  y_values <- y |>
    (\(x) matrix(data = x[109:length(x)], ncol = 2, byrow = FALSE))()
    
  df_2022 <- tibble::tibble(
    category_code = y_values[ , 1],
    category_name = NA_character_,
    domestic_name = stringr::str_to_sentence(string = x_names[ , 1]),
    international_name = stringr::str_to_sentence(string = x_names[ , 2]),
    warning_start = y_start,
    warning_end = y_end,
    peak_pressure = as.integer(x_values[ , 2]),
    peak_speed = as.integer(x_values[ , 1])
  ) |>
    dplyr::mutate(
      category_name = factor(
        category_code,
        levels = c("TD", "TS", "STS", "TY", "STY"),
        labels = c(
          "Tropical Depression", "Tropical Storm", "Severe Tropical Storm",
          "Typhoon", "Super Typhoon"
        )
      ),
      category_code = factor(
        category_code,
        levels = c("TD", "TS", "STS", "TY", "STY")
      )
    )
  
  df_2022
}
  
 
#'
#' @rdname cyclones_process
#' @export
#'

cyclones_process_peak_data <- function(path_to_report) {
  year <- stringr::str_extract(path_to_report, pattern = "[0-9]{4}")
  
  parse(
    text = paste0(
      "cyclones_process_peak_", year, "(path_to_report = path_to_report)"
    )
  ) |>
    eval() |>
    dplyr::mutate(year = year, .before = category_code) |>
    dplyr::rename(
      name = domestic_name,
      rsmc_name = international_name,
      start = warning_start,
      end = warning_end,
      pressure = peak_pressure,
      speed = peak_speed
    )
}