################################################################################
#
# Targets workflow for climate data download, extraction, and processing
#
################################################################################

## Download targets ------------------------------------------------------------

download_targets <- tar_plan(
  ### Set PAGASA climate pubfiles URL ----
  tar_target(
    name = climate_pubfiles_url,
    command = "https://pubfiles.pagasa.dost.gov.ph/pagasaweb/files/cad/"
  ),
  ### Get directories links ----
  tar_target(
    name = climate_directory_urls,
    command = climate_get_pdf_directory_urls(climate_pubfiles_url)
  ),
  ### Get PDF download links ----
  tar_target(
    name = climate_pdf_urls,
    command = climate_get_pdf_urls(climate_directory_urls),
    pattern = map(climate_directory_urls)
  ),
  ### Download climate data ----
  tar_target(
    name = climate_download_files,
    command = climate_download_pdfs(
      pdf_url = climate_pdf_urls,
      directory = "data-raw",
      overwrite = FALSE
    ),
    pattern = map(climate_pdf_urls),
    format = "file",
    error = "continue"
  )
)


## Processing targets ----------------------------------------------------------
processing_targets <- tar_plan(
  tar_target(
    name = pagasa_weather_stations,
    command = get_weather_station_info(climate_download_files)
  ),
  tar_target(
    name = climate_data_normals_1991_2020,
    command = climate_process_1991(climate_download_files)
  ),
  tar_target(
    name = climate_data_extremes_2020,
    command = climate_process_2020(climate_download_files)
  ),
  tar_target(
    name = climate_data_extremes_2021,
    command = climate_process_2021(climate_download_files)
  ),
  tar_target(
    name = climate_data_extremes_2022,
    command = climate_process_2022(climate_download_files)
  ),
  tar_target(
    name = climate_data_extremes_2023,
    command = climate_process_2023(climate_download_files)
  )
)


## Analysis targets ------------------------------------------------------------
analysis_targets <- tar_plan(
  
)


## Output targets --------------------------------------------------------------
output_targets <- tar_plan(
)


## Reporting targets -----------------------------------------------------------
report_targets <- tar_plan(
  
)


## Deploy targets --------------------------------------------------------------
deploy_targets <- tar_plan(

)
