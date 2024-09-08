################################################################################
#
# Targets workflow for forecasts data download, extraction, and processing
#
################################################################################

## Download targets ------------------------------------------------------------

forecasts_download_targets <- tar_plan(
  ### Set PAGASA forecasts pubfiles URL ----
  tar_target(
    name = forecasts_pubfiles_urls,
    command = forecasts_create_urls()
  ),
  ### Download PAGASA forecasts PDF ----
  tar_target(
    name = forecasts_download_files,
    command = forecasts_download(
      url = forecasts_pubfiles_urls,
      directory = "data-raw/forecasts",
      overwrite = FALSE
    ),
    pattern = map(forecasts_pubfiles_urls),
    format = "file"
  )
)


## Processing targets ----------------------------------------------------------
forecasts_processing_targets <- tar_plan(

)


## Analysis targets ------------------------------------------------------------
forecasts_analysis_targets <- tar_plan(
  
)


## Output targets --------------------------------------------------------------
forecasts_output_targets <- tar_plan(
)


## Reporting targets -----------------------------------------------------------
forecasts_report_targets <- tar_plan(
  
)


## Deploy targets --------------------------------------------------------------
forecasts_deploy_targets <- tar_plan(

)
