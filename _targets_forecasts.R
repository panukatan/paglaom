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
      .url = forecasts_pubfiles_urls,
      directory = "data-raw/forecasts",
      overwrite = FALSE
    ),
    pattern = map(forecasts_pubfiles_urls),
    format = "file",
    cue = tar_cue("always")
  ),
  ### Download PAGASA agriculture forecasts PDF ----
  tar_target(
    name = forecasts_agriculture_download_files,
    command = forecasts_agriculture_download(),
    format = "file",
    cue = tar_cue("always")
  )
)


## Data targets ----------------------------------------------------------------
forecasts_data_targets <- tar_plan(
  ### List PAGASA forecasts data files ----
  tar_target(
    name = forecasts_archive_pdfs,
    command = list.files(
      path = "data-raw/forecasts", full.names = TRUE, recursive = TRUE
    ),
    cue = tar_cue("always")
  ),
  ### Extract raw PAGASA forecasts data ----
  # tar_target(
  #   name = forecasts_data_raw,
  #   command = forecasts_get_info(forecasts_archive_pdfs),
  #   pattern = map(forecasts_archive_pdfs),
  #   iteration = "list"
  # ),
  ### List PAGASA agriculture forecasts data files ----
  tar_target(
    name = forecasts_agriculture_archive_pdfs,
    command = list.files(
      path = "data-raw/forecasts_agriculture", 
      full.names = TRUE, recursive = TRUE
    )
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
