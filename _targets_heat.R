################################################################################
#
# Targets workflow for heat index data download, extraction, and processing
#
################################################################################

## Data targets ----------------------------------------------------------------
data_targets <- tar_plan(
  ### Set PAGASA heat index pubfiles URL ----
  tar_target(
    name = heat_index_pubfiles_url,
    command = "https://pubfiles.pagasa.dost.gov.ph/iaas/heat_index/"
  ),
  ### Get download links ----
  tar_target(
    name = heat_index_links,
    command = heat_index_get_image_urls(heat_index_pubfiles_url),
    cue = tar_cue("always")
  ),
  ### Get download URLs ----
  tar_target(
    name = heat_index_links_urls,
    command = heat_index_links$links
  ),
  ### Get download dates ----
  tar_target(
    name = heat_index_links_dates,
    command = heat_index_links$date
  ),
  ### Download heat index images ----
  tar_target(
    name = heat_index_download_files,
    command = heat_index_download_image_files(
      heat_index_url = heat_index_links_urls,
      .date = heat_index_links_dates,
      directory = "data-raw",
      overwrite = FALSE
    ),
    pattern = map(heat_index_links_urls, heat_index_links_dates),
    format = "file"
  )  
)


## Processing targets ----------------------------------------------------------
processing_targets <- tar_plan(
  
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
