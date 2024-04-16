################################################################################
#
# General Targets Workflow
#
################################################################################

## Load libraries and custom functions -----------------------------------------
suppressPackageStartupMessages(source("packages.R"))
for (f in list.files(here::here("R"), full.names = TRUE)) source (f)


## Download targets ------------------------------------------------------------

download_targets <- tar_plan(
  ### Set PAGASA heat index pubfiles URL ----
  tar_target(
    name = heat_index_pubfiles_url,
    command = "https://pubfiles.pagasa.dost.gov.ph/iaas/heat_index/"
  ),
  ### Get download links ----
  tar_target(
    name = heat_index_links,
    command = heat_index_get_image_urls(heat_index_pubfiles_url)
  ),
  ### Download heat index images ----
  tar_target(
    name = heat_index_download_files,
    command = heat_index_download_image_files(
      heat_index_url = heat_index_links$links,
      .date = heat_index_links$date,
      directory = "data-raw",
      overwrite = FALSE
    ),
    pattern = map(heat_index_links$links, heat_index_links$date),
    format = "file"
  )
)


## Data targets ----------------------------------------------------------------
data_targets <- tar_plan(
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


## List targets ----------------------------------------------------------------
all_targets()
