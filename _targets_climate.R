################################################################################
#
# Targets workflow for climate data download, extraction, and processing
#
################################################################################

## Setup workflow using project-wide settings ----------------------------------
source("_targets_setup")


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
