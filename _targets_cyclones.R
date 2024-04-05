################################################################################
#
# General Targets Workflow
#
################################################################################

## Load libraries and custom functions -----------------------------------------
suppressPackageStartupMessages(source("packages.R"))
for (f in list.files(here::here("R"), full.names = TRUE)) source (f)

## Create targets --------------------------------------------------------------

### Download targets ----

download_targets <- tar_plan(
  tar_target(
    name = cyclone_reports_links,
    command = cyclones_get_report_links()
  ),
  tar_target(
    name = cyclone_reports_download_files,
    command = cyclones_download_report(
      url_link = cyclone_reports_links,
      directory = "data-raw/cyclones"
    ),
    pattern = map(cyclone_reports_links),
    format = "file"
  )
)

### Data targets
data_targets <- tar_plan(
  
)


### Processing targets
processing_targets <- tar_plan(
  
)


### Analysis targets
analysis_targets <- tar_plan(
  
)


### Output targets
output_targets <- tar_plan(
  
)


### Reporting targets
report_targets <- tar_plan(
  
)


### Deploy targets
deploy_targets <- tar_plan(
  
)


## List targets ----------------------------------------------------------------
all_targets()
