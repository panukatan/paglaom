################################################################################
#
# General Targets Workflow
#
################################################################################

## Setup workflow using project-wide settings ----------------------------------
source("_targets_setup.R")


## Download targets ------------------------------------------------------------

download_targets <- tar_plan(
  ### Get download links ----
  tar_target(
    name = cyclone_reports_links,
    command = cyclones_get_report_links()
  ),
  ### Download reports ----
  tar_target(
    name = cyclone_reports_download_files,
    command = cyclones_download_report(
      url_link = cyclone_reports_links,
      directory = "data-raw"
    ),
    pattern = map(cyclone_reports_links),
    format = "file"
  )
)


## Data targets ----------------------------------------------------------------
data_targets <- tar_plan(
  ### Process cyclones peak data ----
  tar_target(
    name = cyclones_peak_data,
    command = cyclones_process_peak_data(
      path_to_report = cyclone_reports_download_files
    ),
    pattern = map(cyclone_reports_download_files)
  ),
  ### Output cyclones peak data as CSV ----
  tar_target(
    name = cyclones_peak_data_csv,
    command = {
      write.csv(
        cyclones_peak_data, file = "data/cyclones.csv", row.names = FALSE
      )
      "data/cyclones.csv"
    },
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


## List targets ----------------------------------------------------------------
all_targets()
