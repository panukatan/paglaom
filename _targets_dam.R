################################################################################
#
# Targets workflow for dam level data extraction, and processing
#
################################################################################

## Data targets ----------------------------------------------------------------
data_targets <- tar_plan(
  ### Set PAGASA dam level URL ----
  tar_target(
    name = dam_level_url,
    command = "https://www.pagasa.dost.gov.ph/flood#dam-information"
  ),
  ### Get dam level data ----
  tar_target(
    name = dam_level_data,
    command = dam_get_level(.url = dam_level_url),
    cue = tar_cue("always")
  ),
  ### Get list of saved/stored dam level CSVs ----
  tar_target(
    name = dam_level_data_files,
    command = list.files(path = "data-raw/dam", full.names = TRUE)
  )
)


## Processing targets ----------------------------------------------------------
processing_targets <- tar_plan(
  ### Processing daily dam level data ----
  tar_target(
    name = dam_level_data_processed,
    command = dam_process_data(dam_level_data_files)
  )
)


## Analysis targets ------------------------------------------------------------
analysis_targets <- tar_plan(
  
)


## Output targets --------------------------------------------------------------
output_targets <- tar_plan(
  ### Output dam level data as CSV ----
  tar_target(
    name = dam_level_data_raw_csv,
    command = dam_archive_raw(dam_level_data),
    format = "file"
  ),
  ### Output processed dam level data as CSV ----
  tar_target(
    name = dam_level_data_csv,
    command = dam_archive_processed(dam_level_data_processed),
    format = "file"
  )
)


### Reporting targets
report_targets <- tar_plan(
  
)


### Deploy targets
deploy_targets <- tar_plan(
  
)
