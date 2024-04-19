################################################################################
#
# Targets workflow for dam level data extraction, and processing
#
################################################################################

## Setup workflow using project-wide settings ----------------------------------
source("_targets_setup.R")


## Create targets and list targets objects -------------------------------------

### Data targets
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
  ### Output dam level data as CSV ----
  tar_target(
    name = dam_level_data_csv,
    command = {
      write.csv(
        dam_level_data, 
        file = paste0("data/dam_level_", Sys.Date(), ".csv"), 
        row.names = FALSE
      )
      paste0("data/dam_level_", Sys.Date(), ".csv")
    },
    format = "file"
  )
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


## List targets
all_targets()
