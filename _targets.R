################################################################################
#
# Overall targets workflow for PAGASA data processing and archiving
#
################################################################################

## Setup workflow using project-wide settings ----------------------------------
source("_targets_setup.R")


## Create targets and list targets objects -------------------------------------


### Cyclones targets ----
source("_targets_cyclones.R")


### Climate targets ----
source("_targets_climate.R")


### Dam targets ----
source("_targets_dam.R")


### Heat Index targets ----
source("_targets_heat.R")


### Forecasts targets ----
source("_targets_forecasts.R")


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


### Releases targets ----
source("_targets_releases.R")


## List targets ----------------------------------------------------------------
all_targets()
