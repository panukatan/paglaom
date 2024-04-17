################################################################################
#
# Overall targets workflow for PAGASA data processing and archiving
#
################################################################################

## Setup workflow using project-wide settings ----------------------------------
source("_targets_setup.R")


## Create targets and list targets objects -------------------------------------

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


## List targets
all_targets()
