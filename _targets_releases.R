################################################################################
#
# Targets workflow for weekly data releases
#
################################################################################

## Release targets -------------------------------------------------------------

release_targets <- tar_plan(
  ### Create weekly GitHub release ----
  tar_target(
    name = paglaom_weekly_release_tag,
    command = paglaom_create_weekly_release(major = 1),
    cue = tar_cue("always")
  ),
  ### Upload weekly data release ----
  tar_target(
    name = paglaom_weekly_release,
    command = paglaom_upload_weekly_release(
      climate_download_files,
      cyclone_reports_download_files,
      dam_level_data_files,
      heat_index_download_files,
      #tag = paglaom_weekly_release_tag
      tag = "1.8.34"
    )
  )
)
