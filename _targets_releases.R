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
      tag = paglaom_weekly_release_tag
    )
  )
)
