
<!-- README.md is generated from README.Rmd. Please edit that file -->

# An R workflow for curation of Philippine Atmospheric, Geophysical, and Astronomical Services Administration (PAGASA) datasets

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![License for
code](https://img.shields.io/badge/license%20(for%20code)-GPL3.0-blue.svg)](https://opensource.org/licenses/gpl-3.0.html)
[![License for
data](https://img.shields.io/badge/license%20(for%20data)-CC0-blue.svg)](https://creativecommons.org/publicdomain/zero/1.0/)
[![test targets
climate](https://github.com/panukatan/paglaom/actions/workflows/test-targets-climate.yml/badge.svg)](https://github.com/panukatan/paglaom/actions/workflows/test-targets-climate.yml)
[![test targets
cyclones](https://github.com/panukatan/paglaom/actions/workflows/test-targets-cyclones.yml/badge.svg)](https://github.com/panukatan/paglaom/actions/workflows/test-targets-cyclones.yml)
[![test targets
dam](https://github.com/panukatan/paglaom/actions/workflows/test-targets-dam.yml/badge.svg)](https://github.com/panukatan/paglaom/actions/workflows/test-targets-dam.yml)
[![deploy targets
download](https://github.com/panukatan/paglaom/actions/workflows/deploy-targets-downloads.yml/badge.svg)](https://github.com/panukatan/paglaom/actions/workflows/deploy-targets-downloads.yml)
[![deploy targets
releases](https://github.com/panukatan/paglaom/actions/workflows/deploy-targets-releases.yml/badge.svg)](https://github.com/panukatan/paglaom/actions/workflows/deploy-targets-releases.yml)
[![DOI](https://zenodo.org/badge/782627894.svg)](https://doi.org/10.5281/zenodo.10934146)
<!-- badges: end -->

This repository is a
[`docker`](https://www.docker.com/get-started)-containerised,
[`{targets}`](https://docs.ropensci.org/targets/)-based,
[`{renv}`](https://rstudio.github.io/renv/articles/renv.html)-enabled
[`R`](https://cran.r-project.org/) workflow for the retrieval,
processing, and curation of various [Philippine Atmospheric,
Geophysical, and Astronomical Services Administration
(PAGASA)](https://www.pagasa.dost.gov.ph/) publicly available datasets.

## Why `paglaom`?

The word `paglaom` (pronounced as */paɡˈlaʔom/, \[pʌɡˈl̪a.ʔɔm\]*) is
Bisaya (one of up to 187 languages spoken in the Philippines in addition
to Filipino, which is the national language, and English, which is the
language of instruction in the country) for hope.
[PAGASA](https://pagasa.dost.gov.ph), the national meteorological and
hydrological services agency of the Philippines, draws its name from the
Filipino word *pag-asa* which means hope. The repository name choice,
hence, is a play on these words and also a way to showcase the richness
and diversity that exists in the Pilippines.

The `paglaom` project aims to maintain a database of curated datasets on
varios atmospheric, geophysical, and astronomical phenomena that are
made publicly available by PAGASA on their website. These datasets tend
to be summaries of the multitude of data that PAGASA collects on a high
frequency basis. They also tend to be in formats that are not
machine-readable (e.g., PDF, PNG, HTML formats) meant for reporting to
the Philippine population. PAGASA does provide more granular and
expansive datasets through a specific data request process. The
`paglaom` project aims to showcase publicly available PAGASA data that
can be used for various purposes some of which are:

- for students who need to make a report on topics covered by PAGASA’s
  summarised data for a school assignment or project;

- for individuals who have specific interest in one of the natural
  phenomena that PAGASA monitors and would like to get raw summarised
  data in a format that is usable and transferrable into other formats;

- for data visualisation learners and aficionados who want to try on
  working on data about the various natural phenomena available from
  PAGASA and create unique and interesting plots and graphics.

The broader and more blue skies vision of the `paglaom` project is to
contribute to the increasing interest in science, technology,
engineering, and mathematics (STEM) subjects particularly in the
Philippines with a collection that showcases topics and data that are
homegrown and embedded into the fabric of Philippine life.

Whilst the `paglaom` project by its name and the nature of the data it
curates has an inherent Filipino audience, it is hoped that those
outside of the Philippines will also find the information within useful
in similar contexts described above.

## Repository Structure

The project repository is structured as follows:

    paglaom
        |-- .github/
        |-- data/
        |-- data-raw/
        |-- outputs/
        |-- R/
        |-- reports
        |-- renv
        |-- renv.lock
        |-- .Rprofile
        |-- packages.R
        |-- _targets_climate.R
        |-- _targets_cyclones.R
        |-- _targets_dam.R
        |-- _targets_heat.R
        |-- _targets_setup.R
        |-- _targets.R

- `.github` contains project testing and automated deployment of outputs
  workflows via continuous integration and continuous deployment (CI/CD)
  using Github Actions.

- `data/` contains intermediate and final data outputs produced by the
  workflow.

- `data-raw/` contains raw datasets downloaded from publicly available
  PAGASA sources that are used in the project. This directory is empty
  given that the raw datasets from PAGASA are in large file size formats
  that are not ideal for git versioning hence they are git ignored. This
  directory is kept here to maintain reproducibility of project
  directory structure and ensure that the workflow runs as expected when
  run locally.

- `outputs/` contains compiled reports and figures produced by the
  workflow.

- `R/` contains functions developed/created specifically for use in this
  workflow.

- `reports/` contains literate code for R Markdown reports rendered in
  the workflow.

- `renv/` contains `renv` package specific files and directories used by
  the package for maintaining R package dependencies within the project.
  The directory `renv/library`, is a library that contains all packages
  currently used by the project. This directory, and all files and
  sub-directories within it, are all generated and managed by the `renv`
  package. Users should not change/edit these manually.

- `renv.lock` file is the `renv` lockfile which records enough metadata
  about every package used in this project that it can be re-installed
  on a new machine. This file is generated by the `renv` package and
  should not be changed/edited manually.

- `.Rprofile` file is a project R profile generated when initiating
  `renv` for the first time. This file is run automatically every time R
  is run within this project, and `renv` uses it to configure the R
  session to use the `renv` project library.

- `packages.R` file lists out all R package dependencies required by the
  workflow.

- `_targets*.R` files define the steps in the workflow’s data ingest,
  data processing, data analysis, and reporting pipelines.

## Reproducibility

### R package dependencies

This project was built using `R 4.5.2`. This project uses the `renv`
framework to record R package dependencies and versions. Packages and
versions used are recorded in `renv.lock` and code used to manage
dependencies is in `renv/` and other files in the root project
directory. After cloning this repository, start an R session in the
project’s working directory and then run

``` r
renv::restore()
```

to install all R package dependencies.

### Running the workflow

Currently, the project has workflows that curate the following datasets:

1.  Tropical cyclones data for various cyclones entering the Philippine
    area of responsibility since 2017;

2.  Daily heat index data from various data collection points in the
    Philippines;

3.  Climatological extremes and normals data over time;

4.  Daily dam water level data; and,

5.  Daily weather forecasts.

The following diagram illustrates these workflows

Warning: program compiled against libxml 213 using older 209 +
forecasts_download_files declared \[5 branches\] +
cyclone_reports_download_files declared \[6 branches\] +
climate_pdf_urls declared \[5 branches\] + heat_index_download_files
declared \[4 branches\] + climate_download_files declared \[5 branches\]

``` mermaid
graph LR
  style Graph fill:#FFFFFF00,stroke:#000000;
  subgraph Graph
    direction LR
    x8d6dd4935334fdb0["climate_download_files"]:::outdated --> x6140ccf982d3c194(["climate_data_extremes_2020"]):::outdated
    x8d6dd4935334fdb0["climate_download_files"]:::outdated --> x473bf491133a7469(["climate_data_extremes_2021"]):::outdated
    x8d6dd4935334fdb0["climate_download_files"]:::outdated --> x4c23adf6b8b844fc(["climate_data_extremes_2022"]):::outdated
    x8d6dd4935334fdb0["climate_download_files"]:::outdated --> xbd0d87e3059ade21(["climate_data_extremes_2023"]):::outdated
    x8d6dd4935334fdb0["climate_download_files"]:::outdated --> x4613b03fe04d7349(["climate_data_normals_1991_2020"]):::outdated
    xab7eee1b2e061cd6(["climate_pubfiles_url"]):::outdated --> x8eff01740c695153(["climate_directory_urls"]):::outdated
    x6dac381473a34b9b["climate_pdf_urls"]:::outdated --> x8d6dd4935334fdb0["climate_download_files"]:::outdated
    x8eff01740c695153(["climate_directory_urls"]):::outdated --> x6dac381473a34b9b["climate_pdf_urls"]:::outdated
    xe3caf2708d1d6dbd(["cyclone_reports_links"]):::outdated --> xc740241bf91f6d15["cyclone_reports_download_files"]:::outdated
    x9edcec25e3f6632a(["dam_level_url"]):::outdated --> x348d87801300ab37(["dam_level_data"]):::outdated
    xb06f2f43d869006d(["dam_level_data_processed"]):::outdated --> xd34352c7f528a9bd(["dam_level_data_csv"]):::outdated
    xc84ea619aacf6414(["dam_level_data_files"]):::outdated --> xb06f2f43d869006d(["dam_level_data_processed"]):::outdated
    x348d87801300ab37(["dam_level_data"]):::outdated --> xa20495ab08a6ee03(["dam_level_data_raw_csv"]):::outdated
    x6d73550dd73502d5(["forecasts_pubfiles_urls"]):::uptodate --> xb1dd740dbda14dc7["forecasts_download_files"]:::outdated
    x4eb137467026404e(["heat_index_links_dates"]):::outdated --> xcedf87332b7ffc06["heat_index_download_files"]:::outdated
    xa81591ca0f225a93(["heat_index_links_urls"]):::outdated --> xcedf87332b7ffc06["heat_index_download_files"]:::outdated
    x98cb4933c55c918b(["heat_index_pubfiles_url"]):::outdated --> x3e4a66ee3e59e27e(["heat_index_links"]):::outdated
    x3e4a66ee3e59e27e(["heat_index_links"]):::outdated --> x4eb137467026404e(["heat_index_links_dates"]):::outdated
    x3e4a66ee3e59e27e(["heat_index_links"]):::outdated --> xa81591ca0f225a93(["heat_index_links_urls"]):::outdated
    xfcfe66c812f52e12(["forecasts_agriculture_archive_pdfs"]):::uptodate
    x63842c45be329607(["forecasts_agriculture_download_files"]):::outdated
    x0e87409a9b936a1b(["forecasts_archive_pdfs"]):::outdated
  end
```

To run any of these workflows, run the following command on the R
console:

``` r
targets::tar_make(dplyr::starts_with("PREFIX"))
```

replacing `"PREFIX"` with the keyword for the type of data. For example,
to run the cyclones workflow from R console:

``` r
targets::tar_make(dplyr::starts_with("cyclone"))
```

or from the command line/terminal as follows:

``` bash
Rscript -e "targets::tar_make(dplyr::starts_with('cyclone'))"
```

Running specific components of a workflow involves specifying a target
name or target names of the components you want to run. You should be
able to run a full workflow path by just specifying the name of the last
target in the workflow sequence. For example, the following will run the
entire cyclones data workflow (as an alternative to what is shown
above):

``` r
targets::tar_make(cyclones_peak_data_csv)
```

or from the command line/terminal as follows:

``` bash
Rscript -e "targets::tar_make(cyclones_peak_data_csv)"
```

The target `cyclones_peak_data_csv` is the last target of the cyclones
data workflow. Hence, to be able to produce the `cyclones_peak_data_csv`
target requires running this series of linked targets.

If you would like to run a set of interrelated but not fully linked
targets, you will need to specify more than one target name. For this,
you can use `tidyselect` approaches to name targets to be run. For
example:

``` r
targets::tar_make(dplyr::starts_with(c("cyclone", "dam"))
```

will run all targets in the cyclones and dam levels data workflow.

The project also has a workflow for weekly GitHub release of the various
raw datasets.

Warning: program compiled against libxml 213 using older 209

``` mermaid
graph LR
  style Graph fill:#FFFFFF00,stroke:#000000;
  subgraph Graph
    direction LR
    x974e517bdbf7f743(["paglaom_weekly_release_tag"]):::outdated --> x1990b552c5da4b8b(["paglaom_weekly_release"]):::outdated
    
  end
```

## Author

- [Ernest Guevarra](https://github.com/ernestguevarra)

## Licenses

All code created through this project (found in this repository) is
released under a [GPL-3.0
license](https://opensource.org/licenses/gpl-3.0.html) license.

Data provided through this project are released under a
[CC0](https://creativecommons.org/publicdomain/zero/1.0/) license.
