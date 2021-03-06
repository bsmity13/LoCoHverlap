
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LoCoHverlap

<!-- badges: start -->

[![License:
GPL-3](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://cran.r-project.org/web/licenses/GPL-3)
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![](https://img.shields.io/badge/devel%20version-0.1.0-blue.svg)](https://github.com/bsmity13/LoCoHverlap)
[![R-CMD-check](https://github.com/bsmity13/LoCoHverlap/workflows/R-CMD-check/badge.svg)](https://github.com/bsmity13/LoCoHverlap/actions)
<!-- badges: end -->

`LoCoHverlap` (pronounced “Loke - Overlap”) is an R package to compare
animal range overlaps using Local Convex Hull (LoCoH) home range
estimators and Earth Mover’s Distance (EMD). It streamlines the
following workflow:  
1\. Fit LoCoH home ranges to sets of animal locations.  
2\. Convert fitted LoCoHs to utilization distributions (UD) estimated on
a raster.  
3\. Measure overlap using the continuous metric EMD.

A full description of this method and its use in animal ecology is
currently in prep.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("bsmity13/LoCoHverlap")
```

## Current Status

This package is in the early stages of development and is not yet
functional. The currently planned updates are:

  - [x] Basic workflow
      - [x] Basic function to fit 1 LoCoH
      - [x] Basic function to rasterize 1 LoCoH
      - [x] Basic function to calculate EMD between any number of LoCoHs
  - [ ] Seasonal overlap workflow
      - [ ] Iterate basic workflow over individuals in a season
      - [ ] Compare individuals within a season
      - [ ] Compare an individual across seasons
  - [x] Moving window workflow
      - [x] Use moving window to compare consecutive ranges of an
        individual along a track
  - [x] Simulated example data to be used in help files and vignettes
  - [ ] Vignette for each workflow
      - [ ] Basic
      - [ ] Seasonal
      - [ ] Moving window
  - [ ] Parallelize workflow

This README will be updated as new functionality comes online or
workflow is revised.
