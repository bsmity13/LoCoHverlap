
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LoCoHverlap

<!-- badges: start -->

[![License:
GPL-3](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://cran.r-project.org/web/licenses/GPL-3)
[![](https://img.shields.io/badge/repo%20status-WIP-yellow.svg)](https://www.repostatus.org/#wip)
[![](https://img.shields.io/badge/devel%20version-0.1.0-blue.svg)](https://github.com/bsmity13/LoCoHverlap)
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

  - [ ] Basic workflow
      - [x] Basic function to fit 1 LoCoH
      - [ ] Basic function to rasterize 1 LoCoH
      - [ ] Basic function to calculate EMD between any number of LoCoHs
  - [ ] Seasonal overlap workflow
      - [ ] Iterate basic workflow over individuals in a season
      - [ ] Compare individuals within a season
      - [ ] Compare an individual across seasons
  - [ ] Moving window workflow
      - [ ] Use moving window to implement basic workflow and compare
        consecutive ranges

This README will be updated as new functionality comes online or
workflow is revised.
