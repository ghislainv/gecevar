
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `gecevar` R Package <img src="https://ecology.ghislainv.fr/gecevar/logo.svg" align="right" alt="logo-gecevar" width="120" />

[![R-CMD-check](https://github.com/ghislainv/gecevar/workflows/R-CMD-check/badge.svg)](https://github.com/ghislainv/gecevar/actions)
[![Codecov test
coverage](https://codecov.io/gh/ghislainv/gecevar/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ghislainv/gecevar?branch=main)
<!-- [![CRAN Status](https://www.r-pkg.org/badges/version/gecevar)](https://cran.r-project.org/package=gecevar) -->
<!-- [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3253460.svg)](https://doi.org/10.5281/zenodo.3253460) -->
<!-- [![Downloads](https://cranlogs.r-pkg.org/badges/gecevar)](https://cran.r-project.org/package=gecevar) -->

`gecevar` provides a set of climatic and environmental data for a given
area of interest (eg. country scale) that can be used for ecological
analyses. Data come from a variety of sources (eg. Chelsa,
OpenStreetMap, TropicalMoistForest, SRTMv4.1, SoilGrids). Climatic and
environmental data are available as multiband raster files at a
resolution and in the coordinate reference system provided by the user.

## System requirements

Make sure GDAL and GRASS GIS are installed on your system.

## Installation

You can install **gecevar** from
[GitHub](https://github.com/ghislainv/gecevar) with:

``` r
devtools::install_github("ghislainv/gecevar")
```

## Contributing

The `gecevar` R package is Open Source and released under the [GNU GPL
version 3](https://www.gnu.org/licenses/gpl-3.0.en.html) license.
Anybody who is interested can contribute to the package development
following our [Contributing guide](CONTRIBUTING.html). Every contributor
must agree to follow the project’s [Code of
conduct](CODE_OF_CONDUCT.html).
