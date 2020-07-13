# ggcertara

An R package to provide a standardized look for plots employed by
pharmacometricians.  It provides
a [ggplot2](https://CRAN.R-project.org/package=ggplot2) theme, color palette,
and a collection of plotting functions for basic goodness-of-fit diagnostic
plots.

## Installation

This package needs to be installed from GitHub:

``` r
require(remotes)
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
remotes::install_github("certara/ggcertara", upgrade="never")
```

## Usage

There is a [vignette](https://certara.github.io/ggcertara/vignettes/ggcertara-gof.html)
that explains how to use the package for basic goodness-of-fit diagnostic plots.


