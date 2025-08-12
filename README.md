
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ASUbuildR

<!-- badges: start -->
<!-- badges: end -->

ASUbuildR provides a GUI interface to equip state to more easily
generate Areas of Substantial Unemployment (ASU) that maximize
unemploymnet within ASUs in a state. This package depends on receiving
an Excel file from the U.S. Bureau of Labor Statistics which includes
their tract-level labor force estimates reflecting the twelve months
ending in June, with preliminary estimates for June, revised estimates
for January-May, and benchmarked estimates for July-December in the
prior year.

## Installation

You can install the development version of ASUbuildR from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("schmidtDETR/ASUbuildR")
```

## Running the Application

ASUbuildR launches an interactive Shiny application, which you can open
by running this function:

``` r
#ASUbuildR::launch_ASUbuildR()
```
