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
# Install from your GitHub repo
remotes::install_github("aisolori/ASUbuildR")
```

### Python Dependencies

This package relies on Python libraries which are managed through conda. We use the conda-forge channel exclusively to ensure compatibility and stability:

```bash
# Configure conda to use only conda-forge channel
conda config --set channel_priority strict
conda config --remove-key channels
conda config --add channels conda-forge

# Create and activate environment
conda create -n asubuild python=3.9
conda activate asubuild

# Install required Python packages
conda install -c conda-forge ortools numpy pandas networkx
```

Alternatively, this repository includes a `.condarc` file which will automatically configure conda to use only the conda-forge channel.

## Running the Application

ASUbuildR launches an interactive Shiny application, which you can open
by running this function:

``` r
#ASUbuildR::launch_ASUbuildR()
```