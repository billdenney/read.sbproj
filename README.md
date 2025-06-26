# read.sbproj

<!-- badges: start -->
[![R-CMD-check](https://github.com/billdenney/read.sbproj/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/billdenney/read.sbproj/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/billdenney/read.sbproj/graph/badge.svg)](https://app.codecov.io/gh/billdenney/read.sbproj)
<!-- badges: end -->

The goal of read.sbproj is to allow you to read Matlab SimBiology projects into
R.

## Installation

You can install the development version of read.sbproj from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("billdenney/read.sbproj")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(read.sbproj)
read_sbproj(system.file("example/TfRBACE_model_ver3p5.sbproj", package = "read.sbproj"))
```
