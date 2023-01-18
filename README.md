
<!-- README.md is generated from README.Rmd. Please edit that file -->

# abcdutils

<!-- badges: start -->

<!-- badges: end -->

The goal of abcdutils is to …

## Installation

You can install the development version of abcdutils from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("psvelayudhan/abcdutils")
```

## Examples

``` r
library(abcdutils)

# Search the NDA's data dictionary from R
search_dd("traumatic brain injury")

# Go to the data dictionary page of a dataframe based on its short name
abcd_dd("abcd_otbi01")
```
