
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ROCcurve

<!-- badges: start -->
<!-- badges: end -->

The goal of ROCcurve is to …

## Installation

You can install the development version of ROCcurve from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tswanson222/ROCcurve")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(ROCcurve)

# Fit a model with only one predictor
roc1 <- ROCcurve(y = mtcars$vs, X = mtcars$mpg)

# Or with multiple predictors
roc2 <- ROCcurve(y = mtcars$vs, X = mtcars[, 1:4])

# plot the ROC curve
plot(roc1)
```

<img src="man/figures/README-example-1.png" width="100%" />

``` r
# plot the PR curve
plot(roc1, prc = TRUE)
```

<img src="man/figures/README-example-2.png" width="100%" />

``` r

# Fit PR curves
prc <- ROCcurve(y = mtcars$vs, X = mtcars$mpg, prc = TRUE)
plot(prc)
```

<img src="man/figures/README-example-3.png" width="100%" />
