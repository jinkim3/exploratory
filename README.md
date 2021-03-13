
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ‘exploratory’: an R Package

<!-- badges: start -->

[![CRAN
checks](https://cranchecks.info/badges/summary/exploratory)](https://cran.r-project.org/web/checks/check_results_exploratory.html)
[![Travis build
status](https://travis-ci.com/jinkim3/exploratory.svg?branch=master)](https://travis-ci.com/jinkim3/exploratory)
[![R build
status](https://github.com/jinkim3/exploratory/workflows/R-CMD-check/badge.svg)](https://github.com/jinkim3/exploratory/actions)

[![Total
Downloads](http://cranlogs.r-pkg.org/badges/grand-total/exploratory?color=blue)](https://cran.r-project.org/package=exploratory)
[![](https://img.shields.io/github/last-commit/jinkim3/exploratory.svg)](https://github.com/jinkim3/exploratory/commits/master)
[![CodeFactor](https://www.codefactor.io/repository/github/jinkim3/exploratory/badge)](https://www.codefactor.io/repository/github/jinkim3/exploratory)
<!-- badges: end -->

#### Exploratory Analysis Tool for Behavioral Science Researchers

With one simple command, this tool launches a Shiny App on the local
machine. Using the App’s point-and-click interface, drag and drop
variables in a data set to categorize them as possible independent,
dependent, moderating, or mediating variables. Then run dozens (or
hundreds) of analyses instantly to uncover any statistically significant
relationships among variables.

This tool also allows conducting simple follow-up analyses (e.g.,
correlation and regression) with the point-and-click interface.

## Warning

Any relationship among variables uncovered using this tool should be
tested in follow-up studies. This tool is designed only to facilitate
exploratory analyses and should NEVER be used for p-hacking (Simmons,
Nelson, & Simonsohn, 2011; Ioannidis 2005).

## Installation

You can install the released version of the package ‘exploratory’ from
[CRAN](https://cran.r-project.org/package=exploratory) with:

``` r
install.packages("exploratory")
```

You can also install the development version from [exploratory on
GitHub](https://github.com/jinkim3/exploratory) with:

``` r
install.packages("devtools")
devtools::install_github("jinkim3/exploratory")
```

If you run into errors while using the package, try updating the package
to the most recent version available on [exploratory on
GitHub](https://github.com/jinkim3/exploratory) with:

``` r
update_exploratory()
```

## Example

Here is an example of using this package.

``` r
library(exploratory)

# update the package 'exploratory'
update_exploratory()

# launch the exploratory analysis tool on a local machine
exploratory(data = mtcars)
```
