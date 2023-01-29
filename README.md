
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BASIC interpreter

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

This package allows you to edit and run scripts written in Dartmouth
BASIC (1964). The package contains the grammar of the language, a lexer,
a parser, and an interpreter. It is a port of the Python implementation
by David Beasley to R and uses the R package rly.

## Installation

You can install the current version of `basic` from Github by entering
the following commands into R:

``` r
if (!require(devtools)) install.packages("devtools")
devtools::install_github("paulemms/basic")
```

## Usage

``` r
library(basic)
packageVersion("basic")
#> [1] '0.0.0.9000'
```

To start the interpreter in the R console type:

``` r
basic()
```

To run a demo script type:

``` r
basic('hello.bas')
#> HELLO WORLD
```
