
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

To start the interpreter type at the R prompt:

``` r
basic()
```

It is easier to use R from the terminal in RStudio because that does not
enable autocompletion.

Paste the following text into the BASIC prompt:

    10 FOR I = 1 TO 10
    20 PRINT I
    30 NEXT I
    40 END

To see the listing type `LIST` and to run the script type `RUN`. Use ESC
or Ctrl-C to exit the BASIC interpreter.

To list and run a demo script type at the R prompt:

``` r
b <- basic('hello.bas', home_dir = system.file('scripts', package = 'basic'))
b$list()
#> 5 REM HELLO WORLD PROGAM 
#> 10 PRINT "HELLO WORLD" 
#> 99 END
b$run()
#> HELLO WORLD
```
