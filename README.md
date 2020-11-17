
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mutVis

<!-- badges: start -->

<!-- badges: end -->

The goal of mutVis is to compare the sequence of an unknown or mutated
protein with a set of known protein sequences, find the best matched
ones, and plot the amino acid differences on a graph.

## Installation

You can install the released version of mutVis from
[GitHub](https://github.com) with:

``` r
# install.packages("devtools")
library(devtools)
install_github("angelawong1526/mutVis")
library("mutVis")
```

To run the shinyApp:

``` r
runMutVisApp()
```

## Overview

``` r
browseVignettes("mutVis")
```

This package contains 6 functions to analyze and compare the input
protein sequence with known protein sequences and to find the best
matches with the input protein. The results are visualized on a scatter
plot. There is one function available for the users: `matchProt()`: The
main plotting function. This function generates a scatter plot with all
the amino acid differences found in known protein sequences. The amino
acids are represented on the x-axis in numerical form; they follow the
order of “A, G, I, L, P, V, F, W, Y, D, E, R, H, K, S, T, C, M, N, Q”,
represented by number 1 through 20 (1 being “A” and 20 being “Q”). The
graph below is generated using a much smaller set of known proteins
(only for this example):

``` r
knitr::include_graphics("~/Downloads/2020-2021/BCB410/mutVis/man/Rplot.png")
```

<img src="~/Downloads/2020-2021/BCB410/mutVis/man/Rplot.png" width="100%" />

## Contributions

``` r
library("mutVis")
lsf.str("package:mutVis")
```

The author of the package is Angela Wong.

## References
