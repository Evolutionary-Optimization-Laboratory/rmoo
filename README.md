
<!-- README.md is generated from README.Rmd. Please edit that file -->
# rmoo - R Multi-Objective Optimization <img src="man/figures/logo.png" align="right" width="150px" alt=""/>

<!-- badges: start -->
[![R build status](https://github.com/Evolutionary-Optimization-Laboratory/rmoo/workflows/R-CMD-check/badge.svg)](https://github.com/Evolutionary-Optimization-Laboratory/rmoo/actions) [![CRAN status](https://www.r-pkg.org/badges/version/rmoo)](https://CRAN.R-project.org/package=rmoo) [![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

<!-- badges: end -->
## Overview

A Non-Dominated Sorting based Multi-Objective Optimization package, built upon the ['GA' package](https://CRAN.R-project.org/package=GA).

'rmoo' provides a complete and flexible framework for optimizing multiple supplied objectives. You will have at your disposal a wide range of configuration options for the NSGA, NSGA-II and NSGA-III algorithms, as well as representation of real numbers, permutations and binaries.

## Installation

You can install the **stable** version on [R CRAN](https://cran.r-project.org/package=rmoo):

``` r
install.packages("rmoo")
```

Or you can install the **development** version from [GitHub](https://github.com/Evolutionary-Optimization-Laboratory/rmoo):

``` r
# install.packages("devtools")
devtools::install_github("Evolutionary-Optimization-Laboratory/rmoo")
```

## Usage

A simple example of running nsga3 solving the DTLZ1 problem:

``` r
library(rmoo)

DTLZ1 <- function (x, nobj = 3) 
{
    if (is.null(dim(x))) {
        x <- matrix(x, 1)
    }
    n <- ncol(x)
    y <- matrix(x[, 1:(nobj - 1)], nrow(x))
    z <- matrix(x[, nobj:n], nrow(x))
    g <- 100 * (n - nobj + 1 + rowSums((z - 0.5)^2 - cos(20 * 
        pi * (z - 0.5))))
    tmp <- t(apply(y, 1, cumprod))
    tmp <- cbind(t(apply(tmp, 1, rev)), 1)
    tmp2 <- cbind(1, t(apply(1 - y, 1, rev)))
    f <- tmp * tmp2 * 0.5 * (1 + g)
    return(f)
}

result <- nsga3(fitness = DTLZ1,
                type = "real-valued",
                lower = c(0,0,0),
                upper = c(1,1,1),
                popSize = 92,
                n_partitions = 12,
                maxiter = 300)
                
pcp(object = result)
```

![](https://github.com/Evolutionary-Optimization-Laboratory/rmoo/blob/master/man/figures/README-example-1.jpeg)<!-- -->

``` r
#Scatter without optimal points
scatter(object = result)
```

![](https://github.com/Evolutionary-Optimization-Laboratory/rmoo/blob/master/man/figures/README-example-2.png)<!-- -->

``` r
#Scatter with optimal points (Using reference points as optimal points)
scatter(object = result, optimal = result@reference_points)
```

![](https://github.com/Evolutionary-Optimization-Laboratory/rmoo/blob/plotting/man/figures/README-example-3.png)<!-- -->

``` r
#Polar Coordinates
polar(fitness = result@fitness[1:3,])
```

![](https://github.com/Evolutionary-Optimization-Laboratory/rmoo/blob/plotting/man/figures/README-example-4.png)<!-- -->

``` r
#Headmap Plot
heat_map(fitness = result@fitness[1:3,])
```

![](https://github.com/Evolutionary-Optimization-Laboratory/rmoo/blob/plotting/man/figures/README-example-5.png)<!-- -->
