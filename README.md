[![CRANStatusBadge](http://www.r-pkg.org/badges/version/lagged)](https://cran.r-project.org/package=lagged)
[![R-CMD-check](https://github.com/GeoBosh/lagged/workflows/R-CMD-check/badge.svg)](https://github.com/GeoBosh/lagged/actions)
[![codecov](https://app.codecov.io/gh/GeoBosh/lagged/branch/master/graph/badge.svg?token=2SW9HKG71Y)](https://app.codecov.io/gh/GeoBosh/lagged)

R package 'lagged' provides classes and methods for objects, like autocovariances, whose
natural indexing starts from zero.


# Installing lagged

The [latest stable version](https://cran.r-project.org/package=lagged) is on CRAN. 

    install.packages("lagged")

The vignette shipping with the package gives illustrative examples.
`vignette("Guide_lagged", package = "lagged")`.

You can install the [development version](https://github.com/GeoBosh/lagged) of `lagged` from Github:

    library(devtools)
    install_github("GeoBosh/lagged")


# Overview

The package provides several classes with methods for indexing starting from zero. Objects
can be created with the function `Lagged()`. It returns a suitable Lagged object from a class
suitable for the argument:

    library(lagged)
    v_lagged <- Lagged(0:6)                           # 1d object
    m_lagged <- Lagged(matrix(1:12, nrow = 4))        # 2d object
    a_lagged <- Lagged(array(1:24, dim = c(4,3,2)))  # 3d object

It recognises also `"acf"` objects from base R time series functions:

    ap_lagged <- Lagged(acf(AirPassengers, plot = FALSE))

The maximal lag stored in the object can be obtained with `maxLag()`:

    maxLag(v_lagged)
    maxLag(m_lagged)
    maxLag(a_lagged)

The length of the objects is equal to `maxlag(object) + 1`.

    length(v_lagged)
    length(m_lagged)
    length(a_lagged)

Subsetting with `"["` drops the laggedness and returns vector, matrix, or array, depending on
the dimension of the object. 
Subsetting with one index gives the data for the requested lags:

    tmp <- v_lagged[0:2]
    tmp <- m_lagged[0:2]
    tmp <- a_lagged[0:1]

Values beyond the maximal lag are `NA`. 
Dimensions are not dropped if an extent has length one (i.e. `drop = FALSE`):

    v_lagged[0]
    m_lagged[0]
    a_lagged[0]

To drop dimensions, use "[[":

    v_lagged[[0]]
    m_lagged[[0]]
    a_lagged[[0]]

Arithmetic operations and mathematical functions are defined naturally on lagged
objects. The shorter one is extended with `NA`'s to the length of the longer. 

Operations between lagged and base R objects are defined, as well. However, it is an error to
do operations between objects whose dimensions do not match, unless the base R object is a
scalar, or, more generally, has the size of `x[[0] ]`.

