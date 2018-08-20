\name{[-methods}
\docType{methods}
\alias{[-methods}
\alias{[,FlexibleLagged,ANY-method}
\alias{[,FlexibleLagged,missing-method}
\alias{[,Lagged1d,numeric-method}
\alias{[,Lagged2d,numeric-method}
\alias{[,Lagged3d,numeric-method}
\alias{[,Lagged,missing-method}
\alias{[,ANY,ANY-method}
\alias{[,nonStructure,ANY-method}
\alias{[,slMatrix,ANY-method}
\title{Methods for subsetting defined in package 'lagged'}
\description{Methods for subsetting  defined in package 'lagged'.}
\section{Methods}{
  Subscripting \code{"Lagged"} objects always drops the
  Lagged-ness.

  When \code{i} is missing, \code{x[]}, returns the underlying
  data. This is equivalent to using \code{x[0:maxLag(x)]}.

  Subscripting (with one index) is defined naturally. It returns the
  suitably subscripted \code{data} slot (for "FlexibleLagged" it is the
  data slot of the data slot). In package \pkg{sarima} this is
  \code{"numeric"} or \code{"array"}.  For indices larger than the
  maximal lag the values are \code{NA}.

  Currently negative indices work similarly to the standard R indexing
  in that negative indices are used to drop elements. However, for
  \eqn{k>0}, using \eqn{-k} as an index drops the element for lag
  \eqn{k-2}, not \eqn{k} (since the subsetting is done by something like
  \code{x@data[i+1]}). This is implementation detail, so it may be
  changed and should not be relied upon.

  The following methods for \code{"["} are currently defined in package
  "lagged": 
  \describe{

    \item{\code{signature(x = "FlexibleLagged", i = "missing")}}{
      %%  ~~describe this method here~~
    }

    \item{\code{signature(x = "FlexibleLagged", i = "ANY")}}{
      %%  ~~describe this method here~~
    }

    \item{\code{signature(x = "Lagged1d", i = "numeric")}}{
      %%  ~~describe this method here~~
    }
    
    \item{\code{signature(x = "Lagged2d", i = "numeric")}}{
      %%  ~~describe this method here~~
    }
    
    \item{\code{signature(x = "Lagged3d", i = "numeric")}}{
      %%  ~~describe this method here~~
    }

    \item{\code{signature(x = "Lagged", i = "missing")}}{
      %%  ~~describe this method here~~
    }
    \item{\code{signature(x = "slMatrix", i = "ANY")}}{{
	Describe any discrepancy with the Lagged methods.
      }
}}}
\keyword{methods}
