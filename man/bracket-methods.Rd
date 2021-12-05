\name{[-methods}
\docType{methods}
\alias{[-methods}
\alias{[,FlexibleLagged,missing,ANY,ANY-method}
\alias{[,FlexibleLagged,numeric,missing,logical-method}
\alias{[,FlexibleLagged,numeric,missing,missing-method}
\alias{[,Lagged,missing,ANY,ANY-method}
\alias{[,Lagged1d,numeric,ANY,ANY-method}
\alias{[,Lagged2d,numeric,missing,logical-method}
\alias{[,Lagged2d,numeric,missing,missing-method}
\alias{[,Lagged3d,numeric,missing,logical-method}
\alias{[,Lagged3d,numeric,missing,missing-method}
\alias{[,slMatrix,ANY,ANY,ANY-method}
\alias{[,Lagged2d,ANY,ANY,character-method}
\alias{[,Lagged2d,missing,numeric,missing-method}
\alias{[,Lagged2d,numeric,numeric,missing-method}
\alias{[,FlexibleLagged,missing,missing,ANY-method}
\alias{[,ANY,ANY,ANY,ANY-method}
\alias{[,nonStructure,ANY,ANY,ANY-method}
\alias{[,Lagged2d,character,missing,logical-method}
\alias{[,Lagged2d,character,missing,missing-method}
\alias{[,Lagged2d,character,character,missing-method}
\alias{[,Lagged2d,missing,character,missing-method}
\alias{[,Lagged2d,numeric,character,missing-method}
\alias{[,Lagged2d,character,numeric,missing-method}

\title{Methods for subsetting defined in package 'lagged'}
\description{Methods for subsetting  defined in package 'lagged'.}
\section{Methods}{
  Subscripting \code{"Lagged"} objects always drops the
  Lagged-ness.

  When \code{i} is missing, \code{x[]}, returns the underlying
  data. This is equivalent to using \code{x[0:maxLag(x)]}.

  Subscripting (with one index) is defined naturally. It returns the
  suitably subscripted \code{data} slot (for "FlexibleLagged" it is the
  data slot of the data slot).  For indices larger than the maximal lag
  the values are \code{NA}.

  Currently negative indices work similarly to the standard R indexing
  in that negative indices are used to drop elements. However, for
  \eqn{k>0}, using \eqn{-k} as an index drops the element for lag
  \eqn{k-2}, not \eqn{k} (since the subsetting is done by something like
  \code{x@data[i+1]}). This is implementation detail, so it may be
  changed and should not be relied upon.
  

  The following methods for \code{"["} are currently defined in package
  "lagged": 
  \describe{

  \item{\code{signature(x = "FlexibleLagged", i = "missing", j = "ANY", drop = "ANY")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "FlexibleLagged", i = "numeric", j = "missing", 
          drop = "logical")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "FlexibleLagged", i = "numeric", j = "missing", 
          drop = "missing")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "Lagged", i = "missing", j = "ANY", drop = "ANY")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "Lagged1d", i = "numeric", j = "ANY", drop = "ANY")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "Lagged2d", i = "numeric", j = "missing", 
          drop = "logical")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "Lagged2d", i = "numeric", j = "missing", 
          drop = "missing")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "Lagged3d", i = "numeric", j = "missing", 
          drop = "logical")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "Lagged3d", i = "numeric", j = "missing", 
          drop = "missing")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "slMatrix", i = "ANY", j = "ANY", drop = "ANY")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "Lagged2d", i = "ANY", j = "ANY", drop = "character")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "Lagged2d", i = "missing", j = "numeric", 
          drop = "missing")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "Lagged2d", i = "numeric", j = "numeric", 
          drop = "missing")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "FlexibleLagged", i = "missing", j = "missing", 
          drop = "ANY")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "ANY", i = "ANY", j = "ANY", drop = "ANY")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "nonStructure", i = "ANY", j = "ANY", drop = "ANY")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "Lagged2d", i = "character", j = "missing", 
          drop = "logical")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "Lagged2d", i = "character", j = "missing", 
          drop = "missing")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "Lagged2d", i = "character", j = "character", 
          drop = "missing")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "Lagged2d", i = "missing", j = "character", 
          drop = "missing")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "Lagged2d", i = "numeric", j = "character", 
          drop = "missing")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "Lagged2d", i = "character", j = "numeric", 
          drop = "missing")}}{{
%%  ~~describe this method here~~
}
}}
}
\keyword{methods}
