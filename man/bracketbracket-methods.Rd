\name{[[-methods}
\docType{methods}
\alias{[[-methods}
%\alias{[[,Lagged,numeric-method}
%\alias{[[,slMatrix,numeric-method}
\alias{[[,FlexibleLagged,ANY,ANY-method}
\alias{[[,Lagged,numeric,missing-method}
\alias{[[,Lagged2d,numeric,logical-method}
\alias{[[,Lagged2d,numeric,numeric-method}
\alias{[[,slMatrix,numeric,ANY-method}
\alias{[[,Lagged2d,missing,numeric-method}
\alias{[[,Lagged2d,numeric,missing-method}
\alias{[[,FlexibleLagged,missing,numeric-method}
\alias{[[,FlexibleLagged,numeric,missing-method}
\title{Methods for '[[' in package 'lagged'}
\description{Methods for '[[' in package 'lagged'.}
\section{Methods}{

  Indexing with "[[" returns the value for the specified lag.
  The index should be a single number. 

  This is the recommended way to extract the value at a single index.


\describe{

\item{\code{signature(x = "FlexibleLagged", i = "ANY", j = "ANY")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "Lagged", i = "numeric", j = "missing")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "Lagged2d", i = "numeric", j = "logical")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "Lagged2d", i = "numeric", j = "numeric")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "slMatrix", i = "numeric", j = "ANY")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "Lagged2d", i = "missing", j = "numeric")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "Lagged2d", i = "numeric", j = "missing")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "FlexibleLagged", i = "missing", j = "numeric")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(x = "FlexibleLagged", i = "numeric", j = "missing")}}{{
%%  ~~describe this method here~~
}
}}}
\examples{
## for 1d objects the difference of '[' and '[[' is not visible
(acv1 <- acf2Lagged(acf(ldeaths, plot = FALSE)))
acv1[1]
acv1[[1]]

## in higher dimenions it matters
acv2 <- acf2Lagged(acf(ts.union(mdeaths, fdeaths), plot = FALSE))
acv2[0]   # array
acv2[[0]] # matrix

## as in standard R conventions, '[' can select arbitrary number of elements
acv2[0:1]
## ... while '[[' selects only one, so this is an error:
\dontrun{
acv2[[0:1]]
}

## Lagged2
m <- matrix(10:49, nrow = 4, byrow = TRUE)
m_lagged <- Lagged(m)
m_lagged

## one index: lag
m_lagged[0:1]
m_lagged[0]   # column vector
m_lagged[[0]] # numeric
## two indices: first is row, second is lag
m_lagged[1 , 0]     # '[' doesn't drop dimensions
m_lagged[1 , 0:3]

m_lagged[[1 , 0]]   # '[[' does drop dimensions
m_lagged[[1 , 0:3]]
m_lagged[[1, TRUE]] # the whole first row, as numeric

m_lagged[1:2 , 0:3] # ok, a matrix
## ... but the first arg. of '[[' must be of length one,
## so this throws error:
\dontrun{
m_lagged[[1:2 , 0:3]]
}
}
\keyword{methods}
