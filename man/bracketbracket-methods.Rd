\name{[[-methods}
\docType{methods}
\alias{[[-methods}
\alias{[[,Lagged,numeric-method}
\title{Methods for '[[' in package 'lagged'}
\description{Methods for '[[' in package 'lagged'.}
\section{Methods}{

  Indexing with "[[" returns the value for the specified lag.
  The index should be a single number. 

  This is the recommended way to extract the value at a single index.


\describe{

\item{\code{signature(x = "Lagged", i = "numeric")}}{
%%  ~~describe this method here~~
}
}}
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
}
\keyword{methods}
