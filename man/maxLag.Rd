\name{maxLag}
\docType{methods}
\alias{maxLag}
\alias{maxLag-methods}
\alias{maxLag,Lagged-method}
\alias{maxLag,array-method}
\alias{maxLag,matrix-method}
\alias{maxLag,vector-method}
\alias{maxLag,ANY-method}
\alias{maxLag,slMatrix-method}
\title{
  Give the maximal lag in an object
}
\description{
  Give the maximal lag in an object, such as autocorrelations.
}
\usage{
maxLag(object, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{an object, for which the function makes sense.}
  \item{\dots}{ not used? }
}
\details{

  \code{maxLag} is a generic function to get the maximal lag for which
  information is available in lagged objects.

}
\value{
  a non-negative integer
}
%\references{ ~put references to the literature/web site here ~ }
\author{Georgi N. Boshnakov}
\section{Methods}{
\describe{

\item{\code{signature(object = "Lagged")}}{
    This method applies to all classes inheriting from \code{"Lagged"}.
}
\item{\code{signature(object = "array")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(object = "matrix")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(object = "vector")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(object = "ANY")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(object = "slMatrix")}}{{
%%  ~~describe this method here~~
}
}}}
%\note{}
\seealso{
  \code{"\link{maxLag<-}"}
}
\examples{
## 1d
v <- Lagged(2^(0:6))
v
maxLag(v)
v[c(2,4,6)]
v[8] # NA
## reduce the number of lags in place
maxLag(v) <- 4
v
## extend the object (with NA's)
maxLag(v) <- 6
v
## extend using "["
v[5:8] <- 2^(5:8)
v

## 2d
m <- Lagged(matrix(1:12, nrow = 4))
m
maxLag(m)
maxLag(m) <- 1
m
## maxLag(m) <- 4 # extending this way doesn't work currently
}
\keyword{methods}
\keyword{ts}
