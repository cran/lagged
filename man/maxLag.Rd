\name{maxLag}
\docType{methods}
\alias{maxLag}
\alias{maxLag-methods}
\alias{maxLag,Lagged-method}
\alias{maxLag,array-method}
\alias{maxLag,matrix-method}
\alias{maxLag,vector-method}
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
}}}
%\note{}
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
%\examples{
%}
\keyword{methods}
\keyword{ts}
