\name{nSeasons}
\docType{methods}
\alias{nSeasons}
\alias{nSeasons<-}
\alias{nSeasons-methods}
\title{Number of seasons}
\description{Number of seasons.}
\usage{
nSeasons(object)
nSeasons(object, ...) <- value
}
\arguments{
  \item{object}{
    an object for which the notion of number of seasons makes sense.
  }
  \item{value}{a positive integer number.}
  \item{...}{further arguments for methods.}
}
\details{

  These are generic functions.

}
\value{
  an integer number
}
\author{Georgi N. Boshnakov}
\section{Methods}{
\describe{

\item{\code{signature(object = "SarimaFilter")}}{{
%%  ~~describe this method here~~
}
}\item{\code{signature(object = "VirtualArmaFilter")}}{{
%%  ~~describe this method here~~
}
}}}
%\examples{
%ap <- pcts(AirPassengers)
%nSeasons(ap)
%}
\keyword{methods}
\keyword{ts}
