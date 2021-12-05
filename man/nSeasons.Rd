\name{nSeasons}
\docType{methods}
\alias{nSeasons}
\alias{nSeasons<-}
\alias{nSeasons-methods}
\alias{nSeasons,slMatrix-method}
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
  \item{\code{signature(object = "slMatrix")}}{{
      %%  ~~describe this method here~~
    }}
  }
}
\examples{
m <- slMatrix(matrix(1:12, nrow = 4))
m
nSeasons(m)
}
\keyword{methods}
\keyword{ts}
