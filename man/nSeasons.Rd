\name{nSeasons}
\docType{methods}
\alias{nSeasons}
\alias{nSeasons<-}
\alias{nSeasons-methods}
\alias{nSeasons,slMatrix-method}
\title{Get the number of seasons from an object}
\description{Get the number of seasons from an object.}
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

  Methods for \code{nSeasons} are straightforward when the property
  makes sense for objects from a class. In contrast, methods for the
  replacement version, \code{`nSeasons<-`}, should be defined carefully
  and may not even be feasible. 
}
\value{
  an integer number
}
\author{Georgi N. Boshnakov}
\section{Methods}{

  No methods for \code{`nSeasons<-`} are defined in package
  \pkg{lagged}. The methods defined for \code{nSeasons} are given below.
  
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
