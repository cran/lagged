\name{dataWithLagNames}
\alias{dataWithLagNames}
\title{Lagged data with named lag dimension}
\description{

  Get the data from a Lagged object and ensure that the lag dimension is
  named

}
\usage{
dataWithLagNames(object, prefix = "Lag_")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{an object inheriting from "Lagged".}
  \item{prefix}{a character string.}
}
\details{

  \code{dataWithLagNames()} extracts the data part from a lagged object and
  gives names to the lag dimension, if it is not already named.

  This function is mainly used for programming, particularly in
  \code{show()} methods for lagged objects..

}
\value{
  The data part with names as described in Details.
}
%\references{
%%% ~put references to the literature/web site here ~
%}
\author{Georgi N. Boshnakov}
%\note{
%%%  ~~further notes~~
%}
%
%%% ~Make other sections like Warning with \section{Warning }{....} ~
%
%\seealso{
%%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
x <- Lagged(drop(acf(ldeaths, plot = FALSE)$acf))
## there are no names for the lags:
names(x) # NULL
## but the print method inserts default "Lag_N" names
x
## This sets the names to their defaults:
x1 <- dataWithLagNames(x)
names(x1)
## ... and this sets non-default prefix:
x2 <- dataWithLagNames(x, "L")
names(x2)
x2
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{}
% use one of  RShowDoc("KEYWORDS")
