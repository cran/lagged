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
%\examples{
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
% use one of  RShowDoc("KEYWORDS")
