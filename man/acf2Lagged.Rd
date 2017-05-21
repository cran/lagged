\name{acf2Lagged}
\alias{acf2Lagged}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convert "acf" objects to "Lagged" objects}
\description{Convert "acf" objects to "Lagged" objects.}
\usage{
acf2Lagged(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    an object from "S3" class "acf", typically obtained from
    \code{acf()} and related functions.
  }
}
\details{

  \code{acf2Lagged()} converts objects produced by \code{acf()} and
  friends to suitable "Lagged" objects.

  If \code{x} represents partial autocorrelations, there is no value for
  lag zero. In the univariate case \code{acf2Lagged()} prepends the
  number 1. In the multivariate case, a matrix of NA's is put
  at lag zero.

}
\value{
  an object from class "Lagged1d" (univariate case)
  or "Lagged3d" (multivariate case)
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
## using examples from help(acf)
lh_acf <- acf2Lagged(acf(lh))

lh_acf[0:5]
acf(lh)$acf[1 + 0:5] # same

acf(ts.union(mdeaths, fdeaths))$acf[15,,]

deaths_mts <- ts.union(mdeaths, fdeaths)
deaths_acf <- acf2Lagged(acf(deaths_mts))
base_acf <- acf(deaths_mts)

deaths_acf[14]
base_acf$acf[1 + 14, , ] # same
}
\keyword{ts}
% use one of  RShowDoc("KEYWORDS")
