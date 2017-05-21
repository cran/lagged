\name{Lagged}
\alias{Lagged}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create Lagged objects}
\description{
  Create objects inheriting from Lagged.
}
\usage{
Lagged(data, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{suitable data, see Details.}
  \item{\dots}{further arguments passed on to \code{new()}.}
}
\details{

  \code{Lagged} creates an object inheriting from \code{"Lagged"}. The
  exact class depends on argument \code{data}.

  If \code{data} is a vector, matrix or 3D array, the result is
  "Lagged1d", "Lagged2d" and "Lagged3d", respectively.
  If \code{data} inherits from \code{"Lagged"}, the result is
  "FlexibleLagged".

}
\value{
  a suitable \code{"Lagged"} object, as described in Details
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{Georgi N. Boshnakov}
\note{
  I am considering making \code{Lagged} generic.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\linkS4class{Lagged1d}},
\code{\linkS4class{Lagged2d}},
\code{\linkS4class{Lagged3d}}
}
\examples{
## adapted from examples for acf()
a1 <-  drop(acf(ldeaths)$acf)
la1 <- Lagged(a1)
class(la1)

acv2 <- acf(ts.union(mdeaths, fdeaths))
la2 <- Lagged(acv2)
la2[1]
acv2$acf[2, ,] # same

## indexing in acf() is somewhat misterious, so some examples:
n <- length(mdeaths)
tmpcov <- sum((mdeaths - mean(mdeaths)) * (fdeaths - mean(fdeaths)) ) / n
msd <- sqrt(sum((mdeaths - mean(mdeaths))^2)/n)
fsd <- sqrt(sum((fdeaths - mean(fdeaths))^2)/n)
tmpcov1 <- sum((mdeaths - mean(mdeaths))[2:n] * (fdeaths - mean(fdeaths))[1:(n-1)] ) / n
tmpcov1 / (msd * fsd)
la2[[1]][1,2] == tmpcov1 / (msd * fsd) # FALSE, but:
la2[[1]][1,2] - tmpcov1 / (msd * fsd)  # only numerically different

la2[[1]][1,2] == acv2$acf[2, 1, 2] # TRUE

la2[0]
acv2[0]

la2[1]
acv2[1]

acf(ts.union(mdeaths, fdeaths))[0]
acf(ts.union(mdeaths, fdeaths))[1]
}
\keyword{ts}
% use one of  RShowDoc("KEYWORDS")
