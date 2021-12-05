\name{sl2acfbase}
\alias{sl2acfbase}
\alias{acfbase2sl}
\alias{sl2vecacf}
\title{Convert between vector and season lag representations}
\description{

  Convert between vector and season-lag representations of
  autocovariances of multivariate and periodically correlated time
  series.

}
\usage{
sl2acfbase(mat, maxlag, fullblocks = FALSE)

acfbase2sl(acf)

sl2vecacf(mat, maxlag, fullblocks = FALSE)
}
\arguments{
  \item{acf}{an acf as returned by base R \code{acf}.}
  \item{mat}{
    a matrix containing autocovariances in season-lag arrangement.
  }
  \item{maxlag}{maximal lag, a positive integer.}
  \item{fullblocks}{if TRUE, keep full blocks only.}
}
\details{
  
  These functions rearrange autocovariances and autocorrelations between
  the native season-lag arrangement in package ``pcts'' and the vector
  representations of the corresponding mutivariate models (vector of
  seasons representation of periodic models). Variable \eqn{s} is taken
  be season \eqn{s} and vice versa in the opposite direction.

  ``acfbase'' in the names of the functions refers to the representation
  returned by base function \code{acf}.

  \code{acfbase2sl} rearranges a multivariate acf in season-lag form.

  \code{sl2acfbase} rearranges a season-lag form into the multivariate
  form used by base function \code{acf}.

  \code{sl2vecacf} is similar to \code{sl2acfbase} but the result is
  such that the lag is in the third dimension and \code{r[ , , k]} is
  \eqn{Cov(X_{t}, X_{t-k})} (not its transpose). See also the examples
  below and in \code{\link{acf2Lagged}}.

}
\value{
  for \code{acfbase2sl}, a matrix.

  for \code{sl2acfbase} and \code{sl2vecacf}, an array.
}
%\references{
%%% ~put references to the literature/web site here ~
%}
\author{Georgi N. Boshnakov}
%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
## use a character matrix to illustrate the positions of the elements
matsl <- rbind(paste0("Ra", 0:3), paste0("Rb", 0:3))
matsl
## convert to what I consider "standard" vec format R(k)=EX_tX_{t-k}'
sl2vecacf(matsl)
## convert to the format from acf() (R(k) is the transposed from mine).
sl2acfbase(matsl)
identical(sl2vecacf(matsl), aperm(sl2acfbase(matsl), c(3, 2, 1))) # TRUE

## by default the conversion is lossles;
## so this contains all values from the original and some NA's:
sl2acfbase(matsl)
## the orignal, matsl, can be restored:
acfbase2sl(sl2acfbase(matsl))
identical(acfbase2sl(sl2acfbase(matsl)), matsl)  # TRUE

## this drops some values (if necessary) to keep complete block only
sl2acfbase(matsl, fullblocks = TRUE)
}
\keyword{acf}


