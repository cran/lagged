\name{slMatrix}
\alias{slMatrix}
\title{ Function to create objects from class slMatrix }
\description{
  Provides a flexible way to create objects from class
  \code{slMatrix}. The entries may be specified in several ways.
}
\usage{
slMatrix(init = NA, period, maxlag, seasonnames = seq(length = period),
         lagnames = as.character(0:maxlag), periodunit = "season",
         lagunit = "lag", f = NA, type = "sl")
}
\arguments{
  \item{init}{ values for the the autocovariances, see also argument \code{f}. }
  \item{period}{ the number of seasons in an epoch }
  \item{maxlag}{ maximum lag to be stored }
  \item{seasonnames}{ names of seasons (?) }
  \item{lagnames}{ names of lags }
  \item{periodunit}{ name of the period unit  }
  \item{lagunit}{ name of the unit for lags }
  \item{f}{ function to evaluate or matrix to get the values  of the autocovariances.}
  \item{type}{ format  or the arguments of \code{f}, see details.}
}
\details{
  The internal representation of \code{slMatrix} is a matrix slot, \code{m}, of size
  \code{period x (maxlag+1)}. It is created by a call to \code{matrix()}
  with \code{init} supplying the values (may be \code{NA}s). If
  \code{init} is a matrix values for \code{period} and \code{maxlag}
  are deduced (if not supplied) from its size.

  Change on 21/06/2006: Now, if the length of
  \code{init} is smaller than that of \code{m}, the remaining values are
  filled with NA's (in the past  the normal recycling rules of
  \code{matrix()} applied). The previous behaviour used to hide
  puzzling and difficult to track errors. I cannot be sure but this
  change should not affect old code.

  If \code{f} is given it is used to populate the slot
  \code{m} by a call to \code{fill.slMatrix}. Normally in this case
  \code{init=NA} but this is not required.

  Currently \code{fill.slMatrix} has methods for \code{f} of class
  "matrix" and "function". The arguments (or the indices)
  can be   controlled by the argument \code{type}.

  \code{type="sl"} - standard season-lag pair

  \code{type="tt"} - time-time pair

  \code{type="tl"} - standard season-lag pair

}
\value{
  An object of class \code{slMatrix}
}
%\references{ ~put references to the literature/web site here ~ }
\author{Georgi N. Boshnakov}
\note{ To do: additional work is needed on the case when the dimensions
  of init and the result are not the same (see the details section) }
\seealso{
  \code{\link{slMatrix-class}},
  \code{\link{fill.slMatrix}}
}
%\examples{
%}
\keyword{seasonLag}% at least one, from doc/KEYWORDS
