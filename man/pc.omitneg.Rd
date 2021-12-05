\name{pc.omitneg}
\alias{pc.omitneg}
%\alias{toSeason}
%\alias{toSeasonPair}
%\alias{ttTosl}
%\alias{ttmatToslPairs}
\title{An internal function}
\description{An internal function}
\usage{
pc.omitneg(lags, maxlag)
}
\arguments{
  \item{lags}{ todo: describe! }
  \item{maxlag}{ todo: describe! }
}
\details{

 \code{pc.omitneg} helps to implement dropping of negative indices in
 season-lag objects. It returns its first argument, \code{lags}, if all
 of its elements are non-negative. Otherwise, all elements of
 \code{lags} must be non-positive. In this case the function creates the
 vector \code{0:maxlag} and drops the elements specified by lags.  Note
 that the default indexing will not work properly since zero elements in
 an index are omitted (and there are such indices in season-lag
 objects).

}
\value{
  for \code{pc.omitneg}, a vector of lags (non-negative integers).
}
%\references{
%  % bibentry: boshnakov2009gen
%Boshnakov GN and Iqelan BM (2009).
%``Generation of time series models with given spectral
%properties.''
%\emph{J. Time Series Anal.}, \bold{30}(3), pp. 349--368.
%ISSN 0143-9782, \url{http://dx.doi.org/10.1111/j.1467-9892.2009.00617.x}.
%% end:bibentry:  boshnakov2009gen
%}
\author{Georgi N. Boshnakov}
% \note{
%
%  2013-10-24 - Corrected the description of the return value of
%  \code{ttmatToslPairs}. It incorrectly stated that the first two
%  columns are "tt" pair (they are actually indices in \code{i} and
%  \code{j}).
%
%}
%\seealso{ \code{\link{pcAcvf}} }
\examples{
# ttmatToslPairs
# pc.omitneg
lagged:::pc.omitneg(0:5,10) # 0:5, unchagend since all values >= 0

lagged:::pc.omitneg(-(0:5),10) # 6:10, works like
(0:10)[-(0:5 +1)]     # same

# don't mix positive and negative numbers in pc.omitneg
\dontrun{lagged:::pc.omitneg(c(0,2,3,-4,5), 10)}
}
\keyword{internal}
