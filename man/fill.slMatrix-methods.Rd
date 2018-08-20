\name{fill.slMatrix-methods}
\docType{methods}
\alias{fill.slMatrix}
\alias{fill.slMatrix-methods}
\alias{fill.slMatrix,ANY,ANY-method}
\alias{fill.slMatrix,matrix,function-method}
\alias{fill.slMatrix,matrix,matrix-method}
\title{ Methods for Function fill.slMatrix in Package `sarima' }
\description{
  Provides various ways for filling a matrix object with values
  according to the conventions of class slMatrix. This is a generic
  function. There are methods for values given in a matrix or computed
  by a function. The arguments of the function or the indices of the
  matrix may follow a variety of conventions.
}
\usage{
fill.slMatrix(m, f, period, maxlag, type)
}
\arguments{
  \item{m}{ the matrix to be filled with values }
  \item{f}{ a function whose evaluation produces the entries for m }
  \item{period}{ number of seasons in a cycle }
  \item{maxlag}{ the maximal lag }
  \item{type}{ the indexing convention - "sl" (default) stands for a
    standard season-lag pair, "tt" is for time-time pair, "tl" is for
    time-lag pair, see ... for details. }
}
\details{
  The internal representation for autocovariances and other quantities
  related to periodic correlation is a matrix with one row for each
  season. Methods for this generic provide a way to convert external
  structures to this format.

  \code{slMatrix} calls \code{fill.slMatrix} when called with
  appropriate argument \code{f}.

  todo: need better explanation here!
}
\value{
  the matrix filled with the values as required
}

\section{Methods}{
\describe{
\item{\code{signature(m = "ANY", f = "ANY")}}{
  Issue an error message and stop. The default behaviour is not to allow
  filling with values from unknown objects.
}
\item{\code{signature(m = "matrix", f = "function")}}{
  Fill the values by evaluating a function.  The arguments of the
  function may be in "sl", "tt" or "tl" format.
}
\item{\code{signature(m = "matrix", f = "matrix")}}{
  Fill the values of the slMatrix object from a matrix. The entries may
  be arranged in "sl", "tt" or "tl" format.
}
}}
\references{
  % bibentry: boshnakov2009gen
Boshnakov GN and Iqelan BM (2009).
``Generation of time series models with given spectral
properties.''
\emph{J. Time Series Anal.}, \bold{30}(3), pp. 349--368.
ISSN 0143-9782, \url{http://dx.doi.org/10.1111/j.1467-9892.2009.00617.x}.
% end:bibentry:  boshnakov2009gen

  % bibentry: boshnakov2012pld
Boshnakov GN and Lambert-Lacroix S (2012).
``A periodic Levinson-Durbin algorithm for entropy maximization.''
\emph{Computational Statistics \& Data Analysis}, \bold{56}, pp. 15--24.
ISSN 0167-9473, \url{http://dx.doi.org/10.1016/j.csda.2011.07.001}, \url{http://www.sciencedirect.com/science/article/pii/S0167947311002556}.
% end:bibentry:  boshnakov2012pld
}
\author{Georgi N. Boshnakov}
\seealso{
  \code{\link{slMatrix}}
}
\examples{
# :todo:
}
\keyword{methods}
%\keyword{seasonLag}
\keyword{internal}







