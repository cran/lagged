\name{slMatrix-class}
\docType{class}
\alias{slMatrix-class}
%\alias{[,slMatrix,ANY,ANY,ANY-method}
%\alias{[<-,slMatrix-method}
%\alias{maxLag,slMatrix-method}
%\alias{nSeasons,slMatrix-method}

\title{Class "slMatrix" }
\description{ \code{slMatrix} is a matrix-like object for storing values
  of periodic autocovariance functions, i.e. of functions of two
  arguments which are periodic in the first argument,
  \code{r[t,k]=r[t+d,k]}. The first argument has the meaning of "time" or
  "season" (when taken modulo the period),
  the second is "lag".
  This class provides various access and assignment methods for
  such objects. slMatrix was created as the storage for values of
  periodic autocovariance functions and is used for other related quantities.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("slMatrix", m)},
  where \code{m} is a matrix with m[i,k] giving the values for season i
  and lag (k-1), \eqn{k=1,2,\dots}{k=1,2,...}.
  The number of rows in m is taken to be the number of
  seasons. The function \code{slMatrix} provides several ways to specify
  the data for the slMatrix object.
}
\section{Slots}{
	 \describe{
    \item{\code{m}:}{Object of class \code{"matrix"}. }
  }
}
\section{Methods}{
  \describe{
    \item{[<-}{\code{signature(x = "slMatrix", i = "ANY", j = "ANY",
	value = "ANY")}: ... }

    \item{[}{\code{signature(x = "slMatrix", i = "ANY", j = "ANY", drop
	= "ANY")}:

      The indexing method is quite flexible and allows to extract parts
      of \code{slMatrix} objects in a variety of ways. It returns an
      ordinary matrix or, if \code{drop = TRUE}, vector.

      The syntax for indexing is similar to that for ordinary matrices
      with some features specific to the periodic nature of the first
      index. The named parameters are \code{i}, \code{j}, and
      \code{type}. Both \code{i} and \code{j} can be vectors. The
      interpretation of \code{i} and \code{j} depends on \code{type}.

      \code{x[i,j]} (or \code{x[i,j,type="sl"]}) refers to the value for
      season \code{i} and lag \code{j}.  This is referred to as standard
      season-lag pair, meaning that the elements of \code{i} must be in
      the range 1,...,d, where d is the number of seasons and the lags
      must be non-negative. Negative indices have the usual effect of
      removing the corresponding elements. A zero element for lag is
      admissible.

      \code{x[i,j,type="tl"]} is similar to "sl" but \code{i} is allowed
      to take any (integer) values. These are reduced modulo the number
      of seasons to the 1,...,d, range.

      \code{x[i,j,type="tl+-"]} This allows also the lags to be
      negative.

      \code{x[i,j,type="co"]} ("co" stands for "coefficient") This
      assumes that the values for negative lags and lags larger than
      \code{maxlag} are 0. If assignment is attempted for such lags, a
      message is issued and the assignment is ignored.

      \code{x[i,j,type="tt"]} both arguments have the meaning of
      time. If \code{i} and \code{j} are scalars the pair \code{i,j} is
      converted to standard \code{s,l} pair and the value assigned to
      the relevant element. If \code{i} and/or \code{j} are vectors,
      they are crossed and the procedure is done for each pair.

      If several values need to be assigned to the same \code{s,l} pair
      a warning is isssued if these values are not all equal.

      Obviously, whereever negative arguments are allowed, elements to
      omit cannot be specified with negative indices.

      see \code{\link{[-methods}}. % ]

    }
    \item{maxLag}{\code{signature(x = "slMatrix")}:
      maximum lag available for storage. }

  }
}

\author{Georgi N. Boshnakov}
\note{

  The current implementation of the indexing is inefficient, I simply
  added features and patches as the need arose. Maybe some day I will
  replace it with \code{C} code.

}

\seealso{
	\code{\link{slMatrix}}%,
	% \code{\link{PeriodicVector-class}}
}
\examples{
m1 <- rbind(c(1, 0.81, 0), c(1, 0.4972376, 0.4972376))
x <- slMatrix(m1)
x[1, 0]
x[1:2, 0:1]
x[1:3, 1:3, type = "tt"]
}
\keyword{classes}
