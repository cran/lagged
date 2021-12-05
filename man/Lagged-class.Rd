\name{Lagged-class}
\Rdversion{1.1}
\docType{class}
\alias{Lagged-class}

\title{Class Lagged}
\description{Class Lagged.}
\section{Objects from the Class}{

This class serves as a base class for objects with natural
indexing starting from zero. It is a virtual class, no objects can be
created from it.

Arithmetic and other operations are defined. They return objects
strictly from the core "Lagged" classes, even if the arguments are from
classes inheriting from the core "Lagged" classes. Of course, for such classes
specialised methods can be defined to keep the class when appropriate.
For example, the sum of two autocovariance functions is an
autocovariance function, but their difference may not be a valid one.

In arithmetic operations between "Lagged" objects the arguments are made
of equal length by filling in NA's.  When one of the operands is not
"Lagged", the recycling rule is applied only if that argument is a
singleton.
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class \code{"ANY"}. Subclasses of
      "Lagged" may restrict the class of this slot.}
  }
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "Lagged", i = "missing", j = "ANY", drop = "ANY")}:
      In this case (i.e., \code{i} is missing) \code{[]}, returns the
      underlying data. This is equivalent to using
      \code{x[1:maxLag(x)]}.
    }
    \item{maxLag}{\code{signature(object = "Lagged")}:
      Gives the maximal lag in the object.
    }

    \item{[[}{\code{signature(x = "Lagged", i = "numeric")}: ... }
    \item{[[<-}{\code{signature(x = "Lagged", i = "numeric")}: ... }
    %\item{[<-}{\code{signature(x = "Lagged", i = "missing")}: ... }
  
    \item{[<-}{\code{signature(x = "Lagged", i = "missing")}: ... }
    \item{coerce}{\code{signature(from = "Lagged", to = "array")}: ... }
    \item{coerce}{\code{signature(from = "Lagged", to = "matrix")}: ... }
    \item{coerce}{\code{signature(from = "Lagged", to = "vector")}: ... }
    \item{Math}{\code{signature(x = "Lagged")}: ... }
    \item{Math2}{\code{signature(x = "Lagged")}: ... }
    \item{maxLag<-}{\code{signature(object = "Lagged")}: ... }
    \item{Ops}{\code{signature(e1 = "FlexibleLagged", e2 = "Lagged")}: ... }
    \item{Ops}{\code{signature(e1 = "Lagged", e2 = "FlexibleLagged")}: ... }
    \item{Ops}{\code{signature(e1 = "Lagged", e2 = "Lagged")}: ... }
    \item{Ops}{\code{signature(e1 = "Lagged", e2 = "missing")}: ... }
    \item{Ops}{\code{signature(e1 = "Lagged", e2 = "vector")}: ... }
    \item{Ops}{\code{signature(e1 = "vector", e2 = "Lagged")}: ... }
    \item{Summary}{\code{signature(x = "Lagged")}: ... }}
}
%\references{
%%%  ~~put references to the literature/web site here~~
%}
\author{Georgi N. Boshnakov}
%\note{
%%%  ~~further notes~~
%}
%
%%% ~Make other sections like Warning with \section{Warning }{....} ~
%
\seealso{
  function \code{\link{Lagged}} which creates objects from suitable
    subclasses of \code{"Lagged"},
    and also
    \code{\linkS4class{Lagged1d}},
    \code{\linkS4class{Lagged2d}},
    \code{\linkS4class{Lagged3d}}
}
\examples{
Lagged(1:12)                   # => Lagged1d
Lagged(matrix(1:12, ncol = 3)) # => Lagged2d
Lagged(array(1:24, dim = 2:4))  # => Lagged3d

## equivalently:       
new("Lagged1d", data = 1:12)                   # => Lagged1d
new("Lagged2d", data = matrix(1:12, ncol = 3)) # => Lagged2d
new("Lagged3d", data = array(1:24, dim = 2:4)) # => Lagged3d
}
\keyword{classes}


