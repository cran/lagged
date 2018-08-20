\name{Lagged-class}
\Rdversion{1.1}
\docType{class}
\alias{Lagged-class}
%\alias{[,Lagged,missing-method}
%\alias{[,Lagged,numeric-method}
%\alias{[<-,Lagged,numeric-method}
%\alias{maxLag,Lagged-method}
%\alias{[[,Lagged,numeric-method}
%\alias{[[<-,Lagged,numeric-method}

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
    \item{\code{data}:}{Object of class \code{"ANY"} Subclasses of
      "Lagged" may restrict the class of this slot.}
  }
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "Lagged", i = "missing")}:
      In this case (i.e., \code{i} is missing) \code{[]}, returns the
      underlying data. This is equivalent to using
      \code{x[1:maxLag(x)]}.
    }
    %\item{[}{\code{signature(x = "Lagged", i = "numeric")}:
    %
    %  Subscripting (with one index) is defined naturally. It returns the
    %  suitably subscripted \code{data} slot. Note that the returned
    %  value is not from class "Lagged" but from that of the data slot
    %  (in package \pkg{sarima} this is \code{"numeric"} or
    %  \code{"array"}.  For indices larger than the maximal lag the
    %  values are NA.
    %
    %  Currently negative indices work similarly to the standard R
    %  indexing in that negative indices are used to drop
    %  elements. However, for \eqn{k>0}, using \eqn{-k} as an index drops
    %  the element for lag \eqn{k-2}, not \eqn{k} (since the subsetting
    %  is done by something like \code{x@data[i+1]}). This may be changed
    %  and should not bee relied upon.
    %
    %}
    \item{[<-}{\code{signature(x = "Lagged", i = "numeric")}:

      Replace the values for the specified indices with the new
      values.

    }

    \item{maxLag}{\code{signature(object = "Lagged")}:

      Gives the maximal lag in the object.

    }

    \item{[[}{\code{signature(x = "Lagged", i = "numeric")}: ... }
    \item{[[<-}{\code{signature(x = "Lagged", i = "numeric")}: ... }
    %\item{[<-}{\code{signature(x = "Lagged", i = "missing")}: ... }
  }
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
%\examples{
%showClass("Lagged")
%}
\keyword{classes}


