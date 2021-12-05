\name{Lagged1d-class}
\Rdversion{1.1}
\docType{class}
\alias{Lagged1d-class}
%\alias{[,Lagged1d,numeric-method}
%\alias{[<-,Lagged1d,numeric-method}

\title{Class Lagged1d}
\description{Class Lagged1d.}
\section{Objects from the Class}{
  
  Objects can be created by calls of the form \code{Lagged(v)} or
  \code{new("Lagged1d", data = v)}, where \code{v} is a vector.
  \code{new("Lagged1d", ...)} also works.
  
%%  ~~ describe objects here ~~
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class \code{"vector"}.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{Lagged}"}, directly.
}
\section{Methods}{
  \describe{
    \item{[<-}{\code{signature(x = "Lagged1d", i = "numeric")}: ... }
    \item{[}{\code{signature(x = "Lagged1d", i = "numeric", j = "ANY", drop = "ANY")}: ... }
    \item{show}{\code{signature(object = "Lagged1d")}: ... }
    \item{whichLagged}{\code{signature(x = "Lagged1d", y = "missing")}: ... }}
}
%\references{
%%%  ~~put references to the literature/web site here~~
%}
\author{Georgi N. Boshnakov}
%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Lagged}},
\code{\linkS4class{Lagged2d}},
\code{\linkS4class{Lagged3d}}
}
\examples{
v <- cos(2*pi*(0:10)/10)
new("Lagged1d", data = v) ## ok, but Lagged() is more convenient
x <- Lagged(v)
class(x) # Lagged1d
x
x[0]
x[0:3]
}
\keyword{classes}
