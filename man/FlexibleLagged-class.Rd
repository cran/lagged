\name{FlexibleLagged-class}
\Rdversion{1.1}
\docType{class}
\alias{FlexibleLagged-class}
%\alias{[,FlexibleLagged,ANY-method}
%\alias{[,FlexibleLagged,missing-method}
%\alias{[<-,FlexibleLagged,missing-method}
%\alias{[<-,FlexibleLagged,numeric-method}

\title{Class FlexibleLagged}
\description{Class FlexibleLagged.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("FlexibleLagged",
  data, ...)},
see also convenience function \code{\link{Lagged}},


%%  ~~ describe objects here ~~
"FlexibleLagged" is used mainly in programming as a superclass of
classes which need to inherit from all "Lagged" classes.
It can represent objects from any subclass of
"Lagged". Methods are defined, such that the internal representation is
transparent.
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class \code{"Lagged"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{Lagged}"}, directly.
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "FlexibleLagged", i = "ANY")}: ... }
    \item{[}{\code{signature(x = "FlexibleLagged", i = "missing")}: ... }
    \item{[<-}{\code{signature(x = "FlexibleLagged", i = "missing")}: ... }
    \item{[<-}{\code{signature(x = "FlexibleLagged", i = "numeric")}: ... }
	 }
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
\code{\linkS4class{Lagged1d}},
\code{\linkS4class{Lagged2d}},
\code{\linkS4class{Lagged3d}}
}
\examples{
## Lagged1d
v <- 1:12
v_lagged <- Lagged(v)
v_lagged
identical(v_lagged, new("Lagged1d", data = v)) # TRUE
v_lagged[0:2] # v[1:3]
v_lagged[[0]] # 1

## Lagged2d
m <- matrix(1:12, nrow = 4)
m_lagged <- Lagged(m)
m_lagged
identical(m_lagged, new("Lagged2d", data = m)) # TRUE
m_lagged[0] # matrix with 1  column
m_lagged[[0]] # numeric

## Lagged3d
a <- array(1:24, dim = c(2, 3, 4))
a_lagged <- Lagged(a)
identical(a_lagged, new("Lagged3d", data = a)) # TRUE

dim(a_lagged[0]) # c(2,3,1)
a_lagged[0]
a[ , , 1, drop = FALSE]

dim(a_lagged[[0]]) # c(2,3)
a_lagged[[0]]
a[ , , 1, drop = TRUE]

## as above "FlexibleLagged"
## 1d
v_flex <- new("FlexibleLagged", data = v)
identical(v_flex@data, v_lagged) # TRUE
v_flex[0]   # = v_lagged[0]
v_flex[[0]] # = v_lagged[[0]]

## 2d
m_flex <- new("FlexibleLagged", data = m)
identical(m_flex@data, m_lagged) # TRUE
m_flex[0]   # = m_lagged[0]
m_flex[[0]] # = m_lagged[[0]]

## 3d
a_flex <- new("FlexibleLagged", data = a)
identical(a_flex@data, a_lagged) # TRUE
a_flex[0]   # = a_lagged[0]
a_flex[[0]] # = a_lagged[[0]]
}
\keyword{classes}


