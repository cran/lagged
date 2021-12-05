\name{maxLag<-}
\alias{maxLag<--methods}
\alias{maxLag<-}
\alias{maxLag<-,Lagged-method}
\alias{maxLag<-,FlexibleLagged-method}
\docType{methods}
\title{Change the maximal lag in a lagged object}
\description{Change the maximal lag in a lagged object.}
\usage{
maxLag(object, ...) <- value
}
\arguments{
  \item{object}{an object for which this makes sense.}
  \item{\dots}{currently not used.}
  \item{value}{
    the new value of the maximal lag, a non-negative integer number.
  } 
}
\details{
  The replacement version of \code{maxLag()} changes the maximal lag in
  an object to \code{value}. It is a generic function with no default
  method. 

  For the core Lagged classes this is done by simply extending or
  shrinking the data part to the requested value. Subclasses of
  \code{"Lagged"} and other classes, in general, may need more elaborate
  changes. If so, they should define their own methods.
  
  When \code{value} is larger than the current \code{maxLag(object)},
  the entries for the new lags are filled with NA's. 

}
\value{
  the object whose maximal lag is modified as described in Details.
}
\section{Methods}{
\describe{

\item{\code{signature(object = "Lagged")}}{
%%  ~~describe this method here~~
}
\item{\code{signature(object = "FlexibleLagged")}}{{
%%  ~~describe this method here~~
}
}}}
%\references{
%%% ~put references to the literature/web site here ~
%}
\author{Georgi N. Boshnakov}
%\note{
%%%  ~~further notes~~
%}
%
%%% ~Make other sections like Warning with \section{Warning }{....} ~
%
\seealso{
  \code{\link{maxLag}}
}
\examples{
la1 <- Lagged(drop(acf(ldeaths)$acf))
la3 <- la1
la3
## shrink la3
maxLag(la3) # 18
maxLag(la3) <- 5
la3
maxLag(la3) # 5

## extend la3, new entries are filled with NA's
maxLag(la3) <- 10
la3

## alternatively, use "[<-" which accepts any replacement values
la3[11:13] <- 0
la3
}
\keyword{methods}
\keyword{ts}
% use one of  RShowDoc("KEYWORDS")
