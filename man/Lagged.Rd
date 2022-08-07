\name{Lagged}
\alias{Lagged}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create Lagged objects}
\description{
  Create objects inheriting from Lagged.
}
\usage{
Lagged(data, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{suitable data, see Details.}
  \item{\dots}{further arguments passed on to \code{new()}.}
}
\details{

  \code{Lagged} creates an object inheriting from \code{"Lagged"}. The
  exact class depends on argument \code{data}. This is
  the easiest way to create lagged objects.

  If \code{data} is a vector, matrix or 3D array, the result is
  "Lagged1d", "Lagged2d" and "Lagged3d", respectively.
  If \code{data} inherits from \code{"Lagged"}, the result is
  "FlexibleLagged".

}
\value{
  a suitable \code{"Lagged"} object, as described in Details
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{Georgi N. Boshnakov}
\note{
  I am considering making \code{Lagged} generic.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  the specific classes
\code{\linkS4class{Lagged1d}},
\code{\linkS4class{Lagged2d}},
\code{\linkS4class{Lagged3d}}
}
\examples{
## a discrete distribution with outcomes 0,1,2,3,4,5,6:
v <- dbinom(0:6, size = 6, p = 0.5)
bin6 <- Lagged(v)

## names are used, if present:
names(v) <- paste0("P(x=", 0:6, ")")
bin6a <- Lagged(v)
bin6a
## subsetting drops the laggedness:
bin6a[1:3]
bin6a[]
bin6a[0:2]
## to resize (shrink or extend) the object use 'maxLag<-':
s8 <- s2 <- bin6a
maxLag(s2) <- 2
s2
## extending inserts NA's:
maxLag(s8) <- 8
s8
## use assignment to extend with specific values:
s8a <- bin6a
s8a[7:8] <- c("P(x=7)" = 0, "P(x=8)" = 0)
s8a

## adapted from examples for acf()
## acf of univariate ts
acv1 <- acf(ldeaths, plot = FALSE)
class(acv1) # "acf"
a1 <-  drop(acv1$acf) 
class(a1) # numeric

la1 <- Lagged(a1) # 1d lagged object from 'numeric':
Lagged(acv1)      # 1d lagged object from 'acf':

## acf of multivariate ts
acv2 <- acf(ts.union(mdeaths, fdeaths), plot = FALSE)
class(acv2) # "acf"
a2 <-  acv2$acf
class(a2) # 3d array
dim(a2)

la2a <- acf2Lagged(acv2) # explicitly convert 'acf' to lagged object
Lagged(acv2)            # equivalently, just use Lagged()
identical(la2a, Lagged(acv2)) # TRUE

la2a[0]    # R_0,  indexing lagged object
a2[1, , ]  # same, indexing array data from 'cf' object
acv2[0]    # same, indexing 'acf' object

la2a[1]    # R_1
a2[2, , ]  # same
acv2[1/12] # transposed, see end of this example section as to why use 1/12

la2a[2]    # R_1
a2[3, , ]  # same
acv2[2/12] # transposed, see end of this example section as to why use 1/12

## multiple lags
la2a[0:1]        # native indexing with lagged object
a2[1:2, , ]      # different ordering
acv2$acf[1:2, ,] # also different ordering

## '[' doesn't drop a dimension even when it is of length 1:
la2a[0]
la2a[1]

## to get a singleton element, use '[[':
la2a[[0]]
la2a[[1]]

## arithmetic and math operators
-la1
+la1

2*la1
la1^2
la1 + la1^2

abs(la1)
sinpi(la1)
sqrt(abs(la1))

## Summary group
max(la1)
min(la1)
range(la1)
prod(la1)
sum(la1)
any(la1 < 0)
all(la1 >= 0)

## Math2 group
round(la1)
round(la1, 2)
signif(la1)
signif(la1, 4)

## The remaining examples below are only relevant
## for users comparing to indexing of 'acf'
## 
## indexing in base R acf() is somewhat misterious, so an example with
## DIY computation of lag_1 autocovariance matrix
n <- length(mdeaths)
tmpcov <- sum((mdeaths - mean(mdeaths)) * (fdeaths - mean(fdeaths)) ) / n
msd <- sqrt(sum((mdeaths - mean(mdeaths))^2)/n)
fsd <- sqrt(sum((fdeaths - mean(fdeaths))^2)/n)
tmpcov1 <- sum((mdeaths - mean(mdeaths))[2:n] *
               (fdeaths - mean(fdeaths))[1:(n-1)] ) / n
tmpcov1 / (msd * fsd)

la2a[[1]][1,2] - tmpcov1 / (msd * fsd)  # only numerically different
## the raw acf in the 'acf' object is not surprising:
la2a[[1]][1,2] == acv2$acf[2, 1, 2] # TRUE
## ... but this probably is:
acv2[1]
## the ts here is monthly but has unit of lag 'year'
## so acv2[1] asks for lag 1 year = 12 months, thus
acv2[1/12]
all( acv2$acf[13, , ] == drop(acv2[1]$acf) )    # TRUE
all( acv2$acf[2, , ]  == drop(acv2[1/12]$acf) ) # TRUE
all( la2a[[1]]        == drop(acv2[1/12]$acf) ) # TRUE



}
\keyword{programming}
\keyword{ts}
% use one of  RShowDoc("KEYWORDS")




