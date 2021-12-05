setGeneric("fill.slMatrix", def = function(m, f, period, maxlag, type){ standardGeneric("fill.slMatrix") } )

## 2017-06-02 Moving the definition of =nSeasons()=  from package "pcts" to "sarima"
## 2018-08-10 Moving the definition of =nSeasons()=  from package "sarima" to "lagged"
## TODO: decide whether to define nSeasons() in 'lagged'
##       If so, uncomment here, remove the method from 'pcts',
##       and the def. of nSeasons() from 'sarima'
setGeneric("nSeasons", def = function(object){ standardGeneric("nSeasons") } )
setGeneric("nSeasons<-", def = function(object, ..., value){ standardGeneric("nSeasons<-") } )

######################################################## begin: support functions for slMatrix
pc.omitneg <-
  function(lags,maxlag){
    if(any(lags<0)){
      if(any(lags>0))
        stop("Do not mix positive and negative lags when type=\"sl\"!")
      else
        lags <- setdiff(0:maxlag,-lags)                              # cater for negative j's.
    }
    lags
  }

# 2011-07-07 - slagam ... args, za da e savmestimo s "[", vzh setMethod, koyto izp. tazi f.
#              orkomentiram ponezhe ya slozhich direktno v tyaloto na setMethod.
# pc.f.index <-
#   function(x,i=1:pc.nseasons(x), j=0:maxLag(x), type = "sl", ..., drop = TRUE){
#                                                           # "s" is for "season, "l" is for
#                                                           # "lag", "t" is for "time"
#     y <- x@m
#     period <- pc.nseasons(x)
#     switch(type,
#            "sl" = {season <- i
#                    lag <- pc.omitneg(j,ncol(x)-1)
#                    res <- y[season,lag+1]                # lag+1 because lags start from zero.
#                   },
#            "tt" = {
#              res <- myouter(i,j, function(ii,jj){wrk <- toSeasonPair(ii,jj,period)
#                                                       season <- wrk$season
#                                                       lag <- wrk$lag
# 					              y[season,lag+1]
# 						 }
#                               )
# 		 },
#            "tl" = {season <- toSeason(i,period)
#                   lag <- j
#                   res <- y[season,lag+1]      # lag+1 because lags start from zero.
#                  },
#            "tl+-" = {
#                  if(length(j)==1){
#                    if(j>=0){                   # this works only for scalar  j
#                      season <- toSeason(i,period)
#                      lag <- j
#                    }else{
#                      season <- toSeason(i-j,period)
#                      lag <- -j
#                    }
#                    res <- y[season,lag+1]      # lag+1 because lags start from zero.
#                  }else{
#                    res <- matrix(NA,nrow=length(i),ncol=length(j))
#                    for(k in 1:length(j)){
#                      if(j[k]>=0){                   # this works only for scalar  j
#                        season <- toSeason(i,period)
#                        lag <- j[k]
#                      }else{
#                        season <- toSeason(i-j[k],period)
#                        lag <- -j[k]
#                      }
#                      res[,k] <- y[season,lag+1]      # lag+1 because lags start from zero.
#                    }
#                  }
#                  },
#            "t+l,l+-" = {res <- matrix(NA,nrow=length(i),ncol=length(j))
#                         for(k in 1:length(j)){
#                           res[,k] <- x[i+j[k],j[k],type="tl+-"]
#                         }
#                       },
#            "co" = {season <- toSeason(i,period)
#                    lag <- j
#                    if(lag<0 || lag>maxLag(x) )
#                      res <- 0
#                    else{
#                      res <- y[season,lag+1]      # lag+1 because lags start from zero.
#                    }
#                  },
#           stop("Invalid pctime type of index. type is one of \"sl\", \"tt\" or \"tl\".")
#     )
#     res
# }

## 2016-01-19 removing the default values for i and j, they do not work now
## > m1 <- slMatrix(matrix(1:6, nrow = 2))
## > m1
## An object of class "slMatrix"
## Slot "m":
##       lag
## season 0 1 2
##      1 1 3 5
##      2 2 4 6
##
## > m1[ , 1] <- 10:11
## Error in .local(x, i, j, ..., value = value) (from pc20slMatrix.r#91) :
##   argument "i" is missing, with no default
pc.f.index.replace <-                                                 # indexing (replacement)
function(x, i, j, type = "sl", ... ,value){
       # function(x, i=1:pc.nseasons(x), j=0:maxLag(x), type = "sl", ... ,value){
# admissible combinations for type are "sl", "tl", "tt".
#
  naflag <- missing(i) & missing(j)
  ## 2013-04-01 - seems unfinished, the assignment for "period" is missing, hardly on purpose,
  ##                                I put it now.   todo: check all this!

  if(missing(i))
      i <- 1:nSeasons(x)

  if(missing(j))
      j <- 0:maxLag(x)

  period <- nSeasons(x)

  switch(type,
          "sl" = {season <- i
                  lag <- pc.omitneg(j,ncol(x)-1)
                  x@m[season,lag+1] <- value   # lag+1 because lags start from zero.
                 },
          "tl" = {season <- toSeason(i, period)
                  lag <- pc.omitneg(j,ncol(x)-1)
                  x@m[season,lag+1] <- value   # lag+1 because lags start from zero.
                 },
           "tl+-" = {
                 if(length(j)==1){
                   if(j>=0){                   # this works only for scalar  j
                     season <- toSeason(i,period)
                     lag <- j
                   }else{
                     season <- toSeason(i-j,period)
                     lag <- -j
                   }
                   x@m[season,lag+1] <- value     # lag+1 because lags start from zero.
                 }else{
                   res <- matrix(NA,nrow=length(i),ncol=length(j))
                   for(k in 1:length(j)){
                     if(j[k]>=0){                   # this works only for scalar  j
                       season <- toSeason(i,period)
                       lag <- j[k]
                     }else{
                       season <- toSeason(i-j[k],period)
                       lag <- -j[k]
                     }
                     x@m[season,lag+1] <- value[,k]     # lag+1 because lags start from zero.
                   }
                 }
                 },
           "co" = {season <- toSeason(i, period)
                   lag <- j
                   if(lag<0 || lag>maxLag(x) ){
                     print("attempt to assign a coefficient value at negative/large index.")
                     print("attempt ignored, value not assigned.")
                   }else
                     x@m[season,lag+1] <- value
                 },
          "tt" = {
                  if(naflag){                # to do: add special treatment for scalar i and j
                    wrk <- ttTosl(value, period)
                    x@m <- wrk
                  }else if(length(value) == 1 && length(i)==1 && length(j)==1){
                    wrk <- toSeasonPair(i,j, period)
                    season <- wrk$season
                    lag <- wrk$lag
		    x@m[season,lag+1] <- value
                  }else{
                    wrk <- ttmatToslPairs(i,j, period)
                    if( length(value) == 1){
                      value <- matrix(value,nrow = length(i), ncol = length(j) )
                    }else if( is.vector(value) ){
                      if(length(i)==1)
                        tmp5 <- rep(value,each=length(j))
                      if(length(j)==1)
                        tmp5 <- rep(value,length(j))
                      value <- matrix(tmp5,nrow=length(i),ncol=length(j))
                    }
                    for(k1 in 1:nrow(wrk)){
                       if( (! is.na( x@m[ wrk[k1,3],wrk[k1,4]+1 ] ) ) &&
                           ( x@m[ wrk[k1,3],wrk[k1,4]+1 ] != value[ wrk[k1,1], wrk[k1,2] ] )
                          ){
                      warning("ttTosl: replaced existing value of acf for a season-lag pair.")
                       }
                       x@m[ wrk[k1,3],wrk[k1,4]+1 ] <- value[ wrk[k1,1], wrk[k1,2] ]
                    }
                  }
 		 },
          stop("Invalid pctime type of index. type is one of \"sl\", \"tt\" or \"tl\".")
    )
   x
}

setMethod("fill.slMatrix",
          signature(m = "matrix", f = "function"),
          function(m, f, period, maxlag, type){
              ## print("filling with a function!")
              ##
              ## 2016-03-31 was: for(i in 1:period)
              for(i in seq(length = period))
                  for(j in 0:maxlag)
                      switch(type,
                             "sl" = {m[i,j+1] <- f(i,j) },
                             "tt" = {m[i,j+1] <- f(i,i-j) },
                             "tl" = {m[i,j+1] <- f(i,j) },
                             stop("Invalid pctime index type, expected \"sl\", \"tt\" or \"tl\".")
                             )
              m
          })

setMethod("fill.slMatrix",
          signature(m="matrix",f="matrix"),
          function(m,f,period,maxlag,type){
            for(i in 1:nrow(f))
              for(j in 1:ncol(f))
                switch(type,
                       "sl" = {if(i<=period && j <= maxlag+1)
                                 m[i,j] <- f[i,j]
                              },

                       "tt" = {wrk <- toSeasonPair(i,j,period)
                               seas <- wrk$season
                               la <- wrk$lag
                               if(la<=maxlag){
                                 if( is.na(m[seas,la+1])){  #+1 because lags start at zero
                                   m[seas,la+1] <- f[i,j]
                                 }else{
                                   if(m[seas,la+1] != f[i,j])
                         warning("encountered different values of acf for a season-lag pair.")
                                 }
                               }
                              },

                       "tl" = {if(j<maxlag)
                                 if( is.na(m[1+ i %% period,j+1])){ # +1 as lags start at zero
                                   m[1+ i %% period,j+1] <- f[i,j]
                                 }else{
                                   if(m[1+ i %% period,j+1] != f[i,j])
                         warning("encountered different values of acf for a season-lag pair.")
                                 }
                             },

                       stop("Invalid index type. type is one of \"sl\", \"tt\" or \"tl\".")
                       )
            m
          }
          )
########################################################## end: support functions for slMatrix

######################################################################## begin: class slMatrix

setClass("slMatrix"
         , representation(m = "matrix")
         , prototype=list(m=matrix(NA,nrow=4,ncol=9) )
         )

### setIs("slMatrix","matrix",coerce=function(x) x@m,
###       replace = function(obj,value){
###         obj@m <- value
###         obj
###       }
###       )
###
### setIs("slMatrix","numeric",coerce=function(x) x@m,
###       replace = function(obj,value){
###         obj@m <- value
###         obj
###       }
###       )
                                                                  # 2016-01-01 some streamlining

                   # 2016-03-31 was: seasonnames = 1:period, ...
                   # changing defensively since S4 methods may call it with invalid period ...
slMatrix <- function(init = NA, period, maxlag, seasonnames = seq(length = period),
                     lagnames = as.character(0:maxlag), periodunit = "season",
                     lagunit = "lag", f = NA, type = "sl" ){

    if(missing(period)){
        period <- if(is.matrix(init))
                      nrow(init)
                  else
                      stop("argument 'init' should be a matrix when 'period' is missing")
    }

    if(missing(maxlag)){
        maxlag <- if(is.matrix(init))
                      ncol(init) - 1
                  else if(is.numeric(init) && length(init)>period){     # 13/03/2006
                      if( (length(init) %% period) == 0 )
                          (length(init) %/% period) - 1
                      else
                          (length(init) %/% period)

                      length(init) %/% period - ((length(init) %% period) == 0)
                  }
                  else
                      3 # this is arbitrary
    }
                             # if length(init) %% period != 0,  matrix(...) will issue a warning
    pcr <- matrix(init, nrow = period, ncol = maxlag + 1)   # lazy! to do: do this properly!
            # the above line may issue warning, the following deals with its cause (partially)
    if(length(pcr) > length(init) && length(init) > 1){                     # krapka, 21/06/2006
        pcr[] <- NA
        pcr[1:length(init)] <- init[]
    }

    if(is.function(f) || !is.na(f) ) # 2014-01-04 is.function() to avoid warning.
                                     # todo: the default would better be NULL (need to check
                                     # if this would break something before changing it.)
        pcr <- fill.slMatrix(pcr, f, period, maxlag, type)

                # 2016-01-01 TODO: maybe assign these only when supplied expicitly?  In that
                #       case will need also a method for show() to display the defaults when the
                #       names are NULL.
    dimnames(pcr) <- list(seasonnames, lagnames)
    names(dimnames(pcr)) = c(periodunit, lagunit)

    new("slMatrix", m = pcr)
}

## 2011-07-07 - vrastam setMethod, ponezhe dolniyat trik veche ne raboti.
setMethod("[", signature("slMatrix"),
          ## 2011-07-07 slagam tyaloto na  pc.f.index tuk
          ##             macham default values za i and j i slagam if(missing(i)) i t.n.
          ##             zastoto inache x[1,] i x[,1] davat greshka.
          ##
          ## pc.f.index
    function(x, i, j, type = "sl", ..., drop = TRUE){     # "s" is for "season, "l" is for
                                                          # "lag", "t" is for "time"
        if(missing(i))
            i <- 1:nSeasons(x)

        if(missing(j))
            j <- 0:maxLag(x)

        y <- x@m
        period <- nSeasons(x)
        switch(type,
               "sl" = {season <- i
                       lag <- pc.omitneg(j, ncol(x)-1)
                       res <- y[season, lag+1]              # lag+1 because lags start from zero
                   },
               "tt" = {
                   res <- myouter(i, j, function(ii, jj){wrk <- toSeasonPair(ii, jj, period)
                                                         season <- wrk$season
                                                         lag <- wrk$lag
                                                         y[season, lag + 1]
                                                     }
                                  )
               },
               "tl" = {season <- toSeason(i, period)
                       lag <- j
                       res <- y[season, lag + 1]      # lag+1 because lags start from zero.
                   },
               "tl+-" = {
                   if(length(j) == 1){
                       if(j>=0){                   # this works only for scalar  j
                           season <- toSeason(i, period)
                           lag <- j
                       }else{
                           season <- toSeason(i - j, period)
                           lag <- -j
                       }
                       res <- y[season, lag+1]      # lag+1 because lags start from zero.
                   }else{
                       res <- matrix(NA, nrow = length(i), ncol = length(j))
                       for(k in 1:length(j)){
                           if(j[k] >= 0){                   # this works only for scalar  j
                               season <- toSeason(i, period)
                               lag <- j[k]
                           }else{
                               season <- toSeason(i - j[k], period)
                               lag <- -j[k]
                           }
                           res[ , k] <- y[season, lag+1]   # lag+1 because lags start from zero.
                       }
                   }
               },
               "t+l,l+-" = {res <- matrix(NA, nrow = length(i), ncol = length(j))
                            for(k in 1:length(j)){
                                res[ , k] <- x[i + j[k], j[k], type="tl+-"]
                            }
                        },
               ## 2016-01-01 TODO: case "co" seems to be meant for i,j - scalar; Check!
               "co" = {season <- toSeason(i, period)
                       lag <- j
                       if(lag < 0 || lag > maxLag(x) )
                           res <- 0
                       else{
                           res <- y[season, lag + 1]      # lag+1 because lags start from zero.
                       }
                   },
               stop("Invalid pctime type of index. type is one of \"sl\", \"tt\" or \"tl\".")
               )
        res
    })

# "[.slMatrix" <-      # Note: the commented out setMethod (see above) does not work when
#   pc.f.index         #      i or j are missing
#               # to do: rework pc.f.index and pc.f.index.replace to have the standard arguments
#               #        and make them methods with setMethod.

setReplaceMethod("[", signature(x = "slMatrix"),
                 pc.f.index.replace
                 )

setMethod("nSeasons", signature("slMatrix"),
          function(object) nrow(object@m) )

setMethod("maxLag", signature("slMatrix"),
          function(object) ncol(object@m)-1 )

#### setMethod("nrow",signature("slMatrix"),
####           function(x) nrow(x@m) )
####
#### setMethod("ncol",signature("slMatrix"),
####           function(x) ncol(x@m) )

########################################################################## end: class slMatrix


## below are updates after 2013-12-31

                # need to define the S3 method since as.matrix doesn't see S4 methods for as()
as.matrix.slMatrix <- function(x, ...){
    x@m
}

# no need of the S4 method since as() calls as.matrix
# setAs("slMatrix", "matrix", as.matrix.slMatrix)



## 2019-05-11 new
## for consistency with "Lagged2d"
setMethod("[[", signature(x = "slMatrix", i = "numeric"),
          function(x, i){
              x@m[ , i + 1] # i is lag
          })

## 2019-05-14 new
setAs("matrix", "slMatrix", function(from) new("slMatrix", m = from))
