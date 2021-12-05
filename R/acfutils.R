## convert between (a) `season-lag',
##                 (b) R(k) = EX_tX_{t-k}', k = 0, 1, 2
##                 (c) the rrepresentation resulting from acf() in which R(k) are transposed
##                     (and lag is the first index)
## TODO: better control of maxlag; in particular option to keep all values, even if this
##       leads to NA's in the result (also option for NA or 0's).
acfbase2sl <- function(acf){ # this doesn't need to handle maxlag; simply keep all values
    a <- acf
    stopifnot(dim(a)[2] == dim(a)[3])
    d <- dim(a)[2]
                                        # permute to put the lag index last.
                                        # Note:  also permute R(k) to match acf() output
                                        #     (so, the following, not b <- aperm(a, c(2,3,1)))
    b <- aperm(a, c(3,2,1))
    ind <- dim(b)[3]
    brev <- b[ , , ind:1]
    m <- ind * d
    wrk <- sapply(1:d,
                  function(s){
                      v <- as.vector(brev[s, , ]) # s-th row of the block-matrix
                      c(rev(v[1 : (m - (d - s))]), rep(NA, d-s)) # drop redundant and reverse
                  }                                              # to get lags from 0, 1,...
                  )

    res <- t(wrk) # transpose as often needed with `sapply'

                                        # drop trailing NA columns;
                                        # TODO: guard gainst ending up with zero columns?
                                        # TODO: provide separate argument to control this -
                                        #   NA's may be here for reasons outside this function
    while(all(is.na(res[ , ncol(res)])))
        res <- res[ , -ncol(res), drop = FALSE]

    res
}

sl2vecacf <- function(mat, maxlag, fullblocks = FALSE){## TODO: currently maxlag is ignored
    d  <- nrow(mat)
    nc <- ncol(mat)

    Extended.maxlag <- pc.arith.ceiling(nc - 1, d)
    leftskip <- Extended.maxlag - (nc-1) # number of added lags to slmat (with NA values)

    vecmaxlag <- Extended.maxlag %/% d

    wrk <- matrix(NA, d, Extended.maxlag + d) # add d for the zero lag
    ind <- leftskip : (leftskip + nc - 1) # length is nc
    for(s in 1:d){   # this leaves some NA's in R(0), to be filled later
        wrk[s, s + ind] <- rev(mat[s, ])
    }

    if(fullblocks){  #  && leftskip > 0
        wrk <- wrk[ , -(1:d)] # drop the incomplete block
        vecmaxlag <- vecmaxlag - 1
    }
    wrk <- array(wrk, dim = c(d, d, vecmaxlag + 1))

    res <- wrk[ , , (vecmaxlag + 1):1] # put the lags back in right order (0,1,2,...)

                                     # drop trailing whole NA lags (see the note for the
                                     # analogous action in acfbase2sl() for a drawback);
                                     # TODO: maybe should do this conditionally on an argument
    while(all(is.na(res[ , , dim(res)[3]])))
        res <- res[ , , -dim(res)[3], drop = FALSE]




    R0 <- res[ , , 1]           # now R(0) is conveniently at index 1, fill its upper triangle
    ind <- row(R0) < col(R0)   # lazy; even more lazy (end fool-proof) would be to declare R0
    R0[ind] <- t(R0)[ind]      # symmetric
    res[ , , 1] <- R0

    res
}

sl2acfbase <- function(mat, maxlag, fullblocks = FALSE){
    a <- sl2vecacf(mat, maxlag, fullblocks)
    aperm(a, c(3,2,1))  # now convert to the convention of base acf(): make lag the last
}                       # index and transpose R(k)
