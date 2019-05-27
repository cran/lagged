## These tests were originally (part of) test-acfbase2sl.R in package pcts.

test_that("the conversions between `season-lag' and acf from `base' are ok",
{
    ##
    ## ?acf2ARMA
    ## ?ARMAacf
    ## ARMAacf(c(1.0, -0.25), 1.0, lag.max = 10)
    ## devtools::test()
    ## tacvfARMA(phi = 1.2, sigmasq = 1, maxLag = 5)
    ## ltsa::tacvfARMA(phi = 1.2, sigmasq = 1, maxLag = 5)
    ## ltsa::tacvfARMA(phi = 0.8, sigmasq = 1, maxLag = 5)
    ## (acf(lh))

    ## pc_acf for  2 seasons in `sl' arrangement
    ##    using character values for easier reading
    matsl <- rbind(paste0("Ra",0:9), paste0("Rb",0:9))
    matsl
    sl2vecacf(matsl) # convert to what I consider "standard" vec format R(k)=EX_tX_{t-k}'
    sl2acfbase(matsl) # convert to the format from acf() (R(k) is the transposed from mine).

    sl2vecacf(matsl) # contains NA's but doesn't lose information
    sl2vecacf(matsl, fullblocks=TRUE) # complete blocks but some values are dropped

    expect_identical(acfbase2sl(sl2acfbase(matsl)), matsl)
    expect_false(identical(acfbase2sl(sl2acfbase(matsl, fullblocks = TRUE)), matsl[ , 1:8]))

    ## compute an acf (the example is from ?acf) and replace the values with character for easy
    ## reading
    mdf <- acf(ts.union(mdeaths, fdeaths), lag.max = 9)
        pacf(ts.union(mdeaths, fdeaths), lag.max = 9)
    mdf$acf
    mdf$lag
    ## str(mdf)
    mdf$acf[ ,1,2] <- paste0("ab", 0:9)
    mdf$acf[ ,2,1] <- paste0("ba", 0:9)
    mdf$acf[ ,2,2] <- paste0("bb", 0:9)
    mdf$acf[ ,1,1] <- paste0("aa", 0:9)

    mdf$acf[1,2,1] <- "ab0" # need symmetric R(0)
    mdf$acf
    expect_identical(sl2acfbase(acfbase2sl(mdf$acf)), mdf$acf)

    lapply(1:dim(mdf$acf)[1], function(k) mdf$acf[k,,])
})
