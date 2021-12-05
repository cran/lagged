test_that("slMatrix",
{
    expect_error(pc.omitneg(-2:2), 'Do not mix positive and negative lags when type="sl"')
    expect_error(pc.omitneg(-2:0), 'argument "maxlag" is missing, with no default')
    expect_equal(pc.omitneg(-2:0, maxlag = 6), 3:6)
    expect_equal(pc.omitneg(-(2:1), maxlag = 6), c(0, 3:6))

    expect_equal(pc.omitneg(1:2, maxlag = 6), 1:2)


    m <- matrix(1:12, nrow = 4)
    expect_error(slMatrix(1:12), "argument 'init' should be a matrix when 'period' is missing")
        # TODO: make attributes equal too?:
    expect_equal(slMatrix(1:12, period = 4), slMatrix(m), check.attributes = FALSE)
    expect_warning(slMatrix(1:12, period = 5),
          "data length \\[12\\] is not a sub-multiple or multiple of the number of rows \\[5\\]")
    expect_warning(slMatrix(1:12, period = 5, f = function(x, y) x^2 + y^2))

    msl <- slMatrix(m)
    class(msl[])
    msl[1, ]
    msl[ , 0]
    msl[1, 0]
    msl[1:2, 0:1]
    msl[1:3, 1:3, type = "tt"]
    msl[1:3, 0:2, type = "tl"]

    msl[1:3, 1, type = "co"]

    expect_error(msl[1:3, 1, type = "dummy"], "Invalid pctime type of index")


    msl[[0]] # arg. is lag
    msl[[1]]

    as(m, "slMatrix")
    as.matrix(msl)
    nSeasons(msl)
    maxLag(msl)

    f <- function(t,s) 0.9^(abs(t-s))
    slMatrix(period = 2,maxlag = 5,f = f, type = "tt")
    slMatrix(period = 2,maxlag = 5,f = f, type = "sl")
    slMatrix(period = 2,maxlag = 5,f = f, type = "tl")
    expect_error(slMatrix(period = 2,maxlag = 5,f = f, type = "dummy"),
                 "Invalid pctime index type, expected")

    msl[1,2] <- 3
    msl[1, ] <- 3
    msl[ , 1] <- 9
    msl[] <- 9

    msl[1,2, type = "tt"] <- NA
    ## expect_warning(msl[1,1:2, type = "tt"] <- c(111, 222))


    msl[1,2, type = "tl"] <- NA
    msl[1,2, type = "tl+-"] <- 9999
    msl[1,2, type = "co"] <- 5555

    expect_equal(msl[1, -2, type = "co"], 0)

    expect_output(msl[1, -2, type = "co"] <- 5555, "attempt to assign a coefficient value at negative/large index")
    
    ## The following command was giving error in package "covr". Resolved by setting
    ## `options(useFancyQuotes = FALSE)` in `tests/testthat.R`, see
    ## https://github.com/r-lib/covr/issues/420. Another solution is:
    ##
    ## expect_error(nSeasons(msl) <- 5,
    ##              "unable to find an inherited method for function .*nSeasons<-.*")
    ##
    ## but note that each quote is replaced by .* not just . 
    expect_error(nSeasons(msl) <- 5, "unable to find an inherited method for function .nSeasons<-.")

})

