
test_that("Lagged classes: indexing",
{
    ## Lagged1d
    v <- 1:12
    v_lagged <- Lagged(v)
    expect_identical(v_lagged, new("Lagged1d", data = v))
    expect_equal(v_lagged[0:2], v[1:3])
    expect_equal(v_lagged[[0]], 1)
    expect_equal(v_lagged[0],   v_lagged[0, drop = FALSE])
    expect_equal(v_lagged[[0]], v_lagged[0, drop = TRUE])

    expect_equal(Lagged(1:4) + Lagged(1:2), Lagged(c(2, 4, NA, NA)))

    expect_equal(Lagged(1:4) + 1:4, Lagged(1:4 + 1:4))
    expect_equal(Lagged(1:4) + 1, Lagged(1:4 + 1))
    expect_error(Lagged(1:4) + 1:2, "Incompatible length of operands in a binary operation")


    ## Lagged2d
    m <- matrix(1:12, nrow = 4)
    m_lagged <- Lagged(m)
    expect_identical(m_lagged, new("Lagged2d", data = m))
    expect_equal(dim(m_lagged[0]), c(4, 1))
    expect_equal(m_lagged[0], m[ , 1, drop = FALSE])

    expect_null(dim(m_lagged[[0]]))
    expect_equal(m_lagged[[0]], m[ , 1, drop = TRUE])
    expect_equal(m_lagged[0],   m_lagged[0, drop = FALSE])
    expect_equal(m_lagged[[0]], m_lagged[0, drop = TRUE])

    mm <- matrix(10:49, nrow = 4, byrow = TRUE)
    mm_lagged <- Lagged(mm)

    as.matrix(mm_lagged)

    ## one index: lag
    expect_equal(ncol(mm_lagged[0]), 1)   # column vector
    expect_null(dim(mm_lagged[[0]]))      # numeric
    ## two indices: first is row, second is lag
    expect_true(!is.null(mm_lagged[1, 0]))     # '[' doesn't drop dimensions
    expect_true(!is.null(mm_lagged[1, 0:3]))

    expect_null(dim(mm_lagged[[1, 0]]  )) # '[[' does drop dimensions
    expect_null(dim(mm_lagged[[1, 0:3]]))
    expect_null(dim(m_lagged[[1, TRUE]])) # the whole first row, as numeric

    expect_true(!is.null(mm_lagged[1:2, 0:3])) # ok, a matrix
    ## but the first arg. of '[[' must be of length one, so this throws error:
    expect_error(mm_lagged[[1:2 , 0:3]])

    ## currently no "[" method for "logical"
    ## TODO: maybe add one for symmetry with '[['?
    expect_error(m_lagged[1, TRUE])

    ## TODO: put expectations here:
    mm_lagged[1:4,1:4, drop = "sl"] # "sl" is the default, covered by  drop = "missing"
    mm_lagged[1:4,1:4, drop = "tt"]
    mm_lagged[1:4,1:4, drop = "tl"]
    expect_error(mm_lagged[1:4,1:4, drop = "dummy"], "Invalid arg. type, must be one of")
    mm_lagged[5:12, 1, drop = "tl+-"]
    mm_lagged[6:13, 1:4, drop = "tl+-"]
    mm_lagged[1, 4, drop = "co"]   # TODO: "co" may be worth a class
    mm_lagged[2, -1, drop = "co"]
    mm_lagged[2, 0, drop = "co"]
    mm_lagged[2, 9, drop = "co"]
    mm_lagged[2, 10, drop = "co"]
    mm_lagged[ 1:2, 4, drop = "co"] # 14 24
    mm_lagged[ 1:6, 4, drop = "co"] # 14 24 34 44 14 24
    mm_lagged[ 1:6, 0, drop = "co"] # 14 24 34 44 14 24

    mmm_lagged <- Lagged(mm)
    mmm_lagged[[1]]
    mmm_lagged[[1, TRUE]]
    mmm_lagged[[1, c(TRUE, FALSE)]]
    expect_error(mmm_lagged[[1:2, c(TRUE, FALSE)]], "the length of argument `i' must be equal to one")



    ## Lagged3d
    a <- array(1:24, dim = c(2, 3, 4))
    a_lagged <- Lagged(a)
    expect_identical(a_lagged, new("Lagged3d", data = a))
    expect_equal(dim(a_lagged[0]), c(2,3,1) ) # c(dim(a)[-3], 1) )
    expect_equal(a_lagged[0], a[ , , 1, drop = FALSE])

    expect_equal(dim(a_lagged[[0]]), c(2,3) ) # dim(a)[-3]
    expect_equal(a_lagged[[0]], a[ , , 1, drop = TRUE])
    expect_equal(a_lagged[0],   a_lagged[0, drop = FALSE])
    expect_equal(a_lagged[[0]], a_lagged[0, drop = TRUE])

    as.array(a_lagged)

    ## as above for "FlexibleLagged"
    v_flex <- new("FlexibleLagged", data = v)
    expect_identical(v_flex@data, v_lagged)
    expect_equal(v_flex[0], v_lagged[0])
    expect_equal(v_flex[[0]], v_lagged[[0]])
    expect_equal(v_flex[0],   v_flex[0, drop = FALSE])
    expect_equal(v_flex[[0]], v_flex[0, drop = TRUE])

    expect_equal(v_flex[], v)
    v_flex2 <- v_flex
    v_flex2[3:4] <- 0
    expect_equal(v_flex2[], c(v[1:3], 0, 0, v[6:12]))

    v_flex2[] <- v_flex
    v_flex2[] <- v_flex@data
    expect_error(v_flex2[] <- array(1, dim = c(2,2,2,2)), "Don't know what Lagged class to use for this value")

    v_flex3 <- v_flex
    v_flex3[] <- 1:5
    expect_equal(v_flex3[], 1:5) # length changes

    v_flex3[[1]]
    expect_error(v_flex3[[c(1,2)]], "the length of argument `i' must be equal to one")

    v_flex3      + v_flex3
    v_flex3@data + v_flex3
    v_flex3      + v_flex3@data
    v_flex3 + 1
    1 + v_flex3
    round(v_flex3)
    max(v_flex3)

    as.numeric(v_flex3)
    as.numeric(v_flex3@data)

    as.array(v_flex3)
    as.array(v_flex3@data)

    as.vector(v_flex3@data)
    as.vector(v_flex3)

    ## the data part is 1d here, so this gives error:
    expect_error(v_flex3[[1, 2]], "incorrect number of subscripts")
    m4_3 <- matrix(1:12, nrow = 4)
    m4_3fl <- Lagged(m4_3)
    expect_equal(Lagged(m4_3)[[1, 2]], m4_3[1, 3]) # second index is lag
    expect_equal(-(-Lagged(m4_3)), Lagged(m4_3)) # unary -
    expect_equal(Lagged(m4_3) + Lagged(m4_3), 2 * Lagged(m4_3))

    as.vector(m4_3fl)
    as.matrix(m4_3fl)
    expect_equal(maxLag(m4_3fl),2)
    maxLag(m4_3fl) <- 1

    m_flex <- new("FlexibleLagged", data = m)
    expect_identical(m_flex@data, m_lagged)
    expect_equal(m_flex[0], m_lagged[0])
    expect_equal(m_flex[[0]], m_lagged[[0]])
    expect_equal(m_flex[0],   m_flex[0, drop = FALSE])
    expect_equal(m_flex[[0]], m_flex[0, drop = TRUE])

    a_flex <- new("FlexibleLagged", data = a)
    expect_identical(a_flex@data, a_lagged)
    expect_equal(a_flex[0], a_lagged[0])
    expect_equal(a_flex[[0]], a_lagged[[0]])
    expect_equal(a_flex[0],   a_flex[0, drop = FALSE])
    expect_equal(a_flex[[0]], a_flex[0, drop = TRUE])

    expect_equal(a_flex[0, ],   a_flex[0, , drop = FALSE])



    ## maxLag, maxLag<-
    ##    TODO: extending with "maxLag<-"()

    expect_equal(maxLag(v_lagged), 11)
    expect_equal(maxLag(m_lagged), 2)
    expect_equal(maxLag(a_lagged), 3)

    v2_lagged <- v_lagged
    maxLag(v2_lagged) <- 2
    expect_equal(maxLag(v2_lagged), 2)
    expect_equal(v2_lagged[], v[1:3])

    v2_lagged[1]
    v2_lagged[1, drop = TRUE]
    v2_lagged[1, drop = FALSE]
    v2_lagged[1,]
    v2_lagged[1, , drop = TRUE]
    v2_lagged[1, , drop = FALSE]

    v2_lagged[ , 1]

    m2_lagged <- m_lagged
    maxLag(m2_lagged) <- 2
    expect_equal(maxLag(m2_lagged), 2)
    expect_equal(m2_lagged[], m[, 1:3])

    a2_lagged <- a_lagged
    maxLag(a2_lagged) <- 2
    expect_equal(maxLag(a2_lagged), 2)
    expect_equal(a2_lagged[], a[, , 1:3])

    maxLag(acf(AirPassengers))
    expect_error(maxLag(sin), "No applicable method to compute maxLag")

})



test_that(".whichNativeLagged is ok",
{
    expect_identical(.whichNativeLagged(1:3), "Lagged1d")
    expect_identical(.whichNativeLagged(1:3 / 4), "Lagged1d")

    expect_identical(.whichNativeLagged(matrix(1:12, 4)), "Lagged2d")
    expect_identical(.whichNativeLagged(array(1:24, dim = c(4,3,2))), "Lagged3d")

    ## TODO: was this really the intent of this function (for the case of Lagged objects?
    expect_identical(.whichNativeLagged(new("Lagged1d")), "FlexibleLagged")
    expect_identical(.whichNativeLagged(new("Lagged2d")), "FlexibleLagged")
    expect_identical(.whichNativeLagged(new("Lagged3d")), "FlexibleLagged")

    expect_identical(.whichNativeLagged(new("Lagged3d")), "FlexibleLagged")

    ## otherwise
    expect_true(is.na(.whichNativeLagged(sin))) # a function
})

test_that("Lagged classes: initialisation",
{
    expect_equal_to_reference(new("FlexibleLagged"), "fl0.RDS")

    expect_equal(class( Lagged(data = new("Lagged1d")) ), "FlexibleLagged", FALSE)
    expect_equal(class( Lagged(data = new("Lagged2d")) ), "FlexibleLagged", FALSE)
    expect_equal(class( Lagged(data = new("Lagged3d")) ), "FlexibleLagged", FALSE)

    fl  <- new("FlexibleLagged", data = 1:3)
    fla <- new("FlexibleLagged", data = fl)
    expect_equal(class(fl@data), "Lagged1d", FALSE)
    expect_equal(class(fla@data), "Lagged1d", FALSE)
    expect_equal(fla@data, fl@data)
    expect_identical(fla@data, fl@data)

    acf_ap <- acf(AirPassengers, plot = FALSE)
    expect_identical(Lagged(acf_ap), acf2Lagged(acf_ap))

    ## cars is a data.frame
    expect_error(Lagged(cars), "I don't know how to create a Lagged object from the given data")

    z <- ts(matrix(rnorm(300), 100, 3), start = c(1961, 1), frequency = 12)
    acf2Lagged(acf(z))
    acf2Lagged(acf(z, type = "partial"))

})


test_that("dataWithLagNames() is ok",
{
    ## length-zero objects are returned as is
    expect_identical(dataWithLagNames(numeric(0)), numeric(0))
    m0 <- matrix(0, nrow = 0, ncol = 3)
    expect_identical(dataWithLagNames(m0), m0)

    expect_equal(dataWithLagNames(1:3), c(Lag_0 = 1, Lag_1 = 2,  Lag_2 =3))
    expect_equal(colnames(dataWithLagNames(matrix(1:12, nrow = 3))), paste0("Lag_", 0:3))

    a432 <- dataWithLagNames(array(1:24, dim = c(4,3,2)))
    expect_identical(dimnames(a432), list(rep("", 4), rep("", 3), c("Lag_0", "Lag_1")))
})

test_that("Lagged: show",
{

    fl  <- new("FlexibleLagged", data = 1:3)
    expect_output(show(fl))


    expect_output(show(new("Lagged1d")))
    expect_output(show(new("Lagged2d")))
    expect_output(show(new("Lagged3d")))

})
