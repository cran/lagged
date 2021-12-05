test_that("utils.R",
{
    expect_output(.reportClassName(Lagged(1:3), "Lagged1d"), 'An object of class "Lagged1d"')
    expect_silent(.reportClassName(Lagged(1:3), "Lagged3d"))

    f <- function(x, y, ..., drop) nposargs(sys.call())
    expect_equal(f(1), 1)
    expect_equal(f(1, y = 2), 1)
    expect_equal(f(1, 2), 2)
    expect_equal(f(1, 2, z = 4, drop = TRUE), 2)

    g <- function(x, y, ..., drop) nposargs(sys.call(), TRUE)
    expect_equal(f(1, 2), 2)
    expect_equal(g(1, 2), 1)

    expect_equal(pc.arith.floor(9,4), 8)
    expect_equal(pc.arith.floor(12,4), 12)

    expect_equal(pc.arith.ceiling(9,4), 12)
    expect_equal(pc.arith.ceiling(12,4), 12)

    expect_equal(rsum(matrix(1:6, nrow = 2, byrow = TRUE)), c(6, 15))
    expect_equal(1:3, 1:3)

    tt <- outer(1:6, 1:6)
    expect_equal(dim(ttmatToslPairs(1:6, 1:6, period = 4)), c(36, 4))
    expect_warning(ttTosl(tt, period = 4), "encountered different values of acf for a season-lag pair")


})
