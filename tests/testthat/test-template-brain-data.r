context("Template brain data")

if(!require('nat.flybrains'))
  message("skipping template brain tests since nat.flybrains is not installed")
{
  test_that("origin returns correct result", {
    origin <- origin(FCWB)
    origin.expected <- c(0, 0, 0)
    expect_equal(origin, origin.expected)
  })

  test_that("dim returns correct result", {
    dims <- dim(FCWB)
    dims.expected <- c(1769, 1026, 108)
    expect_equal(dims, dims.expected)
  })

  test_that("voxdims returns correct result", {
    vd <- voxdims(FCWB)
    vd.expected <- c(0.318967307692308, 0.318427024390244, 1)
    expect_equal(vd, vd.expected)
  })

  test_that("boundingbox returns correct result", {
    bb <- boundingbox(FCWB)
    bb.expected <- matrix(c(0, 563.9342, 0, 326.3877, 0, 107), nrow=2)
    expect_equal(bb, bb.expected)
  })
}
