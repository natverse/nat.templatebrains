context("Template brain data")

FCWB <- structure(list(name = "FlyCircuit Whole Brain", type = "Average",
  sex = "Intersex", voxdims = c(0.318967307692308, 0.318427024390244,
    1), origin = c(0, 0, 0), BoundingBox = structure(c(0,
    563.9342, 0, 326.3877, 0, 107), .Dim = 2:3), units = c("microns",
    "microns", "microns"), description = paste0("An intersex averaged brain formed",
    " from 17 female and 9 male brains from the FlyCircuit dataset."),
  regName = "FCWB", dims = c(1769L, 1026L, 108L)), .Names = c("name",
  "type", "sex", "voxdims", "origin", "BoundingBox", "units",
  "description", "regName", "dims"), class = "templatebrain")

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

context("Template brain")

test_that("is.templatebrain works",{
  expect_true(is.templatebrain(FCWB))
  expect_false(is.templatebrain("FCWB"))
})
