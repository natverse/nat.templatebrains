context("Transformation")

points <- matrix(c(30, 30, 30, 110, 110, 110), ncol=3, byrow=T)
colnames(points) <- c("X", "Y", "Z")

if(!require('nat.flybrains')){
  message("skipping bridging registration tests using brains since nat.flybrains is not installed")
} else if(is.null(cmtk.bindir())){
  message("skipping transformation tests since CMTK is not installed")
} else {

test_that("can mirror FCWB", {
  points.mirror <- mirror_brain(points, FCWB)

  points.mirrorexp <- matrix(c(533.852385, 453.736535, 29.7033156, 110.009148, 29.9916627, 107), ncol=3)
  colnames(points.mirrorexp) <- c("X", "Y", "Z")

  expect_equal(points.mirror, points.mirrorexp)
})

test_that("can mirror JFRC2", {
  points.mirror <- mirror_brain(points, JFRC2)

  points.mirrorexp <- matrix(c(601.542797, 522.999806, 12.090383, 97.4817247, 17.7922986, 103.485149), ncol=3)
  colnames(points.mirrorexp) <- c("X", "Y", "Z")

  expect_equal(points.mirror, points.mirrorexp)
})

test_that("can mirror IS2", {
  points.mirror <- mirror_brain(points, IS2)

  points.mirrorexp <- matrix(c(284.682251, 204.314929, 30.2698099, 110.658494, 30.1569345, 110.068235), ncol=3)
  colnames(points.mirrorexp) <- c("X", "Y", "Z")

  expect_equal(points.mirror, points.mirrorexp)
})

test_that("can bridge JFRC2 to FCWB", {
  points.bridge <- xform_brain(points, sample=JFRC2, reference=FCWB)

  points.bridgeexp <- matrix(c(23.1763202, 92.8401558, 35.5010956, 115.450817, 29.7029072, 85.9619354), ncol=3)
  colnames(points.bridgeexp) <- c("X", "Y", "Z")

  expect_equal(points.bridge, points.bridgeexp)
})

test_that("can bridge JFRC2 to IS2 using inverse registration", {
  points.bridge <- xform_brain(points, sample=JFRC2, reference=IS2)

  points.bridgeexp <- matrix(c(-76.9174206, -14.0714713, 44.4391667, 121.076295, 37.566708, 150.794994), ncol=3)
  colnames(points.bridgeexp) <- c("X", "Y", "Z")

  expect_equal(points.bridge, points.bridgeexp)
})

test_that("can use a bridging registration in extra directory",{
  td=tempfile(pattern = 'extrabridge')
  dir.create(td)
  op=options('nat.flybrains.extrabridge'=td)
  on.exit(options(op))

  library(rgl)
  library(nat)
  rcreg=file.path(td,"rhubarb_crumble.list")
  expect_error(xform_brain(c(1,1,1), ref='rhubarb',sample='crumble') )
  i=identityMatrix()
  m=matrix(c(1,1,1),ncol=3)
  cmtk.mat2dof(identityMatrix(), rcreg)
  expect_equal(xform_brain(m, ref='rhubarb',sample='crumble'), m)

  expect_message(xform_brain(m, ref='crumble',sample='rhubarb'),
                 regexp = 'Numerically inverting registration')
  unlink(td, recursive = TRUE)
})

}
