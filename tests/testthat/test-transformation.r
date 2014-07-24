context("Transformation")

if(is.null(cmtk.bindir())){
  message("skipping transformation tests since CMTK is not installed")
} else {

test_that("can use a bridging registration in extra directory",{
  td=tempfile(pattern = 'extrabridge')
  dir.create(td)
  op=options('nat.templatebrains.regdirs'=td)
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
