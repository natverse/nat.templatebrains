context("Transformation")

test_that("we can find registrations",{

  op=options(nat.templatebrains.regdirs=NULL)
  on.exit(options(op))
  expect_error(find_reg("rhubarb.list"), "registration directories")

  td=tempfile(pattern = 'regdir1')
  dir.create(td)
  options('nat.templatebrains.regdirs'=td)

  expect_equal(find_reg("rhubarb.list"), "")
  expect_error(find_reg("rhubarb.list", mustWork = TRUE), "Unable to find")
  dir.create(file.path(td,"rhubarb.list"), recursive = T)
  expect_equal(find_reg("rhubarb.list"), file.path(td,"rhubarb.list"))

  td2=tempfile(pattern = 'regdir2')
  dir.create(file.path(td2, 'rhubarb.list'), recursive = T)
  dir.create(file.path(td2, 'crumble.list'), recursive = T)

  options(nat.templatebrains.regdirs=c(td,td2))
  expect_equal(find_reg("rhubarb.list"), file.path(td,"rhubarb.list"))
  expect_equal(find_reg("crumble.list"), file.path(td2,"crumble.list"))
  unlink(td, recursive = TRUE)
  unlink(td2, recursive = TRUE)
})

test_that("we can find bridging registrations",{
  td=tempfile(pattern = 'extrabridge')
  dir.create(td)
  op=options('nat.templatebrains.regdirs'=td)
  on.exit(options(op))

  rcreg=file.path(td,"rhubarb_crumble.list")
  dir.create(rcreg, recursive = T)
  expect_true(nzchar(bridging_reg(ref='rhubarb', sample='crumble')))
  expect_error(bridging_reg(ref='crumble', sample='rhubarb', mustWork = T))
  expect_false(nzchar(bridging_reg(ref='crumble', sample='rhubarb')))

  br<-bridging_reg(reference ='crumble', sample='rhubarb', checkboth = TRUE)
  expect_true(nzchar(br))
  expect_true(attr(br,'swapped'))

  # now try equivalence of bridging_sequence and bridging_reg
  expect_equivalent(bridging_sequence(ref="rhubarb", sample = "crumble"),
               as.list(bridging_reg(ref='rhubarb', sample='crumble')))
  expect_equivalent(bridging_sequence(ref="crumble", sample = "rhubarb"),
                    as.list(bridging_reg(ref='crumble', sample='rhubarb')))
  expect_equivalent(bridging_sequence(ref="crumble", sample = "rhubarb", checkboth=T),
                    as.list(bridging_reg(ref='crumble', sample='rhubarb', checkboth=T)))
  expect_error(bridging_sequence(ref='crumble', sample='rhubarb', mustWork=T))
})

if(is.null(cmtk.bindir())){
  message("skipping transformation tests since CMTK is not installed")
} else {
context("Transformation.cmtk")
test_that("can use a bridging registration in regdirs",{
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

  unlink(td, recursive = TRUE)
})

}
