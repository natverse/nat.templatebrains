context("regtemplate")

test_that("getting/setting regtemplate works", {
  kcs3.1=kcs20[1:3]
  kcs3.2=kcs3.1

  data("FCWB.demo")
  regtemplate(kcs3.1)="FCWB.demo"
  regtemplate(kcs3.2)=FCWB.demo
  expect_equal(kcs3.1, kcs3.2)
  expect_equal(regtemplate(kcs3.1), FCWB.demo)
  attr(kcs3.2, 'regtemplate')="FCWB.demo"
  expect_equal(regtemplate(kcs3.2), FCWB.demo)
})

test_that("mirroring can use regtemplate", {
  kcs3=kcs20[1:3]
  regtemplate(kcs3)="FCWB.demo"
  kcs3m=mirror_brain(kcs3, transform = 'flip')
  kcs3m2=mirror_brain(kcs3m, transform = 'flip')
  expect_equivalent(kcs3m2, kcs3)
  expect_equal(regtemplate(kcs3m2), regtemplate(kcs3))

  expect_equal(mirror_brain(kcs20[1:3], brain=FCWB.demo, transform = 'flip'),
               kcs3m)
})

test_that("guess_templatebrain",{
  testim <- as.im3d(FCWB.demo)
  expect_equal(guess <- guess_templatebrain(testim, mustWork = T), FCWB.demo)
  expect_equal(guess_templatebrain(testim, rval='name'), as.character(FCWB.demo))

  expect_equal(guess <- guess_templatebrain(c(1769,1026,108), mustWork = T),
               FCWB.demo)
  expect_equal(guess <- guess_templatebrain(c(1769,1026), mustWork = T),
               FCWB.demo)
  expect_error(guess <- guess_templatebrain(c(1769,1999), mustWork = T))
  expect_error(guess <- guess_templatebrain(c(1769), mustWork = T))
})
