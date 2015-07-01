context("Registration repositories")

test_that("make_reg_url behaves", {
  expect_equal(make_reg_url("jefferislab/BridgingRegistrations"),
               "https://github.com/jefferislab/BridgingRegistrations")

  abc=c("a/b", "a/c")
  expect_equal(make_reg_url(abc), paste0("https://github.com/", abc))
})

test_that("add works",{
  dir.create(regroot<-tempfile())
  on.exit(unlink(regroot, recursive = TRUE))
  op=options(nat.templatebrains.regdirs='testing')
  on.exit(options(op), add = TRUE)
  add_reg_folder(regroot)
  rop=getOption('nat.templatebrains.regdirs')
  expect_equal(length(rop), 2L)
  expect_equal(rop[2], "testing")
  add_reg_folder(regroot)
  expect_equal(getOption('nat.templatebrains.regdirs'), rop)
  options(nat.templatebrains.regdirs='testing')
  add_reg_folder(regroot, first = FALSE)
  expect_equal(getOption('nat.templatebrains.regdirs'), rev(rop))
})
