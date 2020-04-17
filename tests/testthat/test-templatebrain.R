test_that("templatebrain works", {
  expect_is(IS2 <- templatebrain("IS2", dims=c(30,40,50), note="Test field"),
            'templatebrain')
  expect_null(IS2$origin)

  expect_equal(as.templatebrain(IS2), IS2)

  expect_error(as.templatebrain("rhubarb.nrrd"))
  expect_equal(origin(IS2), c(0,0,0))
})
