test_that("templatebrain works", {
  expect_is(IS2 <- templatebrain("IS2"), 'templatebrain')
  expect_null(IS2$origin)
})
