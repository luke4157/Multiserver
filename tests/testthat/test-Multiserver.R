test_that("test that Multiserver works", {
  expect_error(Multiserver())
  expect_error(Multiserver(1,))
})


