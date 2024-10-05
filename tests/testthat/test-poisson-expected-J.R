
test_that("stable mean", {
  expect_error(getmeanJ(0.0, .01, 1.0))
  expect_error(getmeanJ(2.0, .01, 1.0))
  expect_no_error(getmeanJ(1.0, .01, 1.0))
  expect_no_error(getmeanJ(1.5, .01, 1.0))
  
  
})