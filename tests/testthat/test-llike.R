test_that("Confirm .Call getDesignCall Agrees with Old", {
  n = 100; d=2
  p = n
  train = sim_circle(n, d)
  better = bark:::createDesignCall(train$x, center=train$x,
                                 L = rep(.5, d),
                                 intercept=rep(1L,p))
  good = bark:::createDesign(train$x, train$x,
                             L = rep(.5, d),
                             intercept = rep(1L,p),
                             n=100, p=100, d=2)
  expect_equal(dim(good), dim(better))
  expect_equal(good, better)
})

## github issue #1 for type checking
test_that("Using .Call getDesignCall Check Types", {
  n = 100; d=2
  p = n
  train = sim_circle(n, d) #
  expect_error(bark:::createDesignCall(train$x,
                                       center=train$x,
                                       L = rep(.5, d),
                                       intercept=rep(1,p)))
  expect_true(is.matrix(bark:::createDesignCall(train$x,
                                                center=train$x,
                                                L = rep(.5, d),
                                                intercept=rep(1L,p))))

})
