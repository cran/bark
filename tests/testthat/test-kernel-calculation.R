# Copyright (c) 2023 Merlise Clyde and Zhi Ouyang. All rights reserved
# See full license at
# https://github.com/merliseclyde/bark/blob/master/LICENSE.md
#
# SPDX-License-Identifier: GPL-3.0-or-later



## github issue #1 for type checking
test_that("Using .Call getDesignCall Check Types", {
  n = 100; d=2
  p = n
  train = sim_circle(n, d) #
  expect_no_error(createDesignCall(train$x,
                                   center=train$x,
                                   L = rep(.5, d),
                                   intercept=rep(1,p)))
                  
  expect_no_error(createDesignCall(train$x,
                                   center=train$x,
                                   L = rep(1L, d),
                                   intercept=rep(1L,p)))
  
  expect_true(is.matrix(createDesignCall(train$x,
                                         center=train$x,
                                         L = rep(.5, d),
                                         intercept=rep(1L,p))))

})

test_that("Using .Call getDesignCall Check dimensions", {
  n = 100; d=2
  p = n
  train = sim_circle(n, d) #
  expect_error(createDesignCall(train$x,
                                center=train$x[5,1],
                                L = rep(.5, p),
                                intercept=rep(1,d)))
  expect_error(createDesign(train$x,
                            center=train$x[5,1],
                            L = rep(.5, p),
                            intercept=rep(1,d)))
  expect_no_error(createDesignCall(train$x,
                                center=train$x,
                                L = rep(.5, d),
                                intercept=rep(1,p)))
  expect_no_error(createDesign(train$x,
                               center=train$x,
                               L = rep(.5, d),
                               intercept=rep(1,p)))
})

test_that("stable mean", {
  expect_error(getmeanJ(0.0, .01, 1.0))
  expect_error(getmeanJ(2.0, .01, 1.0))
  expect_no_error(getmeanJ(1.0, .01, 1.0))
  expect_no_error(getmeanJ(1.5, .01, 1.0))
  
  
})