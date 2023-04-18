# Copyright (c) 2023 Merlise Clyde and Zhi Ouyang. All rights reserved
# See full license at
# https://github.com/merliseclyde/bark/blob/master/LICENSE.md
#
# SPDX-License-Identifier: GPL-3.0-or-later

test_that("test inputs", {
  
  #regression
  traindata <- sim_Friedman2(200, sd=125)
  testdata <- sim_Friedman2(1000, sd=0)
  
  # check main input argument types  
  # no formula (character string)  
  expect_error(bark("y ~ .", data=data.frame(traindata), 
                    testdata= data.frame(testdata),
                    nburn=10, nkeep=100, keepevery=10,
                    classification = FALSE, 
                    common_lambdas = FALSE, 
                    selection = FALSE,
                    printevery=10^10))
  
  # train ata is not a dataframe    
  expect_error(bark(y ~ ., data=traindata, 
                    testdata= data.frame(testdata),
                    nburn=10, nkeep=100, keepevery=10,
                    classification = FALSE, 
                    common_lambdas = FALSE, 
                    selection = FALSE,
                    printevery=10^10))    
  
  # testdata is not a dataframe
  expect_error(bark(y ~ ., data=data.frame(traindata), 
                    testdata=testdata,
                    nburn=10, nkeep=100, keepevery=10,
                    classification = FALSE, 
                    common_lambdas = FALSE, 
                    selection = FALSE,
                    printevery=10^10))    
  
  set.seed(42)
  n = 500
  circle2 = data.frame(sim_circle(n, dim = 2))
  train = sample(1:n, size = floor(n/2), rep=FALSE)
  
  expect_error(bark(y ~ ., data=circle2, subset=train,
                    testdata = circle2[-train, ],
                    classification = 1,
                    nburn = 10,
                    nkeep = 10,
                    printevery = 10^10))
  
  # dim of test & training disagree
  expect_error(bark(y ~ x.1 + x.2., data=circle2, subset=train,
                    testdata = circle2[-train, "x.1"],
                    classification = TRUE,
                    nburn = 10,
                    nkeep = 10,
                    printevery = 10^10))
  
  expect_error(bark(y ~ ., data=circle2, subset=train,
                    testdata = as.matrix(circle2[-train, ]),
                    classification = 1,
                    nburn = 10,
                    nkeep = 10,
                    printevery = 10^10))
  
  expect_no_error(bark(y ~ ., data=circle2, subset=train,
                       #                   testdata = as.matrix(circle2[-train, ]),
                       classification = TRUE,
                       nburn = 10,
                       nkeep = 10,
                       printevery = 10^10))
  expect_error(bark(y ~ ., data=circle2, subset=train,
                    classification = TRUE,
                    nburn = 10,
                    nkeep = 10,
                    fixed = list(alpha = 2.0),
                    printevery = 10^10))
})

# github issue #3
test_that("test bark with p=1", {
  n = 100; p = 1
  df = data.frame(y = rnorm(n), x = rnorm(n))
  expect_no_error(bark(y ~ ., data=df, 
                       classification = FALSE,
                       nburn = 10,
                       nkeep = 1000,
                       keepevery = 10,
                       printevery = 10^10))
  expect_no_error(bark_mat(y.train = df$y, x.train = as.matrix(df$x), 
                           x.test = as.matrix(df$x), 
                           classification = FALSE,
                           nburn = 10,
                           nkeep = 1000,
                           keepevery = 10,
                           printevery = 10^10))
  
  expect_no_error(bark(y ~ ., data=df, 
                       classification = FALSE,
                       nburn = 10,
                       nkeep = 1000,
                       keepevery = 10,
                       selection = TRUE,
                       common_lambdas = FALSE,
                       printevery = 10^10))
  
})

