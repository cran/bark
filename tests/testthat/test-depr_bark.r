# Copyright (c) 2023 Merlise Clyde and Zhi Ouyang. All rights reserved
# See full license at
# https://github.com/merliseclyde/bark/blob/master/LICENSE.md
#
# SPDX-License-Identifier: GPL-3.0-or-later

test_that("old bark", {
  
  #regression
  traindata <- sim_Friedman2(200, sd=125)
  testdata <- sim_Friedman2(1000, sd=0)
 
# check main input argument types  
# y is not a vector
expect_error(bark_mat( y.train=data.frame(traindata), x.train=traindata$x,
                    x.test = testdata$x,
                    nburn=10, nkeep=100, keepevery=10,
                    classification = FALSE, 
                    printevery=10^10))

# x.train data is not a matrix
expect_error(bark_mat(x.train=traindata, y.train = traindata$y,
                  x.test= testdata$x,
                  nburn=10, nkeep=100, keepevery=10,
                  classification = FALSE, 
                  printevery=10^10))    

# testdata is a dataframe
 expect_error(bark_mat(x.train=traindata$x, y.train = traindata$y,
                 x.test = testdata,
                 nburn=10, nkeep=100, keepevery=10,
                 classification = FALSE, 
                 printevery=10^10))    
 
  # wrong type option
  expect_error(bark_mat(traindata$x, traindata$y, testdata$x,
                            nburn=10, nkeep=100, keepevery=10,
                            classification=FALSE, type="F", printevery=10^10))


   expect_error(bark_mat(traindata$x, traindata$y[-1], testdata$x,
                             nburn=10, nkeep=100, keepevery=10,
                             classification=FALSE, type="e", printevery=500))
   
   expect_error(bark_mat(traindata$x, traindata$y, testdata$x[ ,-1],
                         nburn=10, nkeep=100, keepevery=10,
                         classification=FALSE, type="e", printevery=500))
    
   expect_error(bark_mat(traindata$x, traindata$y, testdata$x,
                                      nburn=10, nkeep=100, keepevery=10,
                                      classification=5, type="e", 
                                      printevery=500))
    
   expect_error(bark_mat(traindata$x, traindata$y, as.character(testdata$x),
                            nburn=10, nkeep=10, keepevery=10, 
                            keeptrain=TRUE,
                            classification=FALSE, type="sd", printevery=10^10))
   
                             
   expect_no_error(bark_mat(traindata$x, traindata$y, testdata$x,
                         nburn=10, nkeep=10, keepevery=10, 
                         keeptrain=TRUE,
                         classification=FALSE, type="sd", printevery=10^10))
   
   expect_error(bark_mat(traindata$x, traindata$y, testdata$x,
                         nburn=10, nkeep=100, keepevery=10, 
                         fixed=list(alpha=2),
                         classification=FALSE, type="sd", printevery=10^10))
   
   
   expect_error(bark_mat(as.character(traindata$x), traindata$y, testdata$x,
                            nburn=10, nkeep=10, keepevery=10, 
                            fixed=list(alpha=1), keeptrain=TRUE,
                            classification=FALSE, type="sd", printevery=10))
   expect_error(bark_mat(traindata$x, as.character(traindata$y), testdata$x,
                         nburn=10, nkeep=10, keepevery=10, 
                         fixed=list(alpha=1), keeptrain=TRUE,
                         classification=FALSE, type="sd", printevery=10))
    })

test_that("new bark", {
  skip_on_cran()
  #regression
  traindata <- sim_Friedman2(200, sd=125)
  testdata <- sim_Friedman2(1000, sd=0)
  
  
  set.seed(42)
  fit.bark.depc <- bark_mat(traindata$x, traindata$y, testdata$x,
                            nburn=10, nkeep=100, keepevery=10,
                            classification=FALSE, type="d", printevery=10^10)
  
  
  
  set.seed(42)
  fit.bark  <- bark(y ~ ., data=data.frame(traindata), 
                    testdata= data.frame(testdata),
                    nburn=10, nkeep=100, keepevery=10,
                    classification = FALSE, 
                    common_lambdas = FALSE, 
                    selection = FALSE,
                    printevery=10^10,
                    keeptrain = TRUE)
  
  
  
  expect_equal(mean((fit.bark.depc$yhat.test.mean-testdata$y)^2),
               mean((fit.bark$yhat.test.mean-testdata$y)^2))
  
  set.seed(42)
  fit.bark.depc <- bark_mat(traindata$x, traindata$y, testdata$x,
                            nburn=10, nkeep=100, keepevery=10,
                            classification=FALSE, type="e", printevery=500)
  set.seed(42)
  fit.bark  <- bark(y ~ ., data=data.frame(traindata), 
                    testdata =data.frame(testdata),
                    nburn=10, nkeep=100, keepevery=10,
                    classification=FALSE, selection = FALSE, 
                    printevery = 500, verbose=TRUE)
  
  
  
  expect_equal(mean((fit.bark.depc$yhat.test.mean-testdata$y)^2),
               mean((fit.bark$yhat.test.mean-testdata$y)^2))
  
  
  set.seed(42)
  fit.bark.depc <- bark_mat(traindata$x, traindata$y, testdata$x,
                            nburn=10, nkeep=100, keepevery=10,
                            classification=FALSE, type="sd", printevery=10^10)
  set.seed(42)
  fit.bark  <- bark(y ~ ., data=data.frame(traindata), 
                    testdata =data.frame(testdata),
                    nburn=10, nkeep=100, keepevery=10,
                    classification=FALSE, common_lambdas=FALSE, 
                    printevery=10^10)
  
  
  
  expect_equal(mean((fit.bark.depc$yhat.test.mean-testdata$y)^2),
               mean((fit.bark$yhat.test.mean-testdata$y)^2))
  
  # classification
  set.seed(42)
  n = 500
  circle2 = data.frame(sim_circle(n, dim = 2))
  train = sample(1:n, size = floor(n/2), rep=FALSE)
  set.seed(42)
  circle2.bark = bark(y ~ ., data=circle2, subset=train,
                      testdata = circle2[-train, ],
                      classification = TRUE,
                      nburn = 10,
                      nkeep = 10,
                      selection = TRUE,
                      common_lambdas = TRUE,
                      printevery = 100, verbose = TRUE)
  set.seed(42)
  circle2.bark.depr = bark_mat(y.train=circle2[train,"y"], 
                               x.train=as.matrix(circle2[train, 1:2]),
                               x.test = as.matrix(circle2[-train, 1:2]),
                               classification = TRUE,
                               type="se",
                               nburn = 10,
                               nkeep = 10,
                               printevery = 100)
  #   skip_on_cran()
  expect_equal((circle2.bark$yhat.test.mean > 0) != circle2[-train, "y"],
               (circle2.bark.depr$yhat.test.mean > 0) != circle2[-train, "y"])
  
})


