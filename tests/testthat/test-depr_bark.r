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
                    testdata= testdata$x,
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
                 testdata=testdata,
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

