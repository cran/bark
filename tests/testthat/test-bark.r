test_that("new bark", {
  
  #regression
  traindata <- sim_Friedman2(200, sd=125)
  testdata <- sim_Friedman2(1000, sd=0)
  set.seed(42)
  fit.bark.depc <- bark_mat(traindata$x, traindata$y, testdata$x,
                         nburn=10, nkeep=100, keepevery=10,
                         classification=FALSE, type="d", printevery=10^10)
  set.seed(42)
  fit.bark  <- bark(y ~ ., data=data.frame(traindata), x.test=testdata$x,
                    nburn=10, nkeep=100, keepevery=10,
                    classification=FALSE, type="d", printevery=10^10)
  
  
  
   expect_equal(mean((fit.bark.depc$yhat.test.mean-testdata$y)^2),
                mean((fit.bark$yhat.test.mean-testdata$y)^2))
   
   set.seed(42)
   fit.bark.depc <- bark_mat(traindata$x, traindata$y, testdata$x,
                             nburn=10, nkeep=100, keepevery=10,
                             classification=FALSE, type="e", printevery=10^10)
   set.seed(42)
   fit.bark  <- bark(y ~ ., data=data.frame(traindata), x.test=testdata$x,
                     nburn=10, nkeep=100, keepevery=10,
                     classification=FALSE, type="e", printevery=10^10)
   
   
   
   expect_equal(mean((fit.bark.depc$yhat.test.mean-testdata$y)^2),
                mean((fit.bark$yhat.test.mean-testdata$y)^2))
   
   
   set.seed(42)
   fit.bark.depc <- bark_mat(traindata$x, traindata$y, testdata$x,
                             nburn=10, nkeep=100, keepevery=10,
                             classification=FALSE, type="sd", printevery=10^10)
   set.seed(42)
   fit.bark  <- bark(y ~ ., data=data.frame(traindata), x.test=testdata$x,
                     nburn=10, nkeep=100, keepevery=10,
                     classification=FALSE, type="sd", printevery=10^10)
   
   
   
   expect_equal(mean((fit.bark.depc$yhat.test.mean-testdata$y)^2),
                mean((fit.bark$yhat.test.mean-testdata$y)^2))
   
   set.seed(42)
   n = 500
   circle2 = data.frame(sim_circle(n, dim = 2))
   train = sample(1:n, size = floor(n/2), rep=FALSE)
   set.seed(42)
   circle2.bark = bark(y ~ ., data=circle2, subset=train,
                       x.test = as.matrix(circle2[-train, 1:2]),
                       classification = TRUE,
                       type="se",
                       nburn = 10,
                       nkeep = 100,
                       printevery = 10^10)
   set.seed(42)
   circle2.bark.depr = bark_mat(y.train=circle2[train,"y"], 
                                x.train=as.matrix(circle2[train, 1:2]),
                                x.test = as.matrix(circle2[-train, 1:2]),
                                classification = TRUE,
                                type="se",
                                nburn = 10,
                                nkeep = 100,
                                printevery = 10^10)
   expect_equal(mean((circle2.bark$yhat.test.mean > 0) != circle2[-train, "y"]),
                mean((circle2.bark.depr$yhat.test.mean > 0) != circle2[-train, "y"]))
                
})
