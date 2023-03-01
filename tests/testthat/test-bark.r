test_that("new bark", {
  traindata <- sim.Friedman2(200, sd=125)
  testdata <- sim.Friedman2(1000, sd=0)
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
                
})
