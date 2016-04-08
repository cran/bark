##Simulate regression example
# Friedman 2 data set, 200 noisy training, 1000 noise free testing
# Out of sample MSE in SVM (default RBF): 6500 (sd. 1600)
# Out of sample MSE in BART (default):    5300 (sd. 1000)
traindata <- sim.Friedman2(200, sd=125)
testdata <- sim.Friedman2(1000, sd=0)
fit.bark.d <- bark(traindata$x, traindata$y, testdata$x, classification=FALSE, type="d")
boxplot(as.data.frame(fit.bark.d$theta.lambda))
mean((fit.bark.d$yhat.test.mean-testdata$y)^2)

##Simulate classification example
# Circle 5 with 2 signals and three noisy dimensions
# Out of sample erorr rate in SVM (default RBF): 0.110 (sd. 0.02)
# Out of sample error rate in BART (default):    0.065 (sd. 0.02)
traindata <- sim.Circle(200, dim=5)
testdata <- sim.Circle(1000, dim=5)
fit.bark.se <- bark(traindata$x, traindata$y, testdata$x, classification=TRUE, type="se")
boxplot(as.data.frame(fit.bark.se$theta.lambda))
mean((fit.bark.se$yhat.test.mean>0)!=testdata$y)
