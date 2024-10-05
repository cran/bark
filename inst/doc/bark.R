## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
#  devtools::install_github("merliseclyde/bark")

## -----------------------------------------------------------------------------
library(bark)

## ----Friedman2----------------------------------------------------------------
set.seed(42)
traindata <- data.frame(sim_Friedman2(200, sd=125))
testdata <- data.frame(sim_Friedman2(1000, sd=0))

## ----example-all--------------------------------------------------------------
set.seed(42)
fit.bark.d <- bark(y ~ ., data = traindata,
                   testdata= testdata,
                   classification=FALSE, 
                   selection = FALSE,
                   common_lambdas = FALSE,
#                   fixed = list(eps = .25, gam = 2.5),
                   nburn = 100,
                   nkeep = 250,
                   printevery = 10^10)

mean((fit.bark.d$yhat.test.mean-testdata$y)^2)


## ----example-selection--------------------------------------------------------
set.seed(42)
fit.bark.sd <- bark(y ~ ., data=traindata,
                    testdata = testdata,
                    classification=FALSE, 
                    selection = TRUE,
                    common_lambdas = FALSE,
                    fixed = list(eps = .5, gam = 5),
                    nburn = 100,
                    nkeep = 250,
                    printevery = 10^10)

mean((fit.bark.sd$yhat.test.mean-testdata$y)^2)


## -----------------------------------------------------------------------------
boxplot(as.data.frame(fit.bark.d$theta.lambda))

## -----------------------------------------------------------------------------
boxplot(as.data.frame(fit.bark.sd$theta.lambda))

## -----------------------------------------------------------------------------
bart.available =  suppressMessages(require(BART))
svm.available  =  suppressMessages(require(e1071))
io.available  =  suppressMessages(require(fdm2id))

## ----svm-reg------------------------------------------------------------------
if (svm.available) {
  friedman2.svm = svm(y ~ ., data=traindata, type="eps-regression")
  pred.svm = predict(friedman2.svm, testdata)
  mean((pred.svm - testdata$y)^2)
}


## ----bart-reg-----------------------------------------------------------------
if (bart.available) {
 y.loc = match("y", colnames(traindata))
 friedman2.bart = wbart(x.train = as.matrix(traindata[ , -y.loc]), 
                        y.train =  traindata$y)
  pred.bart =   predict(friedman2.bart, 
                        as.matrix(testdata[ , -y.loc]))
  yhat.bart = apply(pred.bart, 2, mean)
  mean((yhat.bart - testdata$y)^2)
} 

## -----------------------------------------------------------------------------
set.seed(42)
n = 500
circle2 = data.frame(sim_circle(n, dim = 5))
train = sample(1:n, size = floor(n/2), rep=FALSE)

## ----fig.width=4, fig.height=4------------------------------------------------
plot(x.1 ~ x.2, data=circle2, col=y+1)

## ----bark---------------------------------------------------------------------
set.seed(42)
circle2.bark = bark(y ~ ., data=circle2, subset=train,
                    testdata = circle2[-train,],
                    classification = TRUE,
                    selection = TRUE,
                    common_lambdas = FALSE,
                    fixed = list(eps = .5, gam = 5),
                    nburn = 100,
                    nkeep = 250,
                    printevery = 10^10)

## -----------------------------------------------------------------------------
#Classify
#
mean((circle2.bark$yhat.test.mean > 0) != circle2[-train, "y"])

## -----------------------------------------------------------------------------
boxplot(as.data.frame(circle2.bark$theta.lambda))

## ----svm----------------------------------------------------------------------
if (svm.available) {
  circle2.svm = svm(y ~ ., data=circle2[train,], type="C")
  pred.svm = predict(circle2.svm, circle2[-train,])
  mean(pred.svm != circle2[-train, "y"])
}


## ----bart---------------------------------------------------------------------
if (bart.available) {
  y.loc = match("y", colnames(circle2))
  circle.bart = pbart(x.train = as.matrix(circle2[train, -y.loc]), 
                            y.train =  circle2[train, y.loc])
  pred.bart =   predict(circle.bart, as.matrix(circle2[-train, -y.loc]))
  mean((pred.bart$prob.test.mean > .5) != circle2[-train, y.loc])
} 

