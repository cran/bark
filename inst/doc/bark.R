## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=FALSE--------------------------------------------------------------
#  devtools::install_github("merliseclyde/bark")

## -----------------------------------------------------------------------------
library(bark)

## ----Friedman2----------------------------------------------------------------
set.seed(42)
traindata <- sim_Friedman2(200, sd=125)
testdata <- sim_Friedman2(1000, sd=0)

## ----example-all--------------------------------------------------------------
set.seed(42)
fit.bark.d <- bark(y ~ ., data = data.frame(traindata),
                   testdata= data.frame(testdata),
                   classification=FALSE, 
                   selection = FALSE,
                   common_lambdas = FALSE,
                   nburn = 100,
                   nkeep = 250,
                   printevery = 10^10)

mean((fit.bark.d$yhat.test.mean-testdata$y)^2)


## ----example-selection--------------------------------------------------------
set.seed(42)
fit.bark.sd <- bark(y ~ ., data=data.frame(traindata),
                    testdata = data.frame(testdata),
                    classification=FALSE, 
                    selection = TRUE,
                    common_lambdas = FALSE,
                    nburn = 100,
                    nkeep = 250,
                    printevery = 10^10)

mean((fit.bark.sd$yhat.test.mean-testdata$y)^2)


## -----------------------------------------------------------------------------
boxplot(as.data.frame(fit.bark.d$theta.lambda))

## -----------------------------------------------------------------------------
boxplot(as.data.frame(fit.bark.sd$theta.lambda))

## -----------------------------------------------------------------------------
bart.available =  require(BART)
svm.available  =  require(e1071)

## -----------------------------------------------------------------------------
set.seed(42)
n = 500
circle2 = data.frame(sim_circle(n, dim = 2))
train = sample(1:n, size = floor(n/2), rep=FALSE)

## ---- fig.width=4, fig.height=4-----------------------------------------------
plot(x.1 ~ x.2, data=circle2, col=y+1)

## ----bark---------------------------------------------------------------------

circle2.bark = bark(y ~ ., data=circle2, subset=train,
                    testdata = circle2[-train,],
                    classification = TRUE,
                    selection = TRUE,
                    common_lambdas = TRUE,
                    nburn = 100,
                    nkeep = 250,
                    printevery = 10^10)

## -----------------------------------------------------------------------------
#Classify
#
mean((circle2.bark$yhat.test.mean > 0) != circle2[-train, "y"])

## ----svm----------------------------------------------------------------------
if (svm.available) {
  circle2.svm = svm(y ~ x.1 + x.2, data=circle2[train,], type="C")
  pred.svm = predict(circle2.svm, circle2[-train,])
  mean(pred.svm != circle2[-train, "y"])
}


## ----bart---------------------------------------------------------------------
if (bart.available) {
  circle.bart = pbart(x.train = circle2[train, 1:2], 
                            y.train =  circle2[train, "y"])
  pred.bart =   predict(circle.bart, circle2[-train, 1:2])
  mean((pred.bart$prob.test.mean > .5) != circle2[-train, "y"])
} 

## ----plots--------------------------------------------------------------------

plot(x.1 ~ x.2, data=circle2[-train,], pch = y+15, 
     col=(1 + (circle2.bark$yhat.test.mean > 0)),
     main="bark")

if (bart.available) {
  plot(x.1 ~ x.2, data=circle2[-train,], pch = y+15, 
       col= ( 1 + (pred.bart$prob.test.mean > .5)), 
       main="BART")
}

if (svm.available) {
plot(x.1 ~ x.2, data=circle2[-train,], pch = y+15, 
     col= as.numeric(pred.svm), 
     main="svm")
}



