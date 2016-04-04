\name{bark-package}
\alias{bark-package}
\docType{package}
\title{
 Bayesian Additive Regression Kernels
}
\description{
  Implementation of BARK:
  Bayesian Additive Regression Kernels with Feature Selection,
  Zhi Ouyang, Ph.D. thesis, Duke University
}
\details{
\tabular{ll}{
Package: \tab bark\cr
Type: \tab Package\cr
Version: \tab 0.1-0\cr
Date: \tab 2008-07-16\cr
License: \tab GPL version 2 or newer\cr
LazyLoad: \tab yes\cr
}
Overview:\cr
  BARK is a Bayesian \emph{sum-of-kernels} model or because of the
  Bayesian priors is a Bayesian Additive Regression Kernel model. \cr
  For numeric response \eqn{y}, we have
  \eqn{y = f(x) + \epsilon}{y = f(x) + e},
  where \eqn{\epsilon \sim N(0,\sigma^2)}{e ~ N(0,sigma\^2)}.\cr
  For a binary response \eqn{y}, \eqn{P(Y=1 | x) = F(f(x))}, where \eqn{F}
  denotes the standard normal cdf (probit link).

  In both cases, \eqn{f} is the sum of many Gaussian kernel functions.
  The goal is to have very flexible inference for the unknown
  function \eqn{f}.
  It uses an approximated Cauchy process as the prior distribution
  for the unknown function \eqn{f}.

  Feature selection can be achieved through the inference
  on the scale parameters in the Gaussian kernels.
  BARK accepts four different types of prior distributions,
  \code{e}, \code{d}, \code{se}, \code{sd}, enabling
  either soft shrinkage or hard shrinkage for the scale
  parameters.
  
Functions in the package include:\cr
{\code{\link{bark}}}()\cr
sim.Friedman1()\cr
sim.Friedman2()\cr
sim.Friedman3()\cr
sim.Circle()\cr
}
\author{
  Zhi Ouyang <zhi.ouyang@gmail.com>
  Merlise Clyde <clyde@stat.duke.edu>,
  Robert Wolpert <wolpert@stat.duke.edu>

  Maintainer: Zhi Ouyang <zo2@stat.duke.edu>
}
\references{
  Ouyang, Zhi (2008) Bayesian Additive Regression Kernels.
  Duke University. Ph.D. dissertation, Chapter 3.
  \cr
  at:
  \url{http://stat.duke.edu/people/theses/OuyangZ.html}
}

\examples{
\dontrun{
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
}
}
