# Copyright (c) 2023 Merlise Clyde and Zhi Ouyang. All rights reserved
# See full license at
# https://github.com/merliseclyde/bark/blob/master/LICENSE.md
#
# SPDX-License-Identifier: GPL-3.0-or-later
#' @title bark:  Bayesian Additive Regression Trees
#' @description Implementation of Bayesian Additive Regression Kernels
#'  with Feature Selection for  Nonparametric Regression for
#'  Gaussian regression
#'  and classification for binary Probit models
#'
#' @docType package
#' @name bark-package
#'
#' @details BARK is a Bayesian \emph{sum-of-kernels} model or because of the
#' Bayesian priors is a Bayesian Additive Regression Kernel model. \cr
#' For numeric response \eqn{y}, we have
#' \eqn{y = f(x) + \epsilon}{y = f(x) + e},
#' where \eqn{\epsilon \sim N(0,\sigma^2)}{e ~ N(0,sigma\^2)}.\cr
#' For a binary response \eqn{y}, \eqn{P(Y=1 | x) = F(f(x))}, where \eqn{F}
#' denotes the standard normal cdf (probit link).

#' In both cases, \eqn{f} is the sum of many Gaussian kernel functions.
#' The goal is to have very flexible inference for the unknown
#' function \eqn{f}.
#' bark  uses an approximated Cauchy process as the prior distribution
#' for the unknown function \eqn{f}.

#' Feature selection can be achieved through the inference
#' on the scale parameters in the Gaussian kernels.
#' BARK accepts four different types of prior distributions through setting
#' values for \code{selection} (TRUE or FALSE), which allows scale parameters
#' for some variables to be set to zero, removing the variables from the
#' kernels \code{selection = TRUE}; this enables either soft shrinkage or hard 
#' shrinkage for the scale
#' parameters. The input \code{common_lambdas} (TRUE or FALSE) specifies whether
#' a common scale parameter should be used for all predictors (TRUE) or
#' if FALSE allows the scale parameters to differ across all variabless
#' in the kernel.
#' 
#'
#' @examples
#' \donttest{
#'  # Simulate regression example
#'  # Friedman 2 data set, 200 noisy training, 1000 noise free testing
#'  # Out of sample MSE in SVM (default RBF): 6500 (sd. 1600)
#'  # Out of sample MSE in BART (default):    5300 (sd. 1000)
#'  traindata <- sim_Friedman2(200, sd=125)
#'  testdata <- sim_Friedman2(1000, sd=0)
#'  fit.bark.d <- bark(y ~ ., data = data.frame(traindata),
#'                     testdata = data.frame(testdata), 
#'                     classification = FALSE,
#'                     selection = FALSE,
#'                     common_lambdas = TRUE)
#'  boxplot(as.data.frame(fit.bark.d$theta.lambda))
#'  mean((fit.bark.d$yhat.test.mean-testdata$y)^2)

#'  # Simulate classification example
#'  # Circle 5 with 2 signals and three noisy dimensions
#'  # Out of sample erorr rate in SVM (default RBF): 0.110 (sd. 0.02)
#'  # Out of sample error rate in BART (default):    0.065 (sd. 0.02)
#'  traindata <- sim_circle(200, dim=5)
#'  testdata <- sim_circle(1000, dim=5)
#'  fit.bark.se <- bark(y ~ ., data= data.frame(traindata), 
#'                      testdata= data.frame(testdata),
#'                      classification=TRUE, 
#'                      selection=TRUE,
#'                      common_lambdas = FALSE)
#'                    
#'  boxplot(as.data.frame(fit.bark.se$theta.lambda))
#'  mean((fit.bark.se$yhat.test.mean>0)!=testdata$y)
#' }
#'
#' @references
#' Ouyang, Zhi (2008) Bayesian Additive Regression Kernels.
#' Duke University. PhD dissertation, Chapter 3.
#'
#'
#' @import stats
#'
#' @useDynLib bark, .registration = TRUE, .fixes="C_"
#'
#' @family bark functions
#'
NULL




