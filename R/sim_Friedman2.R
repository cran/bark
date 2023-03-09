# Copyright (c) 2023 Merlise Clyde and Zhi Ouyang. All rights reserved
# See full license at
# https://github.com/merliseclyde/bark/blob/master/LICENSE.md
#
# SPDX-License-Identifier: GPL-3.0-or-later
#
#' @title Simulated Regression Problem Friedman 2
#' @description The regression problem Friedman 2 as described in Friedman (1991) and
#' Breiman (1996). Inputs are 4 independent variables uniformly
#' distributed over the ranges
#' \deqn{0 \le x1 \le 100}
#' \deqn{40 \pi \le x2 \le 560 \pi}
#' \deqn{0 \le x3 \le 1}
#' \deqn{1 \le x4 \le 11}

#' The outputs are created according to the formula
#' \deqn{y = (x1^2 + (x2 x3 - (1/(x2 x4)))^2)^{0.5} + e}
#' where e is \eqn{N(0,sd^2)}.
#'
#' @param n number of data points to create
#' @param sd Standard deviation of noise. The default value of 125 gives
#' a signal to noise ratio (i.e., the ratio of the standard deviations) of
#' 3:1. Thus, the variance of the function itself (without noise)
#' accounts for 90\% of the total variance.
#'
#' @return Returns a list with components
#' \item{x}{input values (independent variables)}
#' \item{y}{output values (dependent variable)}
#' @references Breiman, Leo (1996) Bagging predictors. Machine Learning 24,
#' pages 123-140. \cr
#' Friedman, Jerome H. (1991) Multivariate adaptive regression
#' splines. The Annals of Statistics 19 (1), pages 1-67.
#'
#' @examples
#' sim_Friedman2(100, sd=125)
#' @family bark simulation functions
#' @family bark functions
#' @export
# sim_Friedman2()
sim_Friedman2 <- function(n, sd=125) {
  x <- cbind(runif(n, min=0, max=100),
             runif(n, min=40*pi, max=560*pi),
             runif(n, min=0, max=1),
             runif(n, min=1, max=11));

  y <- sqrt((x[,1]^2 + (x[,2]*x[,3] - 1/x[,2]/x[,4])^2));
  y <- y + rnorm(n, mean=0, sd=sd);
  return(list(x=x, y=y));
}
