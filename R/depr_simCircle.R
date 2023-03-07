# Copyright (c) 2023 Merlise Clyde and Zhi Ouyang. All rights reserved
# See full license at
# https://github.com/merliseclyde/bark/blob/master/LICENSE.md
#
# SPDX-License-Identifier: GPL-3.0-or-later
#
#' @title Simulate Data from Hyper-Sphere for Classification Problems
#' @description The classification problem Circle is described in the BARK paper (2008).
#' Inputs are \emph{dim} independent variables uniformly
#' distributed on the interval \eqn{[-1,1]}, only the first 2
#' out of these \emph{dim} are actually signals.
#' Outputs are created according to the formula
#' \deqn{y = 1(x1^2+x2^2 \le 2/\pi)}

#'
#' @param n number of data points to generate
#' @param dim number of dimension of the problem, no less than 2
#'
#' @return Returns a list with components
#' \item{x}{input values (independent variables)}
#' \item{y}{0/1 output values (dependent variable)}
#'
#' @examples
#' \dontrun{
#'   sim.Circle(n=100, dim = 5)
#' }
#' @references Ouyang, Zhi (2008) Bayesian Additive Regression Kernels.
#' Duke University. PhD dissertation, Chapter 3.
#' \cr
#' 
#'
#' @name  sim.Circle-deprecated
#' @usage sim.Circle(n, dim=5)
#' @family bark deprecated functions
#' @keywords internal
NULL

#' @rdname bark-package-deprecated
#' @section \code{sim.Circle}:
#' For \code{sim.Circle}, use \code{\link{sim_circle}}.
#' @export
## sim.Circle()
sim.Circle <- function(n, dim = 5) {
  .Deprecated("sim_circle")
  if (dim < 2) { # start nocov
    stop("number of variables must be >= 2.")  #  end nocov
  }
  x <- matrix(runif(n * dim, min = -1, max = 1), nrow = n)
  r2 <- x[, 1]^2 + x[, 2]^2
  y <- rep(0, n)
  y[r2 <= 2 / pi] <- 1
  return(list(x = x, y = y))
}
