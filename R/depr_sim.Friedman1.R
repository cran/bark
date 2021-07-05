#' @title Simulated Regression Problem Friedman 1
#' @description The regression problem Friedman 1 as described in Friedman (1991) and
#' Breiman (1996). Inputs are 10 independent variables uniformly
#' distributed on the interval \eqn{[0,1]}, only 5 out of these 10 are actually
#' used. Outputs are created according to
#' the formula
#' \deqn{y = 10 \sin(\pi x1 x2) + 20 (x3 - 0.5)^2 + 10 x4 + 5 x5 + e}{
#'   y = 10 sin(\pi x1 x2) + 20 (x3 - 0.5)^2
#'  + 10 x4 + 5 x5 + e}
#'  where e is \eqn{N(0,sd^2)}.
#'
#' @param n number of data points to create
#' @param sd standard deviation of noise, with default value 1
#'
#' @return Returns a list with components
#' \item{x}{input values (independent variables)}
#' \item{y}{output values (dependent variable)}
#' @references Breiman, Leo (1996) Bagging predictors. Machine Learning 24,
#' pages 123-140. \cr
#' Friedman, Jerome H. (1991) Multivariate adaptive regression
#' splines. The Annals of Statistics 19 (1), pages 1-67.
#'
#' @examples \dontrun{
#' sim.Friedman1(100, sd=1)
#' }
#' @name  sim.Friedman1-deprecated
#' @usage sim.Friedman1(n, sd=1)
#' @family bark deprecated functions
#' @keywords internal
NULL

#' @rdname bark-package-deprecated
#' @section \code{sim.Friedman1}:
#' For \code{sim.Friedman1}, use \code{\link{sim_Friedman1}}.
#' @export
#'
# sim.Friedman1()
sim.Friedman1 <- function(n, sd=1) {
  x <- matrix(runif(n*10, min=0, max=1), ncol=10);
  y <- 10*sin(pi*x[,1]*x[,2]) + 20*(x[,3]-.5)^2 + 10*x[,4] + 10*x[,5];
  y <- y + rnorm(n, mean=0, sd=sd);
  return(list(x=x, y=y));
}
