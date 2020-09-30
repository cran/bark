# sim.Friedman2()
#' sim.Friedman2
#'
#' @param n
#' @param sd
#'
#' @return
#' @export
#'
#' @examples
sim.Friedman2 <- function(n, sd=125) {
  x <- cbind(runif(n, min=0, max=100),
             runif(n, min=40*pi, max=560*pi),
             runif(n, min=0, max=1),
             runif(n, min=1, max=11));

  y <- sqrt((x[,1]^2 + (x[,2]*x[,3] - 1/x[,2]/x[,4])^2));
  y <- y + rnorm(n, mean=0, sd=sd);
  return(list(x=x, y=y));
}
