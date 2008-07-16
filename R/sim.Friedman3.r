# sim.Friedman3()
sim.Friedman3 <- function(n, sd=0.1) {
  x <- cbind(runif(n, min=0, max=100),
             runif(n, min=40*pi, max=560*pi),
             runif(n, min=0, max=1),
             runif(n, min=1, max=11));
  y <-  atan((x[,2]*x[,3] - 1/x[,2]/x[,4])/x[,1]);
  y <- y + rnorm(n, mean=0, sd=sd);
  return(list(x=x, y=y));
}
