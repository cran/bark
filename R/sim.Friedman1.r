# sim.Friedman1()
sim.Friedman1 <- function(n, sd=1) {
  x <- matrix(runif(n*10, min=0, max=1), nc=10);
  y <- 10*sin(pi*x[,1]*x[,2]) + 20*(x[,3]-.5)^2 + 10*x[,4] + 10*x[,5];
  y <- y + rnorm(n, mean=0, sd=sd);
  return(list(x=x, y=y));
}
