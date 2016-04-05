##sim.Circle()
sim.Circle <- function(n, dim=5) {
  if (dim < 2) {
    stop ("number of variables must be >= 2.");
  }
  x <- matrix(runif(n*dim, min=-1, max=1), nrow=n);
  r2 <- x[, 1]^2 + x[, 2]^2;
  y <- rep(0, n);
  y[r2 <= 2/pi] <- 1;
  return (list(x=x, y=y));
}
