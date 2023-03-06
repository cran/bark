# SPDX-License-Identifier: GPL-3.0-or-later

##common.r
#  Bayesian Additive Regression Kernel - Commen Functions
#  Common Distributions
#   - dlognorm()
#   - rlognorm()
#   - ddir()
#   - rdir()
#  Matrix Operations
#   - matAdd()
#   - matDel()
#   - matUpd()
#  Other Functions
#   - updacc()

##Load MVTNORM library
#  This is installed for both 32bit and 64bit machine
#  Do not need to indicate different local library path
#  HAVE BUG FOR NEW MACHINES: monstar
#  Tentative Solution: call the library somewhere else
#library(mvtnorm);


##rlognorm()
# random generator of log normal distribution
rlognorm <- function(n, m, s){
  x <- exp(rnorm(n, m, s));
  return(x);
}

# nocov start
##dlognorm()
# density of log normal distribution
dlognorm <- function(x, m, s, log=FALSE){
  den <- -log(2*pi)/2 - log(x) - log(s) -(log(x)-m)^2/(2*s^2);
  if(log==FALSE){
    den <- exp(den);
  }
  return(den);
}

##rdir()
# random generator of Dirichlet distribution
rdir <- function(n, alpha){
  l <- length(alpha);
  x <- matrix(rgamma(l*n, alpha), ncol=l, byrow=TRUE);
  sm <- x %*% rep(1, l);
  x1 <- x/as.vector(sm);
  if(n == 1){
    x1 <- as.vector(x1);
  }
  return(x1);
}

##ddir()
# density of Dirichlet distribution
ddir <- function(x, alpha, log=FALSE){
  logC <- lgamma(sum(alpha)) - sum(lgamma(alpha));
  logS <- sum((alpha-1) * log(x));
  den <- logS + logC;
  if(log==FALSE){
    den <- exp(den);
  }
  return(den);
}



# nocov end