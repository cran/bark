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

##dlognorm()
# density of log normal distribution
dlognorm <- function(x, m, s, log=FALSE){
  den <- -log(2*pi)/2 - log(x) - log(s) -(log(x)-m)^2/(2*s^2);
  if(log==FALSE){
    den <- exp(den);
  }
  return(den);
}
##rlognorm()
# random generator of log normal distribution
rlognorm <- function(n, m, s){
  x <- exp(rnorm(n, m, s));
  return(x);
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

##matAdd()
# matrix addition, add one or several rows at the bottom
matAdd <- function(mat, new){
  if(!is.matrix(mat)){
    stop("**ERROR[matAdd]: Input 'mat' is not in matrix form!**");
  }
  if(!is.matrix(new)){
    stop("**ERROR[matAdd]: Input 'new' is not in matrix form!**");
  }
  return(rbind(mat, new));
}

##matDel()
# matrix deletion, delete one or several rows
matDel <- function(mat, del){
  if(!is.matrix(mat)){
    stop("**ERROR[matDel]: Input 'mat' is not in matrix form!**");
  }
  dimmat <- dim(mat);
  J <- dimmat[1];
  if(min(del)<=0 | max(del)>J){
    stop("**ERROR[matDel]: Input 'del' is outside of index range!**");
  }
  if(length(del) > J){
    stop("**ERROR[matDel]: Input 'del' is longer than dimension of 'mat'!**");
  }else if(length(del) == J){
    return(matrix(0, nrow=0, ncol=dimmat[2]));
  }else if(length(del) +1 == J){
    return(matrix(mat[-del, ], nrow=1));
  }else{
    return(matrix(mat[-del, ], ncol=dimmat[2]));
  }
}

##matUpd()
# matrix update, change one or several rows
# note: ind is within the row index of upd
matUpd <- function(mat, upd, ind){
  if(is.matrix(mat)==FALSE){
    stop("**ERROR[matUpd]: Input 'mat' is not a matrix!**");
  }
  if(length(mat)==0){
    return(mat);
  }
  dimmat <- dim(mat);
  if(ind<=0 | ind>dimmat[1]){
    stop("**ERROR[matUpd]: Input 'ind' is outside dimension of input 'mat'!**")
  }
  mat[ind, ] <- upd;
  if(dimmat[1] == 1){
    mat <- matrix(mat, nrow=1);
  }
  return(mat);
}

##updateacc()
# update acceptance rate, with the last element of list cur
updacc <- function(acc,    # acceptance rate matrix
                   cur     # current list, last item is acc atom
                      ){
  len <- dim(acc)[1];
  lencur <- length(cur);
  if(!names(cur)[lencur] %in% rownames(acc)){
    acc <- matAdd(acc, matrix(c(cur[[lencur]], 1), nrow=1));
    rownames(acc)[len+1] <- names(cur)[lencur];
  }else{
    ind <- which(names(cur)[lencur] == rownames(acc));
    acc[ind, ] <- acc[ind, ] + c(cur[[lencur]], 1);
  }
  return(acc);
}

