# SPDX-License-Identifier: GPL-3.0-or-later

##updatebeta()
# update step for regression coefficients
#  - conjugate normal update conditional on rest
# - use the posterior mean in regression (Rao-Blackwellization)
# - use the posterior sample in probit model (fully Bayes)
updatebeta <- function(y,          # response varaible continuous/[0/1] depend on classification
                       X,          # n*d covariate matrix
                       theta,      # list(p, nvec, varphi, beta, L, phi)
                       fixed,      # list(n, d, alpha, eps, gam, meanJ, palpha, la, lb)
                       classification,
                       fullXX=NULL # precalculated XX matrix
                       ){
  if(classification){
    y <- theta$z;
    theta$phi <- 1;
  }
  if(is.null(fullXX)){
    XX <- getdesign(X, X, theta); # nocov  
  }else{
    XX <- matrix(fullXX[, theta$nvec>0], ncol=sum(theta$nvec>0));
  }
  varphiovern <- theta$varphi[theta$nvec>0]/theta$nvec[theta$nvec>0]^2;
  evv <- eigen(t(XX)%*%XX, symmetric=TRUE, EISPACK=TRUE);
  ivals <- 1/(theta$phi*evv$values + varphiovern);
  if(dim(XX)[2] == 1){
    Sigma <- 1/(theta$phi*t(XX)%*%XX + varphiovern);
  }else{
    Sigma <- evv$vectors %*% diag(ivals) %*% t(evv$vectors)
  }
  mu <- theta$phi*Sigma%*%(t(XX)%*%y);
  theta$beta <- rep(0, 1+fixed$n);
  if(!classification){ # posterior mean (Rao-Blackwellization)
    theta$beta[theta$nvec>0] <- mu;
  }else{
    theta$beta[theta$nvec>0] <- mu + evv$vectors %*% rnorm(length(mu), 0, sd=sqrt(ivals));
  }
  return(theta);
}
