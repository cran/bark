# SPDX-License-Identifier: GPL-3.0-or-later

##updateL.d()
updateL.d <- function(y,
                    X,
                    theta,
                    fixed,
                    tune,
                    classification
                    ){
  la <- fixed$la;
  lb <- fixed$lb;
  p <- fixed$p;
  
  accupdateL <- 0;
  newtheta <- theta;
  exptoss <- rexp(1);

  updind <- sample(fixed$p, 1);
  newtheta$L[updind] <- rlognorm(1, log(theta$L[updind]), tune$lstep);

  logacc <- llike(y, X, newtheta, classification) -
    llike(y, X, theta, classification) +
    dgamma(newtheta$L[updind], la/p, lb, log=T) -
    dgamma(theta$L[updind], la/p, lb, log=T) -
    log(theta$L[updind]) + log(newtheta$L[updind]);
  if(exptoss > - logacc){
    theta <- newtheta;
    accupdateL <- 1;
  }
  return(list(theta=theta, accupdateL=accupdateL));
}
