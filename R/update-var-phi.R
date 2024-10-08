# Copyright (c) 2023 Merlise Clyde and Zhi Ouyang. All rights reserved
# See full license at
# https://github.com/merliseclyde/bark/blob/master/LICENSE.md
#
# SPDX-License-Identifier: GPL-3.0-or-later
#
##updatevarphi()
# update step for unit normal precisions
#  - only update one varphi, where nj>0
#  - selection probability is uniform
#  - once selected, update via normal random walk on log scale ($varphistep)
updatevarphi <- function(y,          # response varaible continuous/[0/1] depend on classification
                         X,          # n*d covariate matrix
                         theta,      # list(ci, L, varphi, phi/z)
                         fixed,      # list(n, d, alpha, eps, gam, sizeJ, meanJ, la, lb)
                         tune,       # list(dpow, upow, varphistep, lstep, rstep, phistep, updtoss)
                         classification,
                         fullXX=NULL
                         ){
  nvec <- theta$nvec;
  varphi <- theta$varphi;
  
  n <- fixed$n;
  alpha <- fixed$alpha;
  eps <- fixed$eps;
  
  accupdatevarphi <- 0;
  newtheta <- theta;
  exptoss <- rexp(1);

  probs <- rep(1, n+1);
  probs[nvec == 0] <- 0;
  updind <- sample(1:(n+1), 1, replace=T, prob=probs);
  newtheta$varphi[updind] <- rlognorm(1, log(theta$varphi[updind]), tune$varphistep);
  
  llik.old <- theta$llik.old ;
  llik.new <- llike(y, X, newtheta, classification, fullXX);
#  llik.old <- llike(y, X, theta, classification, fullXX);
#  if (llik.old != theta$llik.old) {
#    print(paste("update.varphi", llik.old, theta$llik.old));
#  }
  logacc <- llik.new - llik.old +
    dgamma(newtheta$varphi[updind], alpha/2, alpha*eps^2/2, log=T) -
    dgamma(varphi[updind], alpha/2, alpha*eps^2/2, log=T) -
    log(varphi[updind]) + log(newtheta$varphi[updind]);
  
  if(exptoss > - logacc){
    theta <- newtheta;
    theta$llik.old = llik.new;
    accupdatevarphi <- 1;
  }
  return(list(theta=theta, accupdatevarphi=accupdatevarphi));
}
