# Copyright (c) 2023 Merlise Clyde and Zhi Ouyang. All rights reserved
# See full license at
# https://github.com/merliseclyde/bark/blob/master/LICENSE.md
#
# SPDX-License-Identifier: GPL-3.0-or-later
#
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
  llik.old <- theta$llik.old;
  llik.new <- llike(y, X, newtheta, classification) ;
#  llik.old <- llike(y, X, theta, classification);
#  if (llik.old != theta$llik.old) {
#    print(paste("update.ld", llik.old, theta$llik.old));
#  }
  logacc <- llik.new - llik.old +
    dgamma(newtheta$L[updind], la/p, lb, log=T) -
    dgamma(theta$L[updind], la/p, lb, log=T) -
    log(theta$L[updind]) + log(newtheta$L[updind]);
  if(exptoss > - logacc){
    theta <- newtheta;
    theta$llik.old <- llik.new;
    accupdateL <- 1;
  }
  return(list(theta=theta, accupdateL=accupdateL));
}
