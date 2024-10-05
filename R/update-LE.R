# Copyright (c) 2023 Merlise Clyde and Zhi Ouyang. All rights reserved
# See full license at
# https://github.com/merliseclyde/bark/blob/master/LICENSE.md
#
# SPDX-License-Identifier: GPL-3.0-or-later
#
##updateL.e()
updateL.e <- function(y,
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
  
  movesca <- rlognorm(1, 0, tune$lstep);
  newtheta$L <- theta$L * movesca;
  newsca <- max(newtheta$L);
  oldsca <- max(theta$L);
  llik.new <- llike(y, X, newtheta, classification);
  llik.old <- theta$llik.old;  
# llik.old <- llike(y, X, theta, classification); 
#  if (llik.old != theta$llik.old) {
#    print(paste("update.le", llik.old, theta$llik.old));
#  }
  logacc <-  llik.new - llik.old +
    dgamma(newsca, la, p*lb, log=T) -
    dgamma(oldsca, la, p*lb, log=T) -
    log(oldsca) + log(newsca);
  if(exptoss > - logacc){
    theta <- newtheta;
    theta$llik.old <- llik.new;
    accupdateL <- 1;
  }
  return(list(theta=theta, accupdateL=accupdateL));
}
