# Copyright (c) 2023 Merlise Clyde and Zhi Ouyang. All rights reserved
# See full license at
# https://github.com/merliseclyde/bark/blob/master/LICENSE.md
#
# SPDX-License-Identifier: GPL-3.0-or-later
##death()
# death step in RJ-MCMC
#  - death according to multinomial draw w.r.t. power on varphi
#  - birth from prior, random insert into existing kernels
#  - ci* uniformly from intercept and training sample index
#  - varphi* from gamma prior of varphi
death <- function(y,          # response varaible continuous/[0/1] depend on classification
                  X,          # n*d covariate matrix
                  theta,      # list(ci, L, varphi, phi/z)
                  fixed,      # list(n, d, alpha, eps, gam, sizeJ, meanJ, la, lb)
                  tune,       # list(dpow, upow, varphistep, lstep, rstep, phistep, updtoss)
                  pbd,        # list(pbJ, pdJ, pbJp1, pdJp1, pbJm1, pdJm1)
                  classification,
                  fullXX=NULL
                  ){
  nvec <- theta$nvec;
  J <- sum(nvec);

  n <- fixed$n;
  meanJ <- fixed$meanJ;
  
  accdeath <- 0;
  newtheta <- theta;
  exptoss <- rexp(1);

  deathprobs <- nvec*(theta$varphi^tune$dpow);
  deathci <- sample(1:(n+1), 1, replace=T, prob=deathprobs);
  deathprob <- deathprobs[deathci]/sum(deathprobs);
  newtheta$nvec[deathci] <- newtheta$nvec[deathci] - 1;
  llik.old <- theta$llik.old;
  llik.new <- llike(y, X, newtheta, classification, fullXX);
#  llik.old <- llike(y, X, theta, classification, fullXX);
#  if (theta$llik.old != llik.old) {
#    stop(paste("problem in birth"));
#  }
  logacc <-  llik.new - llik.old +
    log(nvec[deathci]/meanJ) - log(deathprob) +
    log(pbd$pbJm1/pbd$pdJ);
  if(exptoss > - logacc){
    theta <- newtheta;
    theta$llik.old <-  llik.new;
    accdeath <- 1;
  }
  return(list(theta=theta, accdeath=accdeath));
}
