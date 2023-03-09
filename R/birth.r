# Copyright (c) 2023 Merlise Clyde and Zhi Ouyang. All rights reserved
# See full license at
# https://github.com/merliseclyde/bark/blob/master/LICENSE.md
#
# SPDX-License-Identifier: GPL-3.0-or-later
##birth()
# birth step in RJ-MCMC
#  - keep (L, phi/z) fixed, only update (J, ci, varphi)
#  - birth from prior, random insert into existing kernels
#  - ci* uniformly from intercept and training sample index
#  - varphi* from gamma prior of varphi
#  - death according to multinomial draw w.r.t. power on varphi
birth <- function(y,          # response varaible continuous/[0/1] depend on classification
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
  alpha <- fixed$alpha;
  eps <- fixed$eps;
    
  accbirth <- 0;
  newtheta <- theta;
  exptoss <- rexp(1);
  
  newci <- sample(n+1, 1);
  newtheta$nvec[newci] <- nvec[newci] + 1;
  if(nvec[newci]==0){
    newtheta$varphi[newci] <- rgamma(1, alpha/2, alpha*eps^2/2);
  }

  deathprobs <- newtheta$nvec*(newtheta$varphi^tune$dpow);
  deathprob <- deathprobs[newci]/sum(deathprobs);
  logacc <- llike(y, X, newtheta, classification, fullXX) -
    llike(y, X, theta, classification, fullXX) +
    log(meanJ/newtheta$nvec[newci]) + log(deathprob) +
    log(pbd$pdJp1/pbd$pbJ);
  if(exptoss > - logacc){
    theta <- newtheta;
    accbirth <- 1;
  }
  return(list(theta=theta, accbirth=accbirth));
}
