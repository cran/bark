# Copyright (c) 2023 Merlise Clyde and Zhi Ouyang. All rights reserved
# See full license at
# https://github.com/merliseclyde/bark/blob/master/LICENSE.md
#
# SPDX-License-Identifier: GPL-3.0-or-later
#
##updatephi()
# update the noise precision in the normal linear model
#  - because beta is itegrated out, no conjugate update
#  - update via normal random walk on log scale ($phistep, updtoss)
#  - non-informative prior 1/phi get canceled with the proposal ratio
updatephi <- function(y,          # response varaible continuous/[0/1] depend on classification
                      X,          # n*d covariate matrix
                      theta,      # list(p, nvec, varphi, beta, L, phi)
                      fixed,
                      tune,
                      classification,  # 0/1, normal linear/binary probit
                      fullXX=NULL # precalculated XX matrix
                      ){
  if(classification){
    stop("ERROR: cannot update phi for classification problems.") # nocov 
  }
  
  accupdatephi <- 0;
  newtheta <- theta;
  exptoss <- rexp(1);

  newtheta$phi <- rlognorm(1, log(theta$phi), tune$phistep);
  llik.old <- theta$llik.old;
  llik.new <- llike(y, X, newtheta, classification, fullXX);
#  llik.old <- llike(y, X, theta, classification, fullXX);
#  if (llik.old != theta$llik.old) {
#    print(paste("update.phi", llik.old, theta$llik.old));
#  }
  logacc <- llik.new - llik.old;  # missing prior?
  if(exptoss > - logacc){
    theta <- newtheta;
    theta$llik.old <- llik.new;
    accupdatephi <- 1;
  }
  return(list(theta=theta, accupdatediag=accupdatephi));
}
