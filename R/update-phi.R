# SPDX-License-Identifier: GPL-3.0-or-later

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
  
  logacc <- llike(y, X, newtheta, classification, fullXX) -
    llike(y, X, theta, classification, fullXX);
  if(exptoss > - logacc){
    theta <- newtheta;
    accupdatephi <- 1;
  }
  return(list(theta=theta, accupdatediag=accupdatephi));
}
