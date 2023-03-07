# SPDX-License-Identifier: GPL-3.0-or-later

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
  
  logacc <- llike(y, X, newtheta, classification) -
    llike(y, X, theta, classification) +
    dgamma(newsca, la, p*lb, log=T) -
    dgamma(oldsca, la, p*lb, log=T) -
    log(oldsca) + log(newsca);
  if(exptoss > - logacc){
    theta <- newtheta;
    accupdateL <- 1;
  }
  return(list(theta=theta, accupdateL=accupdateL));
}
