# Copyright (c) 2023 Merlise Clyde and Zhi Ouyang. All rights reserved
# See full license at
# https://github.com/merliseclyde/bark/blob/master/LICENSE.md
#
# SPDX-License-Identifier: GPL-3.0-or-later
#
##fliponeL.se()
# switch the sign of one lambda element
fliponeL.se <- function(y,
                       X,
                       theta,
                       fixed,
                       tune,
                       classification
                       ){
  p <- fixed$p;
  la <- fixed$la;
  lb <- fixed$lb;
  pbetaa <- fixed$pbetaa;
  pbetab <- fixed$pbetab;
  
  lamzerop <- theta$lamzerop;

  accupdateoneL <- 0;
  newtheta <- theta;
  exptoss <- rexp(1);

  d <- sum(theta$L > 0);
  switchid <- sample(p, 1);
  if(theta$L[switchid] == 0){
    if(d == 0){
      newtheta$L[switchid] <- rgamma(1, la, lb);
      logppratio <- 0;
    }else{
      newtheta$L[switchid] <- max(theta$L);
      logppratio <- dgamma(max(theta$L), la, (d+1)*lb, log=T) -
        dgamma(max(theta$L), la, d*lb, log=T);
    }
    # log PP ratio (prior ratio and propsal ratio)
    # notice the boundary case switch from one non-zero case
    # to all zero case is the same! with RJ-MCMC algorithm
    logppratio <- logppratio + log((1-lamzerop)/lamzerop);
  }else{
    newtheta$L[switchid] <- 0;
    if(d == 0){ # nocov start
      stop("Error: please report bug on https://github.com/merliseclyde/bark 
           Issue of all porposed lambdas zero with selection and common lambda")
      logppratio <- 0; # nocov end
    }else{
      logppratio <- dgamma(max(theta$L), la, d*lb, log=T) -
        dgamma(max(theta$L), la, (d+1)*lb, log=T);
    }
    logppratio <- logppratio + log(lamzerop/(1-lamzerop));
  }
  logacc <- llike(y, X, newtheta, classification) -
    llike(y, X, theta, classification) + logppratio;
  if(exptoss > - logacc){
    theta <- newtheta;
    accupdateoneL <- 1;
  }
  d0 <- sum(theta$L==0);
  d1 <- p - d0;
  theta$lamzerop <- rbeta(1, pbetaa + d0, pbetab + d1);
  return(list(theta=theta, accupdateoneL=accupdateoneL));  
}


##updateoneL.se()
# update the scaling factor in lambda
updateoneL.se <- function(y,
                    X,
                    theta,
                    fixed,
                    tune,
                    classification
                    ){
  la <- fixed$la;
  lb <- fixed$lb;
  
  accupdateL <- 0;
  newtheta <- theta;
  exptoss <- rexp(1);

  d <- sum(theta$L > 0);
  if (d == 0){ # nocov start
    stop("Not expecting no nonzero lambda update; please report issue
         on https://github.com/merliseclyde/bark"); # nocov end
  }
  movesca <- rlognorm(1, 0, tune$lstep);
  newtheta$L <- theta$L * movesca;
  newsca <- max(newtheta$L);
  oldsca <- max(theta$L);
  
  logacc <- llike(y, X, newtheta, classification) -
    llike(y, X, theta, classification) +
    dgamma(newsca, la, d*lb, log=T) -
    dgamma(oldsca, la, d*lb, log=T) -
    log(oldsca) + log(newsca);
  if(exptoss > - logacc){
    theta <- newtheta;
    accupdateL <- 1;
  }
  return(list(theta=theta, accupdateL=accupdateL));
}


##updateL.se()
updateL.se <- function(y,
                       X,
                       theta,
                       fixed,
                       tune,
                       classification
                       ){
  cur <- fliponeL.se(y, X, theta, fixed, tune, classification);
  theta <- cur$theta;
  if (sum(theta$L>0) > 0){
    cur <- updateoneL.se(y, X, theta, fixed, tune, classification);
  }
  return(cur);
}
