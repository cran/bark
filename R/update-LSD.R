# SPDX-License-Identifier: GPL-3.0-or-later

##fliponeL.sd()
# flip the sign of one lambda element
# half half zero nonzero flip, except all zero/nonzero
fliponeL.sd <- function(y,
                     X,
                     theta,
                     fixed,
                     tune,
                     classification
                     ){
  p <- fixed$p;
  pbetaa <- fixed$pbetaa;
  pbetab <- fixed$pbetab;
  
  la <- fixed$la;
  lb <- fixed$lb;
  
  lamzerop <- theta$lamzerop;

  accfliponeL <- 0;
  newtheta <- theta;
  exptoss <- rexp(1);

  isnonzero <- as.numeric(theta$L>0);
  d <- sum(isnonzero);
  if(min(d, p-d) > 0){
    toss <- runif(1, min=0, max=1);
    if(toss <= 0.5){  # death
      switchid <- sample(1:p, 1, prob=isnonzero);
      logppratio <- log(lamzerop/(1-lamzerop)) + log(d/(p-d+1));
      if (d == 1) {
        logppratio <- logppratio + log(2);
      } else {
        fixL <- theta$L;
        fixL[switchid] <- 0;
        fixL <- fixL[fixL>0];
        logppratio <- logppratio +
          sum(dgamma(fixL, la/(d-1), lb, log=T)) -
            sum(dgamma(fixL, la/d, lb, log=T));
      }
    }else{  # birth
      switchid <- sample(1:p, 1, prob=1-isnonzero);
      logppratio <- log((1-lamzerop)/lamzerop) + log((p-d)/(d+1));
      if (d == p-1) {
        logppratio <- logppratio + log(2);
      } else {
        fixL <- theta$L[theta$L > 0];
        logppratio <- logppratio +
          sum(dgamma(fixL, la/(d+1), lb, log=T)) -
            sum(dgamma(fixL, la/d, lb, log=T));
      }
    }
  }else{
    switchid <- sample(p, 1);
    if (d == 0) {  # birth
      logppratio <- log((1-lamzerop)/lamzerop) + log(p/2);
    } else {  # death
      fixL <- theta$L[-switchid];
      logppratio <- log(lamzerop/(1-lamzerop)) + log(1/(2*p)) +
        sum(dgamma(fixL, la/(d-1), lb, log=T)) -
          sum(dgamma(fixL, la/d, lb, log=T));
    }
  }
  if(theta$L[switchid] == 0){
    # birth, flip a zero to non-zero element
    newtheta$L[switchid] <- rgamma(1, la/(d+1), lb);
  }else{
    # death, flip a non-zero element to zero
    newtheta$L[switchid] <- 0;
  }
  
  logacc <- llike(y, X, newtheta, classification) -
    llike(y, X, theta, classification) + logppratio;
  if(exptoss > - logacc){
    theta <- newtheta;
    accfliponeL <- 1;
  }

  d0 <- sum(theta$L==0);
  d1 <- p - d0;
  theta$lamzerop <- rbeta(1, pbetaa + d0, pbetab + d1);
  return(list(theta=theta, accfliponeL=accfliponeL));
}


##updateoneL.sd()
# update one non-zero lambda
updateoneL.sd <- function(y,
                       X,
                       theta,
                       fixed,
                       tune,
                       classification
                       ){
  p <- fixed$p;
  pbetaa <- fixed$pbetaa;
  pbetab <- fixed$pbetab;
  
  la <- fixed$la;
  lb <- fixed$lb;
  
  lamzerop <- theta$lamzerop;

  accupdateoneL <- 0;
  newtheta <- theta;
  exptoss <- rexp(1);
  
  isnonzero <- as.numeric(theta$L>0);
  d <- sum(isnonzero);
  if(d > 0){
    switchid <- sample(1:p, 1, prob=isnonzero);
    newtheta$L[switchid] <- rlognorm(1,
                                     log(theta$L[switchid]),
                                     tune$lstep);
    logacc <- llike(y, X, newtheta, classification) -
      llike(y, X, theta, classification) +
        dgamma(newtheta$L[switchid], la/d, lb, log=T) -
          dgamma(theta$L[switchid], la/d, lb, log=T) -
    log(theta$L[switchid]) + log(newtheta$L[switchid]);
    if(exptoss > - logacc){
      theta <- newtheta;
      accupdateoneL <- 1;
    }
  }

  return(list(theta=theta, accupdateoneL=accupdateoneL)); 
}

##updateL.sd()
updateL.sd <- function(y,
                    X,
                    theta,
                    fixed,
                    tune,
                    classification
                    ){
  cur <- fliponeL.sd(y, X, theta, fixed, tune, classification);
  theta <- cur$theta;
  if (sum(theta$L>0) > 0){
    cur <- updateoneL.sd(y, X, theta, fixed, tune, classification);
  }
  return(cur);
}
