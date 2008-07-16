##updateci()
# update step for center index in theta
# eqivalent to a death + birth step together
#  - keep the number of kernels fixed
#  - first select one existing location j, with nj>0
#  - selection probability is power on varphi ($upow) for those nj>0
#  - delete the selected kernel and add a new kernel at random location
#  - if location new, also propose new varphi
updateci <- function(y,          # response varaible continuous/[0/1] depend on classification
                     X,          # n*d covariate matrix
                     theta,      # list(ci, L, varphi, phi/z)
                     fixed,      # list(n, d, alpha, eps, gam, sizeJ, meanJ, la, lb)
                     tune,       # list(dpow, upow, varphistep, lstep, rstep, phistep, updtoss)
                     classification,
                     fullXX=NULL
                     ){
  nvec <- theta$nvec;
  J <- sum(nvec);

  n <- fixed$n;
  meanJ <- fixed$meanJ;
  alpha <- fixed$alpha;
  eps <- fixed$eps;

  accupdateci <- 0;
  newtheta <- theta;
  exptoss <- rexp(1);
  dpsfromold <- nvec*(theta$varphi^tune$dpow);
  dpsfromold <- dpsfromold/sum(dpsfromold);
  
  delind <- sample(1:(n+1), 1, replace=T, prob=dpsfromold);
  newind <- sample(n+1, 1);
  newtheta$nvec[delind] <- newtheta$nvec[delind] - 1;
  newtheta$nvec[newind] <- newtheta$nvec[newind] + 1;
  if(nvec[newind] == 0){
    newtheta$varphi[newind] <- rgamma(1, alpha/2, alpha*eps^2/2);
  }
  dpsfromnew <- newtheta$nvec*(newtheta$varphi^tune$dpow);
  dpsfromnew <- dpsfromnew/sum(dpsfromnew);

  logacc <- llike(y, X, newtheta, classification, fullXX) -
    llike(y, X, theta, classification, fullXX) +
    log(nvec[delind]/(nvec[newind]+1)) +
    log(dpsfromnew[newind]/dpsfromold[delind]);

  if(exptoss > - logacc){
    theta <- newtheta;
    accupdateci <- 1;
  }
  return(list(theta=theta, accupdateci=accupdateci));
}
