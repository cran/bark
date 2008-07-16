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

  logacc <- llike(y, X, newtheta, classification, fullXX) -
    llike(y, X, theta, classification, fullXX) +
    log(nvec[deathci]/meanJ) - log(deathprob) +
    log(pbd$pbJm1/pbd$pdJ);
  if(exptoss > - logacc){
    theta <- newtheta;
    accdeath <- 1;
  }
  return(list(theta=theta, accdeath=accdeath));
}
