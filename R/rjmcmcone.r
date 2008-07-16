##rjmcmcone()
# one iteration of reversible jump mcmc
# - choose birth/death/update
# - always update z/beta/phi
# - sometimes update L, and recalculate fullXX (tune$frequL)
rjmcmcone <- function(y,          # response varaible continuous/[0/1] depend on classification
                      X,          # n*d covariate matrix
                      theta,      # list(p, nvec, varphi, beta, L, phi)
                      fixed,
                      tune,
                      classification,  # 0/1, normal linear/binary probit
                      type,       # "e", "d", "se", "sd"
                      fullXX=NULL
                      ){
  if(is.null(fullXX)){
    fullXX <- getfulldesign(X, X, theta);
  }
  J <- sum(theta$nvec);
  if(J==1){
    pbd <- list(pbJ=.8, pdJ=.0, pbJp1=.4, pdJp1=.4, pbJm1=NA, pdJm1=NA);
  }else if(J==2){
    pbd <- list(pbJ=.4, pdJ=.4, pbJp1=.4, pdJp1=.4, pbJm1=.8, pdJm1=.0);
  }else{
    pbd <- list(pbJ=.4, pdJ=.4, pbJp1=.4, pdJp1=.4, pbJm1=.4, pdJm1=.4);
  }
  toss <- runif(1, min=0, max=1);
  
  if(toss<pbd$pbJ){                  #birth
    cur <- birth(y, X, theta, fixed, tune, pbd, classification, fullXX);
  }else if(toss<(pbd$pbJ+pbd$pdJ)) { #death
    cur <- death(y, X, theta, fixed, tune, pbd, classification, fullXX);
  }else{                             #update
    cur <- update(y, X, theta, fixed, tune, classification, fullXX);
  }
  if(classification){
    cur$theta <- updatebeta(y, X, cur$theta, fixed, classification, fullXX);
    cur$theta <- updatez(y, X, cur$theta, classification, fullXX);
  }else{
    cur$theta <- updatephi(y, X, cur$theta, fixed, tune, classification, fullXX)$theta;
  }
  if(runif(1) < tune$frequL){
    cur$theta <- updateL(y, X, cur$theta, fixed, tune, classification, type)$theta;
    fullXX <-  getfulldesign(X, X, cur$theta);
  }
  return(list(theta=cur$theta, fullXX=fullXX, acc=cur$acc));
}
