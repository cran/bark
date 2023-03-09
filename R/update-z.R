# Copyright (c) 2023 Merlise Clyde and Zhi Ouyang. All rights reserved
# See full license at
# https://github.com/merliseclyde/bark/blob/master/LICENSE.md
#
# SPDX-License-Identifier: GPL-3.0-or-later
#
#updatez()
# update the augmented normal variable z in a gibbs move for probit model
# - no need for tuning parameter, always accept
# - recommend first update beta, then z.
# - any number whose absolute value > 20 shrinks to (+-)20
updatez <- function(y,          # response varaible continuous/[0/1] depend on classification
                    X,          # n*d covariate matrix
                    theta,      # list(p, nvec, varphi, beta, L, phi)
                    classification,  # 0/1, normal linear/binary probit
                    fullXX=NULL # precalculated XX matrix
                    ){
  if(!classification){
    stop("ERROR: cannot update z for non probit models.")  # nocov
  }
  if(is.null(fullXX)){
    XX <- getdesign(X, X, theta);
  }else{
    XX <- matrix(fullXX[, theta$nvec>0], ncol=sum(theta$nvec>0));
  }
  z <- rep(NA, length(y));
  u <- runif(length(y));
  Xb <- XX %*% theta$beta[theta$nvec>0];
  z[y==1] <- Xb[y==1] + qnorm(u[y==1] * pnorm(Xb[y==1]) + pnorm(-Xb[y==1]));
  z[y==0] <- Xb[y==0] + qnorm(u[y==0] * pnorm(-Xb[y==0]));
  z[z > 20] <- 20;
  z[z < -20] <- -20;
  theta$z <- z;
  return(theta);
}
