# Copyright (c) 2023 Merlise Clyde and Zhi Ouyang. All rights reserved
# See full license at
# https://github.com/merliseclyde/bark/blob/master/LICENSE.md
#
# SPDX-License-Identifier: GPL-3.0-or-later
#
##update()
# update some parameters within rjmcmc
# - update ci, varphi  respectively
# - exclude L, because fullXX may need recalculated.
# - exclude the updates for z/beta/phi (alwasy update them in rjmcmc)
update <- function(y,          # response varaible continuous/[0/1] depend on modeltype
                   X,          # n*d covariate matrix
                   theta,      # list(p, nvec, varphi, beta, L, phi)
                   fixed,
                   tune,
                   modeltype,
                   fullXX=NULL
                   ){
  toss <- runif(1, min=0, max=1);
  if(toss < .5){
    cur <- updateci(y, X, theta, fixed, tune, modeltype, fullXX);
  }else{
    cur <- updatevarphi(y, X, theta, fixed, tune, modeltype, fullXX);
  }
  return(cur);
}
