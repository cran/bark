# Copyright (c) 2023 Merlise Clyde and Zhi Ouyang. All rights reserved
# See full license at
# https://github.com/merliseclyde/bark/blob/master/LICENSE.md
#
# SPDX-License-Identifier: GPL-3.0-or-later
#

# Calculate  design matrix for kernels
#
createDesignCpp <- function(X, center, L, intercept, n,
                            p, d) {
  z <- .C(C_getDesignCpp,
          as.double(X), # n*d, data matrix vector
          as.double(center), # p'*d, center matrix vector
          as.double(L), # d*1, kernel vector
          as.integer(intercept), # p'*1, indicator of intercept
          as.integer(n), # n, number of observations
          as.integer(p), # p, number of kernels (model dimension)
          as.integer(d), # d, observation dimension
          z = double(n * p))$z # n*p' design matrix
  z <- matrix(z, ncol = p)  # coerce to matrix
}



createDesignCall <- function(X, center, L, intercept) {
  xdim = dim(X)
  cdim = dim(center)
  ldim = length(L)
  idim = length(intercept)
  
  if (xdim[2] !=  ldim || xdim[2] != cdim[2] || cdim[1] != idim) {
    stop("diminsions of inputs to createDesign due not conform")
  }
  .Call(C_getDesign,
        X, # n*d, data matrix vector
        center, # p'*d, center matrix vector
        as.double(L), # d*1, kernel vector
        as.integer(intercept)) # p'*1, indicator of intercept
}

createDesign <- function(X, center, L, intercept, n, p, d) {
  xdim = dim(X)
  cdim = dim(center)
  ldim = length(L)
  idim = length(intercept)
  
  if (xdim[2] !=  ldim || xdim[2] != cdim[2] || cdim[1] != idim) {
    stop(paste("diminsions of inputs to createDesign do not conform\n
         (X, C, L, I) = ",  c(xdim, cdim, ldim, idim)))
  }
  .Call(C_getDesign,
        X, # n*d, data matrix vector
        center, # p'*d, center matrix vector
        as.double(L), # d*1, kernel vector
        as.integer(intercept)) # p'*1, indicator of intercept
}
## getdesign()
#  Use cpp code in the shared library to calculate the kernel design matrix
#  get the design matrix for those nvec>0 columns only
getdesign <- function(dMat, # n*d, data matrix
                      cMat, # p*d, center matrix
                      theta # list(L, nvec, ...)
) {
  nvec <- theta$nvec
  cMat1 <- rbind(1, cMat)
  isi <- rep(0, sum(nvec > 0))
  if (nvec[1] > 0) {
    isi[1] <- 1
  }
  n = nrow(dMat)
  p = length(isi)
  d = ncol(dMat)
  z <- createDesign(dMat, # n*d, data matrix vector
                    cMat1[nvec > 0, , drop=FALSE], # p'*d, center matrix vector
                    theta$L, # d*1, kernel vector
                    isi, # p'*1, indicator of intercept
                    n, # n, number of observations
                    p, # p, number of kernels (model dimension)
                    d) # d, observation dimension
  # n*p' design matrix
  return(z)
}


## getfulldesign()
#  Use cpp code in the shared library to calculate the kernel design matrix
#   - get the full design matrix for intercept and all cMat columns
getfulldesign <- function(dMat, # data matrix
                          cMat, # center matrix
                          theta # list(L, ...)
) {
  nvec <- theta$nvec
  cMat1 <- rbind(1, cMat)
  isi <- c(1, rep(0, dim(cMat)[1]))
  n = nrow(dMat)
  p = length(isi)
  d = ncol(dMat)
  z <- createDesign(dMat, # n*d, data matrix vector
                    cMat1, # p'*d, center matrix vector
                    theta$L, # d*1, kernel vector
                    isi, # p'*1, indicator of intercept
                    n, # n, number of observations
                    p, # p, number of kernels (model dimension)
                    d) # d, observation dimension
  return(z)
}


