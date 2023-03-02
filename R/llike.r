## likeli.r
#  Bayesian Additive Regression Kernel - likelihood functions
#   - getdesign()
#   - getfulldesign()
#   - llike()
#   - getmeanJ()

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


## getmeanJ()
# calculate meanJ from the approximated alpha stable process
getmeanJ <- function(alpha,
                     eps,
                     gam) {
  if (alpha <= 0 | alpha >= 2) {
    stop("ERROR: stable index alpha is not in (0, 2).")
  } else if (alpha == 1) {
    meanJ <- gam / eps
  } else {
    meanJ <- (gam * alpha^(1 - alpha / 2) / eps^alpha * sqrt(pi)) *
      (gamma(alpha) * gamma(alpha / 2) / gamma((alpha + 1) / 2)) * sin(pi * alpha / 2)
  }
  return(meanJ)
}


## llike()
# log likelihood calculation
# marginalized beta in the conjugate normal-gamma setting
# for binary model, likelihood conditional on hidden z values
llike <- function(y, # continuous or 0/1 response
                  X, # n*d covariate matrix
                  theta, # list(p, nvec, varphi, beta, L, phi)
                  classification, # TRUE/FALSE class/regression
                  fullXX = NULL # precalculated XX matrix
) {
  if (classification) {
    y <- theta$z
    theta$phi <- 1
  }
  if (is.null(fullXX)) {
    XX <- getdesign(X, X, theta)
  } else {
    XX <- matrix(fullXX[, theta$nvec > 0], ncol = sum(theta$nvec > 0))
  }
  
  varphiovern <- theta$varphi[theta$nvec > 0] / theta$nvec[theta$nvec > 0]^2
  evv <- eigen(t(XX) %*% XX, symmetric = TRUE)
  if (dim(XX)[2] == 1) {
    Sigma <- 1 / (theta$phi * t(XX) %*% XX + varphiovern)
  } else {
    Sigma <- evv$vectors %*% diag(1 / (theta$phi * evv$values +
      varphiovern)) %*% t(evv$vectors)
  }
  mu <- theta$phi * Sigma %*% (t(XX) %*% y)
  ll <- length(y) / 2 * log(theta$phi / 2 / pi) +
    sum(log(varphiovern)) / 2 -
    sum(log(varphiovern + theta$phi * evv$values)) / 2 -
    (theta$phi * sum((y - XX %*% mu)^2) + sum(varphiovern * mu^2)) / 2
  return(ll)
}
