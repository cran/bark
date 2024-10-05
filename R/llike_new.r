
## llike()
# log likelihood calculation
# marginalized beta in the conjugate normal-gamma setting
# for binary model, likelihood conditional on hidden z values
llike<- function(y, # continuous or 0/1 response
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
  
  if (dim(XX)[2] == 1) {
    Sigma.inv = (t(XX) %*% XX + varphiovern/theta$phi)
    Sigma <- 1 / Sigma.inv
    evv <- list(values = Sigma.inv)
  } else {
    evv <- eigen(t(XX) %*% XX + diag(varphiovern/theta$phi), symmetric = TRUE)
    Sigma <- evv$vectors %*% diag(evv$values ^-1)  %*% t(evv$vectors)
  }

  mu <- Sigma %*% (t(XX) %*% y)
  ll <- 0.5*(length(y)*log(theta$phi / 2 / pi) +
             sum(log(varphiovern)) -
             sum(log(evv$values*theta$phi)) -
             theta$phi*(sum((y - XX %*% mu)^2) + 
                          sum(varphiovern * mu^2/theta$phi)) 
  )
  return(ll)
}


llike_C <- function(y, # continuous or 0/1 response
                 X, # n*d covariate matrix
                 theta, # list(p, nvec, varphi, beta, L, phi)
                 classification, # TRUE/FALSE class/regression
                 fullXX = NULL # precalculated XX matrix
) {
  if (classification) {
    y <- theta$z
  }
  if (is.null(fullXX)) {
    XX <- getdesign(X, X, theta)
  } else {
    XX <- matrix(fullXX[, theta$nvec > 0], ncol = sum(theta$nvec > 0))
  }
  
  .Call(C_llike,
        y,
        X, # n*d, data matrix vector
        theta, # p'*d, center matrix vector
        classification,
        fullXX)
}

