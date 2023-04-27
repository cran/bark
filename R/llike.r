
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
  
  if (dim(XX)[2] == 1) {
    Sigma.inv = (theta$phi * t(XX) %*% XX + varphiovern)
    Sigma <- 1 / Sigma.inv
    evv <- list(values = Sigma.inv)
  } else {
    evv <- eigen(theta$phi*t(XX) %*% XX + diag(varphiovern), symmetric = TRUE)
    Sigma <- evv$vectors %*% diag(evv$values ^-1)  %*% t(evv$vectors)
  }
  mu <- theta$phi * Sigma %*% (t(XX) %*% y)
  ll <- length(y) / 2 * log(theta$phi / 2 / pi) +
    sum(log(varphiovern)) / 2 -
    sum(log(evv$values)) / 2 -
    (theta$phi * sum((y - XX %*% mu)^2) + sum(varphiovern * mu^2)) / 2
  return(ll)
}
