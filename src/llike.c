/*
## llike()
# log likelihood calculation
# marginalized beta in the conjugate normal-gamma setting
# for binary model, likelihood conditional on hidden z values
llike_new <- function(y, # continuous or 0/1 response
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
*/

/*
 * Copyright (c) 2023 Merlise Clyde and Zhi Ouyang. All rights reserved
 * See full license at
 * https://github.com/merliseclyde/bark/blob/master/LICENSE.md
 */

// SPDX-License-Identifier: GPL-3.0-or-later
// kernelCalculationCall.cpp
// regression kernel calculations using C


#include "bark.h"

SEXP llike(SEXP RY, // nx1 response
               SEXP RX,  	// n*d, data matrix vector
               SEXP Rtheta,  	// 
               SEXP RClass,  	// dindicator for reg or classification
               SEXP RkXX) //  design matrix for kernel
{
  int nProtected  = 0;
  int *xdims = INTEGER(getAttrib(RkXX,R_DimSymbol));
  int n = LENGTH(RY);
  int d = xdims[1];	// d, ncols design
  
  int classification = LOGICAL(RClass)[0];
  
  
  SEXP ANS = PROTECT(allocVector(VECSXP, 4)); ++nProtected;
  SEXP ANS_names = PROTECT(allocVector(STRSXP, 4)); ++nProtected;
  
  SEXP mu = PROTECT(duplicate(RY)); ++nProtected;
  SEXP llik = PROTECT(allocVector(REALSXP, 1)); ++nProtected;
  SEXP RXwork = PROTECT(duplicate(RkXX)); ++nProtected;
  
  int k = 0, i = 0;
  double *nvec = REAL(getListElement(Rtheta, "nvec")); 
  double *varphi = REAL(getListElement(Rtheta, "varphi")); 
  double phi = REAL(getListElement(Rtheta, "phi"))[0];
  SEXP varphiovern = PROTECT(duplicate(getListElement(Rtheta, "varphi")));++nProtected;

  
  double one=1.0, zero=0.0; 
  if (classification == 1) {
    double *Y = REAL(getListElement(Rtheta, "z"));
  }
  else {
   double *Y = REAL(RY); 
  }
  
  memset(REAL(varphiovern), 0.0, d*sizeof(double));
  for (i = 0; i < d; i++) {
    if (nvec[i] > 0)  {
      REAL(varphiovern)[k] = varphi[k]/(nvec[i]*nvec[i]*phi);
      k += 1; 
    }
  }
  
  Rprintf("k = %d\n", k);
  
  int k2 = k*k;
  SEXP RkXtX = PROTECT(allocVector(REALSXP, k2)); ++nProtected;
  memset(REAL(RkXtX), 0.0, k2*sizeof(double));
 
  SEXP beta = PROTECT(allocVector(REALSXP, k)); ++nProtected;
  memset(REAL(beta), 0.0, k*sizeof(double));
  
  
  double *kXtX = REAL(RkXtX);  
  double *Xwork = REAL(RXwork);
  // compute kXX^TkXX
  // F77_NAME(dsyrk)("U", "T", &p, &n, &one, &Xwork[0], &n, &zero, &kXtX[0], &p, FCONE, FCONE);
  F77_NAME(dsyrk)("U", "T", &k, &n, &one, &Xwork[0], &n, &zero, kXtX, &k FCONE FCONE);
 
  for (i=0; i < k; i++) {
    REAL(RkXtX)[i*(k+1)] = REAL(varphiovern)[i];
  }
  
  SET_VECTOR_ELT(ANS, 0, llik );
  SET_STRING_ELT(ANS_names, 0, mkChar("loglik"));
  SET_VECTOR_ELT(ANS, 1, beta );
  SET_STRING_ELT(ANS_names, 0, mkChar("beta"));
  SET_VECTOR_ELT(ANS, 2, mu );
  SET_STRING_ELT(ANS_names, 2, mkChar("mu"));
  SET_VECTOR_ELT(ANS, 3, RkXtX );
  SET_STRING_ELT(ANS_names, 3, mkChar("varphi"));
  setAttrib(ANS, R_NamesSymbol, ANS_names);
  
  UNPROTECT(nProtected);
  
  return(ANS);
}
