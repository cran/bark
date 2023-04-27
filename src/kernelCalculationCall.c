/*
 * Copyright (c) 2023 Merlise Clyde and Zhi Ouyang. All rights reserved
 * See full license at
 * https://github.com/merliseclyde/bark/blob/master/LICENSE.md
 */

// SPDX-License-Identifier: GPL-3.0-or-later
// kernelCalculationCall.cpp
// regression kernel calculations using C


#include <math.h>
#include <R.h>
#include <Rinternals.h>


  // getDesign: To calculate the design matrix in C code.
  // Diagonal kernels with maximum 1.

  SEXP getDesign(SEXP RX,  	// n*d, data matrix vector
                 SEXP RC,  	// p*d, center matrix vector
                 SEXP RL,  	// d*1, kernel vector
                 SEXP Risi) // p*1, indicator of intercept
  {
    int   *xdims = INTEGER(getAttrib(RX,R_DimSymbol));
    int   *cdims = INTEGER(getAttrib(RC,R_DimSymbol));
    int   n = xdims[0]; // n, number of observations
    int   d = xdims[1];	// d, observation dimension
    int   p = cdims[0];

    int *isi, i, j, k;
    double *x, *l, *c, *z,  eitem, esum;;

/*    
      xdim = dim(X)
      cdim = dim(center)
      ldim = length(L)
      idim = length(intercept)
      
      if (xdim[2] !=  ldim || xdim[2] != cdim[2] || cdim[1] != idim) {
        stop("diminsions of inputs to createDesign due not conform")
      }
 */
    isi = INTEGER(Risi);
    x = REAL(RX);
    c = REAL(RC);
    l = REAL(RL);

    SEXP RZ = PROTECT(allocMatrix(REALSXP, n, p));
    z = REAL(RZ);

    //Calculate the exponent multiplied by the constant
    for(i=0; i < n; i++){
      for(j=0; j < p; j++){
        if(isi[j] == 1){
          z[i+j*n] = 1.0;
        } else{
          esum = 0.0;
          for(k=0; k< d; k++){
            eitem = x[i + n*k] - c[j + p*k];
            esum -= l[k] * eitem * eitem;
          }
          z[i + j*n] = pow(M_E, esum);
        }
      }
    }
    UNPROTECT(1);
    return(RZ);
  }


