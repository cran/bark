//kernelCalculationCall.cpp
// regression kernel calculations using c++

#include <iostream>
#include <math.h>
#include <R.h>
#include <Rinternals.h>

using namespace std;



extern "C"{
  // getDesignCpp: To calculate the design matrix in cpp code.
  //  Diagonal kernels with maximum 1.

  SEXP getDesignFail(SEXP RX,  	// n*d, data matrix vector
		             SEXP RC,  	// p*d, center matrix vector
		             SEXP RL,  	// d*1, kernel vector
		             SEXP Risi) // p*1, indicator of intercept
  {
		  int   *xdims = INTEGER(getAttrib(RX,R_DimSymbol));
//      int   *cdims = INTEGER(getAttrib(RC,R_DimSymbol));
		  int   n = xdims[0]; // n, number of observations
		  int   d = xdims[1];	// d, observation dimension
		  int   p = LENGTH(RC);

	// Add a check to see if p = cdims[0] and if not provide
	// a warning

		  int *isi, i, j, k;
		  double *x, *l, *c, *z,  eitem, esum;;

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
}

extern "C"{
  // getDesignCpp: To calculate the design matrix in cpp code.
  //  Diagonal kernels with maximum 1.

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
}

