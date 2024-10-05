/*
 * Copyright (c) 2023 Merlise Clyde and Zhi Ouyang. All rights reserved
 * See full license at
 * https://github.com/merliseclyde/bark/blob/master/LICENSE.md
 */


// SPDX-License-Identifier: GPL-3.0-or-later

// kernelCalculation.cpp
// regression kernel calculations using c++

#include <iostream>
#include <math.h>
#include "R.h"

using namespace std;



extern "C"{
  // getDesignCpp: To calculate the design matrix in cpp code.
  //  Diagonal kernels with maximum 1.

void getDesignCpp(double *x,  	// n*d, data matrix vector
		   double *c,  	// p*d, center matrix vector
		   double *l,  	// d*1, kernel vector
		   int *isi,    // p*1, indicator of intercept
		   int *n, 	// n, number of observations
		   int *p, 	// p, number of kernels (model dimension)
		   int *d, 	// d, observation dimension
		   double *z	// n*p, design matrix in a vector
		   ){
    int i, j, k;
    double eitem, esum;

    //Calculate the exponent multiplied by the constant
    for(i=0; i<*n; i++){
      for(j=0; j<*p; j++){
	if(isi[j] == 1)  {
	  z[i+j*(*n)] = 1;
	} else{
	  esum = 0.0;
	  for(k=0; k<*d; k++) {
	    eitem = x[i+(*n)*k] - c[j+(*p)*k];
	    esum -= l[k] * eitem * eitem;
	  }
	  z[i+j*(*n)] = pow(M_E, esum);
	}
      }
    }
  }
}
