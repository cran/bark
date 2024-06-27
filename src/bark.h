#define USE_FC_LEN_T
#include <Rconfig.h>
#include <R_ext/BLAS.h>
#ifndef FCONE
# define FCONE
#endif

#include <stdio.h> // for NULL
#include <math.h>
#include <string.h>
#include <float.h>
#include <R.h>
#include <Rdefines.h>
#include <Rmath.h>
#include <Rinternals.h>
#include <R_ext/Constants.h>
#include <R_ext/Applic.h>
#include <R_ext/Rdynload.h>

SEXP getListElement(SEXP list, char *str);
