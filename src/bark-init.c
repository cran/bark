/*
 * Copyright (c) 2023 Merlise Clyde and Zhi Ouyang. All rights reserved
 * See full license at
 * https://github.com/merliseclyde/bark/blob/master/LICENSE.md
*/

// SPDX-License-Identifier: GPL-3.0-or-later

#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>


/* .Call calls */
extern SEXP getDesign(SEXP, SEXP, SEXP, SEXP);
extern SEXP llike(SEXP, SEXP, SEXP, SEXP, SEXP);


static const R_CallMethodDef CallEntries[] = {
  {"getDesign", (DL_FUNC) &getDesign, 4},
  {"llike", (DL_FUNC) &llike, 5},
  {NULL, NULL, 0}
};

void R_init_bark(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}


