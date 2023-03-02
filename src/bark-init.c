#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>


/* .C calls */
extern void getDesignCpp(void *, void *, void *, void *, void *, void *, void *, void *);

/* .Call calls */
extern SEXP getDesign(SEXP, SEXP, SEXP, SEXP);

static const R_CMethodDef CEntries[] = {
  {"getDesignCpp", (DL_FUNC) &getDesignCpp, 8},
  {NULL, NULL, 0}
};

static const R_CallMethodDef CallEntries[] = {
  {"getDesign", (DL_FUNC) &getDesign, 4},
  {NULL, NULL, 0}
};

void R_init_bark(DllInfo *dll)
{
  R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}


