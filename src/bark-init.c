#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>


/* .C calls */
extern void getDesignCpp(void *, void *, void *, void *, void *, void *, void *, void *);

static const R_CMethodDef CEntries[] = {
    {"getDesignCpp", (DL_FUNC) &getDesignCpp, 8},
    {NULL, NULL, 0}
};

void R_init_bark(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
