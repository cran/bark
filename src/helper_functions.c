/* Modified to be compatible with R memory allocation */
#include <stdio.h>
#include <stdlib.h>
#include <R.h>
#include "bark.h"



/* getListElement from Writing R Extensions */


     SEXP getListElement(SEXP list, char *str)
     {
       SEXP elmt = R_NilValue, names = getAttrib(list, R_NamesSymbol);
       int i;

       for (i = 0; i < length(list); i++)
         if(strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
           elmt = VECTOR_ELT(list, i);
           break;
         }
       return elmt;
     }


