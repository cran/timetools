#include <stdlib.h> // for NULL
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include "timetools.h"

#define CDEF(name, n, args)  {#name, (DL_FUNC) &name, n, args}

void myC(double *x, int *n, char **names, int *status);

static R_NativePrimitiveArgType ot_t[] = {
	INTSXP, INTSXP, INTSXP, INTSXP 
};

static R_NativePrimitiveArgType pni_t[] = {
	INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP
};

static R_NativePrimitiveArgType ppw_t[] = {
	INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP
};

static const R_CMethodDef cMethods[] = {
	CDEF(overlapping_timeintervaldf, 4, ot_t),
	CDEF(project_nb_intersections, 7, pni_t),
	CDEF(project_pos_weight, 9, ppw_t),
	{NULL, NULL, 0, NULL}
};

void R_init_timetools(DllInfo *dll)
{
	R_registerRoutines(dll, cMethods, NULL, NULL, NULL);
	R_useDynamicSymbols(dll, FALSE);
	R_forceSymbols(dll, TRUE);
}


