#include <R.h>
#include <Rinternals.h>

void overlapping_timeintervaldf (int * start, int * end, int * length, int * ol)
{
	int i, j;
	*ol = 0;

	for (i = 0 ; i < *length ; i++)
	{
		for (j = i+1 ; j < *length ; j++)
		{
			if (*(end + i) <= *(start + j) ) continue ;
			if (*(start + i) >= *(end + j) ) continue ;
			if (*(start + i) == *(end + i) ) continue ;

			Rprintf ("i : %i - j : %i\n", i+1, j+1);
			*ol = 1;
			break;
		}
		if (*ol == 1) break;
	}
}

SEXP overlapping_timeintervaldf_logical (SEXP start, SEXP end, SEXP length)
{
	int i, j, k, nb;
	int *s = INTEGER(start);
	int *e = INTEGER(end);
	int  l = INTEGER(length)[0];

	nb = 0;
	for (i = 0 ; i < l ; i++)
	{
		for (j = i+1 ; j < l ; j++)
		{
			if (*(e + i) <= *(s + j) ) continue ;
			if (*(s + i) >= *(e + j) ) continue ;
			if (*(s + i) == *(e + i) ) continue ;

			Rprintf ("i : %i - j : %i\n", i+1, j+1);
			nb++;
		}
	}

	SEXP ol = PROTECT(allocVector(INTSXP, nb));
	int * oli = INTEGER(ol);
	k = 0;
	for (i = 0 ; i < l ; i++)
	{
		for (j = i+1 ; j < l ; j++)
		{
			if (*(e + i) <= *(s + j) ) continue ;
			if (*(s + i) >= *(e + j) ) continue ;
			if (*(s + i) == *(e + i) ) continue ;

			*(oli + k) = i * l + j;
			k++;
		}
	}
	UNPROTECT(1);
	return ol;
}
