#include <R.h>

void overlapping_timeintervaldf (int * start, int * end, int * length, int * ol)
{
	int i, j;
	int test = 0;
	*ol = 0;

	for (i = 0 ; i < *length ; i++)
	{
		for (j = i+1 ; j < *length ; j++)
		{
			if (*(end + i) <= *(start + j) ) continue ;
			if (*(start + i) >= *(end + j) ) continue ;
			if (*(start + i) == *(end + i) ) continue ;

			Rprintf ("i : %i - j : %i\n", i, j);
			*ol = 1;
			break;
		}
		if (*ol == 1) break;
	}
}
