#include <R.h>

void project_nb_intersections (int * s1, int * e1, int * l1, int * s2, int * e2, int * l2, int * nb)
{
	int i, j ;
	int t1 = 0, t2 = 0;
	*nb = 0 ;
	for (i = 0 ; i < *l1 ; i++)
		for (j = 0 ; j < *l2 ; j++)
		{
			t1 = ( *(s1 + i) != *(e1 + i) ) ;
			t1 = (t1 && ( *(s2 + j) != *(e2 + j) ) ) ;

			t2 = *(s1 + i) < *(e2 + j) ;
			t2 = t2 && *(e1 + i) > *(s2 + j);

			t1 = t1 && t2 ;
			if (t1 == 1) (*nb)++ ;
		}
}


void project_pos_weight (int * s1, int * e1, int * l1, int * s2, int * e2, int * l2, int * pos1, int * pos2, int * weight)
{
	int i, j, nb = 0, s = 0, e = 0;
	int t1 = 0, t2 = 0 ;
	for (i = 0 ; i < *l1 ; i++)
		for (j = 0 ; j < *l2 ; j++)
		{
			t1 = ( *(s1 + i) != *(e1 + i) ) ;
			t1 = (t1 && ( *(s2 + j) != *(e2 + j) ) ) ;

			t2 = *(s1 + i) < *(e2 + j) ;
			t2 = t2 && *(e1 + i) > *(s2 + j);

			t1 = t1 && t2 ;
			if (t1 == 1)
			{
				*(pos1 + nb) = i + 1;
				*(pos2 + nb) = j + 1;

				if (*(s1 + i) > *(s2 + j)) {
					s = *(s1 + i);
				} else {
					s = *(s2 + j);
				}
				if (*(e1 + i) < *(e2 + j)) {
					e = *(e1 + i);
				} else {
					e = *(e2 + j);
				}

				*(weight + nb) = e - s;
				nb++;
			}
		}
}


