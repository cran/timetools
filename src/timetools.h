/* overlapping.c */
extern void overlapping_timeintervaldf(int *start, int *end, int *length, int *ol);
extern SEXP overlapping_timeintervaldf_logical(SEXP start, SEXP end, SEXP length);
/* project_intersection.c */
extern void project_nb_intersections(int *s1, int *e1, int *l1, int *s2, int *e2, int *l2, int *nb);
extern void project_pos_weight(int *s1, int *e1, int *l1, int *s2, int *e2, int *l2, int *pos1, int *pos2, int *weight);
