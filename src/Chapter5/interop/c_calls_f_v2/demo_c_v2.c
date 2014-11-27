/* File: demo_c_v2.c
 * Purpose: Main-program for application 'demo_c_v2'
 * Author: Dragos B. Chirila */

#include <stdlib.h>

/* declaration of Fortran procedure */
void test_proc_fort_v2(int n_f, double arr_f[3][2]);

int main() {
  int i, j, n_c=17;
  double arr_c[3][2];

  /* initialize 'arr_c' with some data */
  for(j=0; j<3; j++) {
    for(i=0; i<2; i++) {
      arr_c[j][i] = (double) (i+1)*(j+1);
    }
  }

  test_proc_fort_v2(n_c, arr_c);  /* C -calls-> Fort */

  return EXIT_SUCCESS;
}
