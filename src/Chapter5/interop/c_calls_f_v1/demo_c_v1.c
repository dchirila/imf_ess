/* File: demo_c_v1.c
 * Purpose: Main-program for application 'demo_c_v1'
 * Author: Dragos B. Chirila */

#include <stdlib.h>

/* declaration of Fortran procedure */
void test_proc_fort_v1(void);

int main() {
  test_proc_fort_v1();  /* C -calls-> Fort */

  return EXIT_SUCCESS;
}
