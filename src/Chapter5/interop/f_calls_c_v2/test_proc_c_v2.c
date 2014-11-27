/* File: test_proc_c_v2.c
 * Purpose: Code for called function, for application 'demo_fort_v2'
 * Author: Dragos B. Chirila */

#include <stdio.h>

void test_proc_c_v2(int n, double arr[3][2]) {
  int i, j;

  printf("Hello from \"test_proc_c_v2(C)\", "
         "invoked from \"demo_fort_v2(Fort)\"!\n");
  printf("n = %d\n", n);
  for(j=0; j<3; j++) {
    for(i=0; i<2; i++) {
      printf("arr[%d,%d,%d] = %8.2f\n", j, i, arr[j][i]);
    }
  }
}
