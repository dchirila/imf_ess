#!/bin/bash -x
set -e

#rm *.o demo_c_v2

# Compile
gfortran -c test_proc_fort_v2.f90
gcc -c demo_c_v2.c

# Link
gcc -o demo_c_v2 demo_c_v2.o test_proc_fort_v2.o -lgfortran

# Run
./demo_c_v2
