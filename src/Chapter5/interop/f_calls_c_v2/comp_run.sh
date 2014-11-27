#!/bin/bash -x
set -e

#rm *.o demo_fort_v2

# Compile
gcc -c test_proc_c_v2.c
gfortran -c demo_fort_v2.f90

# Link
gfortran -o demo_fort_v2 demo_fort_v2.o test_proc_c_v2.o -lc

# Run
./demo_fort_v2
