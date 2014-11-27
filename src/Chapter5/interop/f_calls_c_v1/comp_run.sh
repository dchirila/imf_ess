#!/bin/bash -x
set -e

#rm *.o demo_fort_v1

# Compile
gcc -c test_proc_c_v1.c
gfortran -c demo_fort_v1.f90

# Link
gfortran -o demo_fort_v1 demo_fort_v1.o test_proc_c_v1.o -lc

# Run
./demo_fort_v1
