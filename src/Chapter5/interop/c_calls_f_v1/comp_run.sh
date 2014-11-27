#!/bin/bash -x
set -e

#rm *.o demo_c_v1

# Compile
gfortran -c test_proc_fort_v1.f90
gcc -c demo_c_v1.c

# Link
gcc -o demo_c_v1 demo_c_v1.o test_proc_fort_v1.o -lgfortran

# Run
./demo_c_v1
