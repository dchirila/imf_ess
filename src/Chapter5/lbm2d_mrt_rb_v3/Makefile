# Filename: Makefile
# Author: Dragos B. Chirila
# Purpose: Makefile for the Rayleigh-Benard code in Section 5.3.5.3 of the book
#          (Version 3).
# NOTES: - relative to the Makefile for Version 2, here we 
# NOTES: - relative to the Makefiles discussed in Section 5.1, we introduce here
#          yet another feature -- placing build-results out of the way, into a
#          separate folder ('build_dir' below).

build_dir := build
srcs := lbm2d_mrt_rb_v3.f90 LbmConstantsMrtD2Q5.f90 \
	LbmConstantsMrtD2Q9.f90 MrtSolverBoussinesq2D_class.f90 \
	NumericKinds.f90 OutputAscii_class.f90 \
	OutputBase_class.f90 OutputNetcdf_class.f90 \
	RBenardSimulation_class.f90
objs := $(addprefix $(build_dir)/, $(srcs:.f90=.o))
prog := $(build_dir)/lbm2d_mrt_rb_v3

$(prog): $(objs)
	$(LINK.f) $^ $(LOADLIBES) $(LDLIBS) $(OUTPUT_OPTION)

$(build_dir)/%.o: %.f90
	$(COMPILE.f) $< $(OUTPUT_OPTION)

clean:
	-$(RM) $(build_dir)/*.mod $(build_dir)/*.o $(prog)

.PHONY: clean

# additional dependencies
$(filter-out $(build_dir)/NumericKinds.o, $(objs)): $(build_dir)/NumericKinds.o

$(build_dir)/lbm2d_mrt_rb_v3.o: $(build_dir)/RBenardSimulation_class.o

$(build_dir)/RBenardSimulation_class.o: $(build_dir)/MrtSolverBoussinesq2D_class.o \
	$(build_dir)/OutputAscii_class.o $(build_dir)/OutputNetcdf_class.o

$(build_dir)/MrtSolverBoussinesq2D_class.o: $(build_dir)/LbmConstantsMrtD2Q5.o \
	$(build_dir)/LbmConstantsMrtD2Q9.o

$(build_dir)/OutputAscii_class.o $(build_dir)/OutputNetcdf_class.o: \
	$(build_dir)/OutputBase_class.o

# NOTES: - may have to change the name of the compiler
#        - use 'nc-config --fflags' and 'nc-config --flibs' to obtain the proper
#          values for including the netCDF library on your platform (the paths
#          after '-I' in FFLAGS and after '-L' in LDLIBS may be different)
FC := gfortran-4.8
FFLAGS := -march=native -O3 -fopenmp -std=f2008ts -cpp -pedantic -Wall -J$(build_dir) -I/usr/include
LDLIBS := -L/usr/lib -lnetcdff -lnetcdf
#FC := ifort
#FFLAGS := -O3 -xHost -ipo -fp-model precise -fopenmp -std08 \
		#-diag-enable warn -warn all -nogen-interface \
		#-module $(build_dir) $(shell nf-config --fflags)
#LDLIBS := $(shell nf-config --flibs)
