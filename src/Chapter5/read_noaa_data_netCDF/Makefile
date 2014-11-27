# Filename: Makefile
# Author: Dragos B. Chirila
# Purpose: Makefile for the read_noaa_woa_2009_data.f90 program, in Section
#          5.2.2 of the book.

build_dir := build
srcs := read_noaa_woa_2009_data.f90 OceanData_class.f90 GeomUtils.f90 NumericKinds.f90
objs := $(addprefix $(build_dir)/, $(srcs:.f90=.o))
prog := $(build_dir)/read_noaa_woa_2009_data

$(prog): $(objs)
	$(LINK.f) $^ $(LOADLIBES) $(LDLIBS) $(OUTPUT_OPTION)

$(build_dir)/%.o: %.f90
	$(COMPILE.f) $< $(OUTPUT_OPTION)

clean:
	-$(RM) *.mod $(build_dir)/*.mod $(build_dir)/*.o $(prog)

.PHONY: clean

# additional dependencies
$(filter-out $(build_dir)/NumericKinds.o, $(objs)): $(build_dir)/NumericKinds.o
$(build_dir)/read_noaa_woa_2009_data.o: $(build_dir)/OceanData_class.o
$(build_dir)/OceanData_class.o: $(build_dir)/GeomUtils.o

# NOTES: - may have to change the name of the compiler
#        - use 'nc-config --fflags' and 'nc-config --flibs' to obtain the proper
#          values for including the netCDF library on your platform (the paths
#          after '-I' in FFLAGS and after '-L' in LDLIBS may be different)
FC := gfortran-4.8
FFLAGS := -march=native -O3 -std=f2008ts -pedantic -Wall -J$(build_dir) -I/usr/include
LDLIBS := -L/usr/lib -lnetcdff -lnetcdf
