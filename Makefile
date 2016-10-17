.PHONY: all

all:
	gfortran -c gmx.f90 -lxdrfile  -std=f2008
	gfortran -c ndx.f90 -lxdrfile  -std=f2008
	gfortran ndx.o gmx.o test.f90 -lxdrfile

