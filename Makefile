.PHONY: all

all:
	gfortran -c gmxfort.f90 -lxdrfile  -std=f2008
	gfortran gmxfort.o test.f90 -lxdrfile

