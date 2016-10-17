.PHONY: all

all:
	gfortran -c trajectory.f90 -lxdrfile  -std=f2008
	gfortran -c indexfile.f90 -std=f2008
	gfortran -c utils.f90 -std=f2008
	gfortran trajectory.o indexfile.o test.f90 -lxdrfile -std=f2008

