.PHONY: all install clean
PREFIX ?= /usr
INCLUDE = ${PREFIX}/include
LIBDIR = ${PREFIX}/lib

all:
	gfortran -c src/indexfile.f90 -std=f2008 -fPIC
	gfortran -c src/trajectory.f90 -lxdrfile  -std=f2008 -fPIC
	gfortran -c src/utils.f90 -std=f2008 -fPIC
	gfortran -o libgmxfort.so *.o -shared -lxdrfile

install:
	@install -Dm644 *.mod -t ${INCLUDE}
	@install -Dm755 *.so -t ${LIBDIR}

clean:
	rm *.o *.mod *.so *.out
