.PHONY: all install clean
PREFIX ?= /usr
INCLUDE = ${DESTDIR}${PREFIX}/include
LIBDIR = ${DESTDIR}${PREFIX}/lib

all:
	@mkdir -p include lib
	@gfortran -c src/indexfile.f90 -Jinclude -o src/indexfile.o -std=f2008 -fPIC -Wall
	@gfortran -c src/trajectory.f90 -Jinclude -o src/trajectory.o -lxdrfile  -std=f2008 -fPIC -Wall
	@gfortran -c src/utils.f90 -Jinclude -o src/utils.o -std=f2008 -fPIC -Wall
	@gfortran -o lib/libgmxfort.so src/*.o -shared -lxdrfile -Wall

install:
	@install -Dm644 include/* -t ${INCLUDE}
	@install -Dm755 lib/* -t ${LIBDIR}

clean:
	@rm src/*.o include/*.mod lib/*.so
	@rmdir include lib
