.PHONY: all install clean test
PREFIX ?= /usr
INCLUDE = ${DESTDIR}${PREFIX}/include
LIBDIR = ${DESTDIR}${PREFIX}/lib
LICDIR = ${DESTDIR}${PREFIX}/share/licenses/libgmxfort

all:
	@mkdir -p include lib
	@gfortran -c src/indexfile.f90 -Jinclude -o src/indexfile.o -std=f2008 -fPIC -Wall
	@gfortran -c src/trajectory.f90 -Jinclude -o src/trajectory.o -lxdrfile  -std=f2008 -fPIC -Wall
	@gfortran -c src/utils.f90 -Jinclude -o src/utils.o -std=f2008 -fPIC -Wall
	@gfortran -o lib/libgmxfort.so src/*.o -shared -lxdrfile -Wall

test:
	@gfortran tests/test.f90 -o tests/test -Iinclude -Jtests lib/libgmxfort.so
	@./tests/test

install:
	@install -Dm644 include/* -t ${INCLUDE}
	@install -Dm755 lib/* -t ${LIBDIR}
	@install -Dm644 LICENSE  -t ${LICDIR}

clean:
	@rm src/*.o include/*.mod lib/*.so tests/*.mod tests/test
	@rmdir include lib
