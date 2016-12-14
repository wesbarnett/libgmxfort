.PHONY: all install clean test
PREFIX ?= /usr
INCLUDE = ${DESTDIR}${PREFIX}/include
LIBDIR = ${DESTDIR}${PREFIX}/lib
LICDIR = ${DESTDIR}${PREFIX}/share/licenses/libgmxfort

libgmxfort.so: common.o indexfile.o trajectory.o utils.o
	@mkdir -p lib 
	@gfortran -o lib/$@ src/*.o -std=gnu -fPIC -shared -lxdrfile -Wall

%.o: src/%.f90
	@mkdir -p include
	@gfortran -c -o src/$@ $< -Jinclude -std=gnu -fPIC -shared -lxdrfile -Wall

test: libgmxfort.so
	@gfortran tests/test.f90 -o tests/$@ -Iinclude -Jtests lib/libgmxfort.so
	@./tests/test

install: libgmxfort.so
	@install -Dm644 include/* -t ${INCLUDE}
	@install -Dm755 lib/* -t ${LIBDIR}
	@install -Dm644 LICENSE  -t ${LICDIR}

clean:
	@rm -f src/*.o include/*.mod lib/*.so tests/*.mod tests/test
	@rmdir lib include
