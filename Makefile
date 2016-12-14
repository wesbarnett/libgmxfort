.PHONY: all install clean test
PREFIX  ?= /usr
NAME     = libgmxfort
INCLUDE  = ${DESTDIR}${PREFIX}/include
LIBDIR   = ${DESTDIR}${PREFIX}/lib
LICDIR   = ${DESTDIR}${PREFIX}/share/licenses/libgmxfort
SOURCES := $(wildcard src/*.f90)
OBJECTS := $(SOURCES:src/%.f90=%.o)
CFLAGS  += -fPIC -shared  -Wall
LDFLAGS += -lxdrfile

libgmxfort.so: common.o indexfile.o trajectory.o utils.o
	@mkdir -p lib/pkgconfig
	@sed 's.MYPREFIX.${PREFIX}.g' src/pkgconfig/libgmxfort.pc.in > lib/pkgconfig/libgmxfort.pc
	@gfortran -o lib/$@ src/*.o ${CFLAGS} ${LDFLAGS}

%.o: src/%.f90
	@mkdir -p include
	@gfortran -c -o src/$@ $< -Jinclude ${CFLAGS} ${LDFLAGS}

test: ${NAME}.so
	mkdir -p tests
	@gfortran -o tests/$@ src/tests/test.f90 lib/$< -Iinclude -Jtests 
	@./tests/test

install: ${NAME}.so
	@install -Dm644 include/* -t ${INCLUDE}
	@install -Dm755 lib/*.so -t ${LIBDIR}
	@install -Dm755 lib/pkgconfig/* -t ${LIBDIR}/pkgconfig
	@install -Dm644 LICENSE  -t ${LICDIR}

clean:
	@rm -fr src/*.o include lib tests/*.mod tests *.mod
