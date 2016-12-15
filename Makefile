.PHONY: all install clean test
PREFIX  ?= /usr/local
NAME     = libgmxfort
PREREQ   = libxdrfile
INCLUDE  = ${DESTDIR}${PREFIX}/include
LIBDIR   = ${DESTDIR}${PREFIX}/lib
LICDIR   = ${DESTDIR}${PREFIX}/share/licenses/libgmxfort
SOURCES := $(wildcard src/*.f90)
OBJECTS := $(SOURCES:src/%.f90=%.o)
CFLAGS  += `pkg-config --cflags ${PREREQ}` -Wall
LDFLAGS += `pkg-config --libs ${PREREQ}`
VERSION := $(shell git describe --tags)

${NAME}.so: ${OBJECTS} ${NAME}.pc
	@pkg-config --exists ${PREREQ} --print-errors
	@mkdir -p lib
	@gfortran -o lib/$@ src/*.o ${CFLAGS} ${LDFLAGS}

trajectory.o: indexfile.o common.o

indexfile.o: common.o

%.o: src/%.f90
	@mkdir -p include
	@gfortran -c -o src/$@ $< -Iinclude -Jinclude ${CFLAGS} ${LDFLAGS}

${NAME}.pc:
	@mkdir -p lib/pkgconfig
	@sed -e 's.MYPREFIX.${PREFIX}.g' -e 'sxMYVERSIONx${VERSION}xg' src/pkgconfig/$@.in > lib/pkgconfig/$@

test: ${NAME}.so
	@mkdir -p tests
	@gfortran -o tests/$@ src/tests/test.f90 lib/$< -Iinclude -Jtests 
	@./tests/test

install: ${NAME}.so ${NAME}.pc
	@install -Dm644 include/* -t ${INCLUDE}
	@install -Dm755 lib/*.so -t ${LIBDIR}
	@install -Dm755 lib/pkgconfig/* -t ${LIBDIR}/pkgconfig
	@install -Dm644 LICENSE  -t ${LICDIR}

clean:
	@rm -fr src/*.o include lib tests/*.mod tests *.mod
