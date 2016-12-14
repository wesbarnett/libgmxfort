.PHONY: all install clean test
PREFIX  ?= /usr/local
NAME     = libgmxfort
INCLUDE  = ${DESTDIR}${PREFIX}/include
LIBDIR   = ${DESTDIR}${PREFIX}/lib
LICDIR   = ${DESTDIR}${PREFIX}/share/licenses/libgmxfort
SOURCES := $(wildcard src/*.f90)
OBJECTS := $(SOURCES:src/%.f90=%.o)
CFLAGS  += -fPIC -shared  -Wall `pkg-config --cflags libxdrfile`
LDFLAGS += `pkg-config --libs libxdrfile`

${NAME}.so: ${OBJECTS} ${NAME}.pc
	@mkdir -p lib
	@gfortran -o lib/$@ src/*.o ${CFLAGS} ${LDFLAGS}

trajectory.o: indexfile.o common.o

indexfile.o: common.o

%.o: src/%.f90
	@mkdir -p include
	@gfortran -c -o src/$@ $< -Iinclude -Jinclude ${CFLAGS} ${LDFLAGS}

${NAME}.pc:
	@mkdir -p lib/pkgconfig
	@sed 's.MYPREFIX.${PREFIX}.g' src/pkgconfig/$@.in > lib/pkgconfig/$@

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
