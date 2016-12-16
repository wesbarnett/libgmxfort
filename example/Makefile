.PHONY: all clean

all:
	@gfortran main.f90 -I/usr/include `pkg-config --libs libgmxfort` -Wall

clean:
	@rm a.out
