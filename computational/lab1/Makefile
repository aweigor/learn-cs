FC=gfortran
FFLAGS=-Wall -Wno-tabs -std=f2008ts -fimplicit-none -Wno-maybe-uninitialized -Wno-unused-function -static-libgfortran -flto
FOPT=-O3 -ftree-vectorize -fopt-info-vec

all:
	$(FC) $(FFLAGS) -Wall -Wtabs -c src/forsythe.f90 -J obj/ -o obj/forsythe.o
	$(FC) $(FFLAGS) -c src/environment.f90 -J obj/ -o obj/environment.o
	$(FC) $(FFLAGS) $(FOPT) -c src/matrix_tools.f90 -J obj/ -o obj/matrix_tools.o
	$(FC) $(FFLAGS) $(FOPT) -c src/main.f90 -I obj/ -o obj/main.o
	$(FC) $(FFLAGS) $(FOPT) -o bin/app obj/forsythe.o obj/environment.o obj/main.o obj/matrix_tools.o 

clean:
	rm -rf obj/*
	rm -rf bin/*

run:
	cd ./bin; ./app;
	cat bin/output.txt
