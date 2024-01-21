!FC=gfortran
FFLAGS=-Wall -std=f2008ts -fimplicit-none -Wno-maybe-uninitialized -Wno-unused-function -static-libgfortran -flto
FOPT=-O3 -ftree-vectorize -fopt-info-vec

all:
	$(FC) $(FFLAGS) -c environment.f90 -o environment.o
	$(FC) $(FFLAGS) $(FOPT) -c main.f90 -o obj/main.o
	$(FC) $(FFLAGS) $(FOPT) -o app environment.o main.o
# 	$(FC) $(FFLAGS) $(FOPT) -o app main.o

#	$(FC) -c src/environment.f90 -J obj/ -o obj/environment.o
#	$(FC) -O2 -c src/main.f90 -I obj/ -o obj/main.o
#	$(FC) -O2 -o bin/app ./obj/environment.o obj/main.o

clean:
	rm -rf obj/*
	rm -rf bin/*

run:
	cd ./bin; ./app;
	cat bin/output.txt
