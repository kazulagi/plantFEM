FC=gfortran
CC=gcc

gcc -c cMathClass.c
gcc -c cOpenCLClass.c -lOpenCL
gfortran -o a.out cOpenCLClass.o cMathClass.o fMathClass.f90 -lOpenCL

#IBM Blue Gene Q
#FC=bgxlf90_r
#CC=bgxlc_r

#IBM Power
#FC=xlf90_r
#CC=xlc_r

#intel
#FC=ifort
#CC=icc

#Portland Group both x86 and power
#FC=pgf90
#CC=pgcc


#mymkdir: dodir.f90 mymkdir.o
#	rm -f cfuncs.mod
#	$(FC) dodir.f90 mymkdir.o -o mymkdir
#
#mymkdir.o: mymkdir.c
#	$(CC) -c mymkdir.c
#
#clean:
#	rm -f mymkdir.o mymkdir cfuncs.mod dodir.o
#
#tar:
#	tar -cf mkdir.tar dodir.f90 mymkdir.c index.html $(MAKEFILE_LIST)
