FC=gfortran
CC=gcc

gcc -c cMathClass.c
gfortran -o a.out cMathClass.o fMathClass.f90

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
