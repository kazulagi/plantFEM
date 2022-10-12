use LinearSolverClass
implicit none

type(COO_) :: A
type(CRS_) :: A_
real(real64),allocatable :: x(:),b(:)
integer(int32) :: itrmax
real(real64) :: er, relative_er

call A%init(3)
call A%update(1,1, 5.0d0)
call A%update(1,2, 6.0d0)
call A%update(1,3, 7.0d0)
call A%update(2,1, 10.0d0)
call A%update(2,2, 20.0d0)
call A%update(2,3, 23.0d0)
call A%update(3,1, 15.0d0)
call A%update(3,2, 50.0d0)
call A%update(3,3, 67.0d0)

A_ = A%to_CRS()
x = zeros(3)
b = [2.0d0,3.0d0,4.0d0]
itrmax = 10000
er = 1.0e-13
relative_er = 1.0e-13
! ILU-PBCGSTAB
call PBiCGSTAB_CRS(CRS=A_,x=x,b=b,itrmax=itrmax, er=er, relative_er=relative_er,debug=.true.)
print *, "x=",x
print *," should be"
print *, [0.540d0,-0.350d0,0.20d0]

end