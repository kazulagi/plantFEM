use IOClass
use ArrayClass
implicit none

type(IO_) :: f
type(Math_) :: math
real(real64),allocatable :: t(:),ut(:),vt(:),at(:)
real(real64) :: w

! Frequency
w = 1.0d0

t = linspace([0.0d0,1.0d0], 1024)

vt = 2.0d0*t

ut = I_dx(x=t,fx=vt,f0=0.0d0)
at = d_dx(x=t,fx=vt)

call f%open("u.txt","w")
call f%write(t,ut)
call f%close()
call f%plot(option="with lines")

call f%open("a.txt","w")
call f%write(t,at)
call f%close()
call f%plot(option="with lines")

call f%open("v.txt","w")
call f%write(t,vt)
call f%close()
call f%plot(option="with lines")

end