use ArrayClass
use IOClass
implicit none

real(real64),allocatable :: t(:),Ft(:)
real(real64) :: tau
type(IO_) :: f

tau = 100.0d0
t = linspace([0.0d0,2.0d0*tau],10000)
Ft = t

! accel.
Ft(1   :5000) = 1.0d0
Ft(5001:6000)  = -10.0d0
Ft(6001:7000)  = 1.0d0
Ft(7001:8000)  = -3.0d0
Ft(8001:9000)  = 10.0d0
Ft(9001:10000) = -1.0d0

call f%plot(t,Ft)
! velocity
call f%plot(t, I_dx(t,Ft) )
! displacement
call f%plot(t, I_dx(t,I_dx(t,Ft)) )

end