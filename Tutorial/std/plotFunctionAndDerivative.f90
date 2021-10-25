use ArrayClass
use IOClass
implicit none

real(real64),allocatable :: t(:),Ft(:)
real(real64) :: tau
type(IO_) :: f

tau = 100.0d0
t = linspace([0.0d0,2.0d0*tau],10000)
Ft = t

Ft(1   : 5000) = 0.50d0*t(1:5000)*t(1:5000)/tau/tau
Ft(5001:10000) = - 0.50d0*t(5001:)*t(5001:)/tau/tau +2.0d0*t(5001:)/tau-1

call f%plot(t,Ft)
! 1st Derivative
call f%plot(t, d_dx(t,Ft) )
! 2nd Derivative
call f%plot(t, d_dx(t,d_dx(t,Ft)) )

end