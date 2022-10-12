use ArrayClass
use IOClass
implicit none

real(real64),allocatable :: t(:),Ft(:),Ft_observation(:)
real(real64) :: tau
type(IO_) :: f

tau = 100.0d0
t  = [0.0d0, 5.0d0, 6.0d0, 7.0d0, 10.0d0, 15.0d0]
Ft = [0.0d0, 1.0d0, 1.0d0,-1.0d0, -3.0d0,  0.0d0]

! Refine data plots!
! N=6 >> N=10000
call refine(  &
        x = t, &
        Fx = Ft, &
        x_range = [0.0d0, 20.0d0],&
        num_point = 10000 &
        )

! raw
call f%plot(t,Ft)
! velocity
call f%plot(t, I_dx(t,Ft) )
! displacement
call f%plot(t, I_dx(t,I_dx(t,Ft)) )

end