use ArrayClass
use IOClass
implicit none

real(real32),allocatable :: t(:),Ft(:),Ft_observation(:)
real(real32) :: tau
type(IO_) :: f

tau = 100.0
t  = [0.0, 5.0, 6.0, 7.0, 10.0, 15.0]
Ft = [0.0, 1.0, 1.0,-1.0, -3.0,  0.0]

! Refine data plots!
! N=6 >> N=10000
call refine(  &
        x = t, &
        Fx = Ft, &
        x_range = [0.0, 20.0],&
        num_point = 10000 &
        )

! raw
call f%plot(t,Ft)
! velocity
call f%plot(t, I_dx(t,Ft) )
! displacement
call f%plot(t, I_dx(t,I_dx(t,Ft)) )

end