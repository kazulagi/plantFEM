use MathClass
use IOClass
implicit none

type(IO_) :: f

real(real64) :: t,dt,sigma

dt = 0.010d0
sigma = 1.0d0

call f%open("Ricker.txt","w")
do i_i = 1,5000
    t   = dble(i_i-1)*dt
    call f%write(t, RickerFunction(t=t,sigma=sigma,center=10.0d0) )
enddo
call f%close()
call f%plot(option="with lines")

end