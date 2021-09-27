use std
implicit none

type(IO_) :: f
type(Math_) :: math
integer(int32) :: num_sampling = 1300 ! =12 : aliasing, =1000: HQ wave
real(real64),allocatable   :: t(:), Ft(:)

! from t=-100 ~ 100, 1000 sampling
t = linspace([0.0d0,10.0d0],num_sampling)

! create sampling wave
Ft = zeros(num_sampling)
Ft(:) = sin(2.0d0*Math%PI*t) ! sin( 2*PI*t )

! Export as txt file
call f%open("sin_wave_hq.txt","w")
call f%write(t,Ft)
call f%close()

! show
call f%plot("sin_wave_hq.txt","with lines")

end