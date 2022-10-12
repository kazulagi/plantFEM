use std
implicit none

type(IO_) :: f
integer(int32) :: num_sampling = 10000
real(real64),allocatable   :: t(:), Ft(:)
real(real64) :: omega ! frequency

omega = 100.0d0 ! 100 Hz
! from t=-100 ~ 100, 1000 sampling
t = linspace([-1.0d0,1.0d0],num_sampling)

! create sampling wave
Ft = zeros(num_sampling)
Ft(:) = sin( omega * t(:) )/(omega*t(:) )

call f%open("sampling_wave.txt","w")
call f%write(t,Ft)
call f%close()

end