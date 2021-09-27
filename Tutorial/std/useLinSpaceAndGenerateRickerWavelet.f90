use std
implicit none

type(IO_) :: f
integer(int32) :: num_sampling = 10000
real(real64),allocatable   :: t(:), Ft(:)

! from t=-100 ~ 100, 1000 sampling
t = linspace([0.0d0,4.0d0],num_sampling)

! create sampling wave
Ft = zeros(num_sampling)
Ft(:) = (1 - t*t)*exp(-t*t*0.50d0 )

call f%open("Ricker_wavelet.txt","w")
call f%write(t,Ft)
call f%close()

call f%plot("Ricker_wavelet.txt")

end