
program main
use plantfem
implicit none

real(real64) :: in_freq ! in_frequency
real(real64),allocatable :: t(:),freq(:),wave(:)
complex(real64),allocatable :: spec(:)
type(Math_) :: math
type(IO_) :: f

in_freq = 10.0d0 ! 10 Hz
t = linspace([0.0d0,10.0d0],10*1000)

wave = cos(2*math%pi*in_freq*t)

wave(100) = 100.0d0
wave(200) = -100.0d0
wave(300) = 100.0d0

call f%open("wave.txt","w")
call f%write(t .h. wave)
call f%close()

spec = FFT(  to_complex(wave(1:2**12) )  )
freq = linspace([0.0d0,1.0d0/(t(2)-t(1) )/2.0d0],size(spec))

call f%open("spec.txt","w")
call f%write(freq .h. abs(spec(1:size(spec)/2) ))
call f%close()

wave = median_filter(wave,window_size=10)
call f%open("wave_median.txt","w")
call f%write(t .h. wave)
call f%close()
spec = FFT(  to_complex(wave(1:2**12) )  )
freq = linspace([0.0d0,1.0d0/(t(2)-t(1) )/2.0d0],size(spec))
call f%open("spec_median.txt","w")
call f%write(freq .h. abs(spec(1:size(spec)/2 ) ))
call f%close()

end program 