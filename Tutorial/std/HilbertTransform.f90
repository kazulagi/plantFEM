! hilbert transform


use MathClass
use ArrayClass
use IOClass
implicit none

type(Math_) :: math
type(IO_)   :: f
complex(complex64),allocatable :: t(:), wave(:), h_top_wave(:)&
    ,h_bottom_wave(:),spectre(:)
real(real64) :: omega_1
real(real64) :: omega_2


omega_1=0.10d0/math%pi
omega_2=0.3d0/math%pi

t = linspace([0.0d0,1000.0d0],1024)
wave = sin(omega_1*t(:)) * 0.30d0*sin(omega_2*t(:) +2.0d0)* exp(-omega_1/10.0d0*t(:) ) +0.10d0*sin(omega_1*10.0d0*t(:)) 

call f%open("input_wave.txt")
call f%write(abs(t),real(wave) )
call f%close()
call f%plot(option="with lines")

h_top_wave = hilbert(wave)
h_bottom_wave = hilbert(-wave)

call f%open("ht_wave.txt")
call f%write(abs(t),abs(h_top_wave) )
call f%plot(option="with lines")

end 