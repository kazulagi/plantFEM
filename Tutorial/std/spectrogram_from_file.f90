! hilbert transform


use MathClass
use ArrayClass
use IOClass
implicit none

type(Math_) :: math
type(IO_)   :: f
type(Random_) :: random
complex(complex64),allocatable :: t(:), wave(:), h_top_wave(:)&
    ,h_bottom_wave(:),spectre(:),t_w(:,:)
real(real64),allocatable :: hz(:)
real(real64) :: omega_1
real(real64) :: omega_2

integer(int32),parameter :: window_size = 4096*2


omega_1=0.10d0/math%pi
omega_2=0.3d0/math%pi

!t = linspace([0.0d0,1000.0d0],1024)
!wave = sin(omega_1*t(:)) * 0.30d0*sin(omega_2*t(:) +2.0d0)* exp(-omega_1/10.0d0*t(:) ) +0.40d0*sin(omega_1*20.0d0*t(:)) 
!wave(500:550) = wave(500:550)  +0.80d0*sin(omega_1*80.0d0*t(500:550)) 
!
!do i_i=500,510
!    wave(i_i) = wave(i_i)  + random%gauss(mu=0.0d0,sigma=0.300d0)
!enddo
!wave(400:450) = wave(400:450)*2.0d0
t    = loadtxt("input_file.txt",column=1)
wave = loadtxt("input_file.txt",column=2)
hz   = linspace([0.0d0,100.0d0],window_size)

call f%open("input_wave.txt")
call f%write(abs(t),real(wave) )
call f%close()
call f%plot(option="with lines")


t_w = short_time_FFT(wave(1:size(wave)/2 ),window_size)

call f%open("result.txt")
do i_i=1,size(t_w,1)
    
    if(i_i < window_size/2) cycle

    if(mod(i_i,100)/=0 ) cycle

    do j_j=1,size(t_w,2)/2
        write(f%fh,*) dble(t(i_i)),hz(j_j),abs(t_w(i_i,j_j))
    enddo
    write(f%fh,*) " "
enddo
call f%close()
call f%splot(option="with lines")

end 