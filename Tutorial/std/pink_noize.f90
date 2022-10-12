use RandomClass
use IOClass
use TimeClass
use ArrayClass
implicit none

type(Random_) :: random
type(IO_)     :: f
type(Time_)   :: time
real(real64),allocatable :: t(:), w(:)
complex(real64),allocatable :: f_t(:), F_w(:)


! white noise
t   = time%t([0.0d0,3600.0d0],Hz=200.0d0, max_sample=1024)
w   = time%freq(t)
call print(w)
f_t = random%white(num_sample=size(t), mu=0.0d0, sigma=1.0d0 )
F_w = FFT(f_t)

call f%plot(x=t, fx=dble(f_t),option="with lines")
call f%plot(x=w, fx=abs(f_w(1:size(w) ) ),option="with lines; set logscale; replot")
f_w(1:size(w)) = f_w(1:size(w))*1.0d0/w(1:size(w) )
f_w(size(w)+1:2*size(w)) = f_w(size(w)+1:2*size(w))*1.0d0/w(1:size(w) )
f_t = IFFT(f_w)
call f%plot(x=w, fx=abs(f_w(1:size(w) ) ),option="with lines; set logscale; replot")
call f%plot(x=t, fx=dble(f_t),option="with lines")


end