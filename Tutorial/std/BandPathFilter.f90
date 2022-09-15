use SpectreAnalysisClass
implicit none

type(SpectreAnalysis_) :: speana
real(real64),allocatable :: x(:), x_b(:), t(:)
type(IO_) :: f

real(real64),allocatable :: freq(:), PowerSpectrum(:)
integer(int32) :: n = 100000

call speana%init(&
    sampling_Hz = 100.0   & ! 10 Hz
)

t   = speana%time(n=n)
x   = speana%whiteNoize(n=n)

call f%open("x.txt","w")
call f%write(t,x)
call f%close()
!call f%plot(option="with lines")

x_b = speana%bandpath(x=x,freq_range=[12.0,12.1])

call f%open("x_b.txt","w")
call f%write(t,x_b)
call f%close()
!call f%plot(option="with lines")

call speana%add("x_b.txt",lines=[1,n],BOL=1)
!call speana%add("x.txt",lines=[1,n],BOL=1)

PowerSpectrum = speana%PowerSpectrum(&
    channel = 2, & ! 
    Taper   = 10, & ! 5 %
    Stacking= linspace([0.0,1.0],20), & ! 5 times, from 0 sec, 60 sec, ... 240 sec.
    window_size = 1024*4 &
)
freq = speana%freq_axis(window_size = 1024*4)
call speana%display%plot(x=freq,fx=PowerSpectrum,option="with lines; replot;")

end