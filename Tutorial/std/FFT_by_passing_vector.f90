use std
implicit none

real(real64),allocatable :: t(:),a_Logger_1(:),a_Logger_2(:),freq(:)
complex(complex64),allocatable :: FourierSpectrum_Logger_1(:)
complex(complex64),allocatable :: FourierSpectrum_Logger_2(:)
type(Random_) :: random
type(IO_) :: f
type(SpectreAnalysis_) :: speana

t = linspace([0.0,6*60*100.0],6*60*100) ! 100Hz * 1 hour
a_Logger_1 = random%gauss(mu=0.0d0,sigma=1.0d0,n=size(t) )
a_Logger_2 = random%gauss(mu=0.0d0,sigma=1.0d0,n=size(t) )

call f%open("../data/sample_created.csv","w")
call f%write("symbol,time,accel")
call f%write("Logger_1",t,a_Logger_1)
call f%write("Logger_2",t,a_Logger_2+5.0d0)
call f%close()


!! FFT

call speana%init(&
    sampling_Hz = 100.0   & ! 100 Hz
)

FourierSpectrum_Logger_1 = speana%FFT(&        
    vector  = a_Logger_1 ,& ! 
    Taper   = 10, & ! 10 %
    Stacking= linspace([0.0,100.0],20), & ! 20 times, from 0 sec, 400 sec.
    window_size = 1024 &
)

FourierSpectrum_Logger_2 = speana%FFT(&        
    vector  = a_Logger_2 ,& ! 
    Taper   = 10, & ! 10 %
    Stacking= linspace([0.0,100.0],20), & ! 20 times, from 0 sec, 400 sec.
    window_size = 1024 &
)
freq = speana%freq_axis(window_size=1024)

call f%open("../data/sample_created_FFT.csv","w")
call f%write("symbol,freq,accel")
call f%write("Logger_1",freq,dble(FourierSpectrum_Logger_1))
call f%write("Logger_2",freq,dble(FourierSpectrum_Logger_2))
call f%close()





end
