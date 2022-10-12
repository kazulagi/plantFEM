program main
    use SpectreAnalysisClass
    implicit none
    type(SpectreAnalysis_) :: speana
    type(IO_) :: f
    real(real64),allocatable :: t(:),x(:),y(:),z(:)

    type(Math_) :: math
    type(Random_) :: random
    complex(complex64),allocatable :: FourierSpectrum(:)
    real(real64),allocatable :: freq(:), PowerSpectrum(:)
    integer(int32) :: i,j,k
    real(real64) :: time, w
    character(:),allocatable :: filepath

    call speana%init(&
            sampling_Hz = 10.0   & ! 10 Hz
        )

        ! add data
        ! lines from 1 to 600 (if EOF, fill zero)
        ! for each line, read [BOL to EOL]

![Sample data] (10 Hz sampling, w = 0.5 Hz) with white noise
!call f%open("Tutorial/std/sample_data_xyz.txt")
!do i_i=1,1000000
!    w = 2.0d0*math%pi*2.0d0 ! 2.0 Hz
!    time = (i_i-1)/speana%sampling_Hz
!    write(f%fh,*) sin(w*time+random%gauss(mu=0.0d0,sigma=1.0d0) ),&
!        cos(w*time+random%gauss(mu=0.0d0,sigma=1.00d0) ),& 
!        random%gauss(mu=1.0d0,sigma=1.0d0)*sin(w*time )
!enddo
!call f%close()

    filepath = "Tutorial/std/sample_data_xyz.txt"
    call speana%add(filepath,lines=[1,44000],BOL=1)
        
    
    t = speana%channel(0)
    x = speana%channel(1)
    y = speana%channel(2)
    z = speana%channel(3)

    x = x -  average(x) 
    y = y -  average(y) 
    z = z -  average(z) 
    
    ! check wave
    call speana%display%plot(x=t/60.0d0/60.0d0,fx=z*taper_function(n=size(t),&
        percent=5 ),option="with lines")
    
    ! FFT
    !FourierSpectrum = speana%FFT(&        
    !        channel = 1, & ! 
    !        Taper   = 10, & ! 10 %
    !        Stacking= [0.0],!, 30.0, 60.0, 120.0, 150.0, 180.0], & ! 5 times, from 0 sec, 60 sec, ... 240 sec.
    !        window_size = 1024 &
    !    )
    PowerSpectrum = speana%PowerSpectrum(&        
        channel = 3, & ! 
        Taper   = 10, & ! 5 %
        Stacking= linspace([0.0,400.0],20), & ! 5 times, from 0 sec, 60 sec, ... 240 sec.
        window_size = 1024*4 &
    )
    freq = speana%freq_axis(window_size = 1024*4)

    
    call speana%display%plot(x=freq,fx=PowerSpectrum,option="with lines; set logscale; replot;")
    
    
end program main