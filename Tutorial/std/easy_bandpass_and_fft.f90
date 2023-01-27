program main
    use SpectreAnalysisClass
    implicit none
    complex(real64),allocatable :: spectrum(:,:)
    real(real64) :: t, dt
    type(IO_) :: f
    type(Random_) :: random
    type(SpectreAnalysis_) :: test_speana
    

    ! test bandpass
    t = 0.0d0
    dt = 0.010d0
    call f%open("raw_signal.txt","w")
    do i_i=1,100000
        write(f%fh,*) t, random%random()
        t = t + dt
    enddo
    call f%close()
    
    call test_speana%init(sampling_Hz=100.0)

    t = 0.0d0
    call f%open("raw_signal_bandpass.txt","w")
    do i_i=1,100000
        write(f%fh,*) t, test_speana%bandpass(x=random%random(),freq_range=[5.0,6.0])
        t = t + dt
    enddo
    call f%close()
    spectrum = FFT(infile="raw_signal.txt",&
        outfile="raw_signal.FFT",&
        window_size=4096,&
        dt=dt,column=2,as_abs=.true.)
    spectrum = FFT(infile="raw_signal_bandpass.txt",&
        outfile="raw_signal_bandpass.FFT",&
        window_size=4096,&
        dt=dt,column=2,as_abs=.true.)
    
end program main