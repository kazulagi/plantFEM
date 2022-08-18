program main
    use SpectreAnalysisClass
    implicit none
    type(SpectreAnalysis_) :: speana,speana_z
    type(SpectreAnalysis_),allocatable :: wave_data(:)

    type(Math_) :: math
    type(Random_) :: random
    integer(int32) :: i,j,n_frame,n_sample
    character(:),allocatable :: filepath
    type(IO_) :: test_wave
    real(real64) :: dt, tn, freq_sample(5)
    integer(int32) :: data_idx

    
    call speana%init(sampling_Hz=1000.0) ! 100 Hz
    ! >>>>>>>>>>>>>>>>>
    ! create data
    freq_sample(1) = 10.0d0
    freq_sample(2) = 20.0d0
    freq_sample(3) = 30.0d0
    freq_sample(4) = 13.0d0
    freq_sample(5) = 10.0d0
    do i=1,5
        call test_wave%open("test_wave" + zfill(i,4) + ".txt" )
        dt = 1.0d0/1000.0d0 ! 100 Hz
        tn = 0.0d0
        do j=1,1024*1000
            tn = tn + dt
            write(test_wave%fh,*) tn, sin( freq_sample(i)/math%PI * tn &
            + random%gauss(mu=0.0d0,sigma=0.10d0) ) !+ sin(freq_sample(2)/math%PI*tn )
        enddo
        call test_wave%close()
    enddo
    ! <<<<<<<<<<<<<<<<


    ! >>>>>>>>>>>>>>>>
    ! read data
    n_frame=1024
    n_sample = 1024*1000
    allocate(wave_data(5) )
    do data_idx = 1, size(wave_data)
        ! 100 Hz sampling
        call wave_data(data_idx)%init(sampling_Hz = 1000.0) 
        ! filepath is
        filepath = "test_wave" + zfill(data_idx,4) + ".txt" 
        ! add wave
        call wave_data(data_idx)%add(filepath,lines=[1,n_sample],BOL=1)
        ! check wave
        !call wave_data(data_idx)%display%plot(&
        !    x=wave_data(data_idx)%channel(1), &
        !    fx =wave_data(data_idx)%channel(2)*taper_function(&
        !        n=size(wave_data(data_idx)%channel(2)&
        !        ),&
        !    percent=0 ),option="with lines")
    enddo

    call speana%FDD(&
        channel = 2, & ! 
        Taper   = 10, & ! 5 %
        Stacking= linspace([0.0,0.0],1), & ! 5 times, from 0 sec, 60 sec, ... 240 sec.
        window_size = n_frame, &
        wave_data=wave_data &
    )
    
    
    call speana%export("FDD")
    
end program main