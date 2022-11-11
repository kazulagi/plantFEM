program main
    use SPACClass
    implicit none

    real(real64),allocatable :: Center_x(:), Circle_x(:,:),Angle(:)
    real(real64),allocatable :: t(:),freq(:),SPAC_COEFF(:),phase_velocity(:),HoverV_spectra(:)
    real(real64) :: radius,Maximum_phase_velocity
    integer(int32) :: FFT_SIZE,NUM_SAMPLE,sampling_Hz,num_logger,Maximum_itr,&
        num_smoothing
    type(IO_) :: f
    character(50) :: fpath
    character(:),allocatable :: filepath,config
    


    ! Array * .txt are observation data.
    ! please change the name when you use this script.
    call getarg(1,fpath)
    filepath = trim(adjustl(fpath))
    
    ! >>>>>>>> INPUT DATA INFO >>>>>>>>>>>
    config = filepath+".condition.json"
    sampling_Hz = fint(f%parse(config,key1="sampling_hz")) != 250
    radius      =freal(f%parse(config,key1="radius")) != 4.0d0
    num_logger  = fint(f%parse(config,key1="num_logger")) != 4
    fft_size    = fint(f%parse(config,key1="fft_size")) != 1024*2*2
    Maximum_phase_velocity    =freal(f%parse(config,key1="maximum_phase_velocity")) != 5000.0
    Maximum_itr    = fint(f%parse(config,key1="maximum_itr")) != 100000
    num_smoothing    = fint(f%parse(config,key1="num_smoothing")) != 10

    
    NUM_SAMPLE = f%numLine(filepath)
    t= to_time_axis(sampling_Hz,NUM_SAMPLE)
    
    Circle_x = zeros(NUM_SAMPLE,num_logger-1)
    Center_x = zeros(NUM_SAMPLE)
    
    Center_x = from_CSV(filepath,column=2)
    ! 1,3
    do i_i=1,num_logger-1
        Circle_x(:,i_i) = from_CSV(filepath,column=1+(i_i)*3+1 )
    enddo

    ! >>>>>>>> INPUT DATA INFO >>>>>>>>>>>


    

    freq =  to_frequency_axis(FFT_SIZE=FFT_SIZE,sampling_Hz=sampling_Hz)

    ! >>>>>>>> H/V spectra >>>>>>>>
    HoverV_spectra = to_HoverV_spectra(&
        H        =from_CSV(filepath,column=1+2),&
        V        =from_CSV(filepath,column=1+1),&
        FFT_SIZE        =FFT_SIZE)

    do i_i=1,10
        HoverV_spectra = moving_average(HoverV_spectra)
    enddo
    call f%open(filepath+"_HoverV-spectra_EW.csv","w")
    do i_i=1,size(freq)
        write(f%fh,*) freq(i_i),",",HoverV_spectra(i_i)
    enddo
    call f%close()
    !call f%plot(option="with lines")
    ! >>>>>>>> H/V spectra >>>>>>>>


    ! >>>>>>>> H/V spectra >>>>>>>>
    HoverV_spectra = to_HoverV_spectra(&
        H        =from_CSV(filepath,column=1+3),&
        V        =from_CSV(filepath,column=1+1),&
        FFT_SIZE        =FFT_SIZE)

    do i_i=1,10
        HoverV_spectra = moving_average(HoverV_spectra)
    enddo
    call f%open(filepath+"_HoverV-spectra_NS.csv","w")
    do i_i=1,size(freq)
        write(f%fh,*) freq(i_i),",",HoverV_spectra(i_i)
    enddo
    call f%close()
    !call f%plot(option="with lines")
    ! >>>>>>>> H/V spectra >>>>>>>>


    ! >>>>>>>> SPAC coefficient >>>>>>>> 
    SPAC_COEFF = to_SPAC_COEFF(&
        Center_x=Center_x,&
        Circle_x=Circle_x,&
        FFT_SIZE=FFT_SIZE)
    
    ! smoothing
    do i_i=1,num_smoothing
        SPAC_COEFF = moving_average(SPAC_COEFF)
    enddo
    
    call f%open(filepath+"_SPAC_COEFF.csv","w")
    do i_i=1,size(freq)
        write(f%fh,*)freq(i_i),",",SPAC_COEFF(i_i)
    enddo
    call f%close()
    !call f%plot(option="with lines")
    ! >>>>>>>> SPAC coefficient >>>>>>>> 



    

    ! >>>>>>>> Rayleigh wave dispersion curve >>>>>>>> 
    phase_velocity = to_phase_velocity(&
        Center_x        =Center_x,&
        Circle_x        =Circle_x,&
        FFT_SIZE        =FFT_SIZE,&
        radius          =radius,&
        sampling_Hz     =sampling_Hz,&
        max_c           = Maximum_phase_velocity, & ! m/s
        max_itr         = Maximum_itr, &
        debug           =.true. )
        
    ! smoothing
    do i_i=1,num_smoothing
        phase_velocity = moving_average(phase_velocity)
    enddo
    call f%open(filepath+"_Rayl-Dispersion.csv","w")
    do i_i=1,size(freq)
        write(f%fh,*)freq(i_i),",",phase_velocity(i_i) 
    enddo
    call f%close()
    !call f%plot(option="with lines")
    ! >>>>>>>> Rayleigh wave dispersion curve >>>>>>>> 

end program

