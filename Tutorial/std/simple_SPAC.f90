program main
    use SPACClass
    implicit none

    real(real64),allocatable :: Center_x(:), Circle_x(:,:),Angle(:)
    real(real64),allocatable :: t(:),freq(:),SPAC_COEFF(:),phase_velocity(:),HoverV_spectra(:)
    real(real64) :: radius
    integer(int32) :: FFT_SIZE,NUM_SAMPLE,sampling_Hz,num_logger
    type(IO_) :: f


    ! Array * .txt are observation data.
    ! please change the name when you use this script.

    ! >>>>>>>> INPUT DATA INFO >>>>>>>>>>>
    NUM_SAMPLE = f%numLine("Array_A_0037_dim_3.txt")-1
    sampling_Hz = 50
    radius      = 3.0d0
    num_logger  = 36
    t= to_time_axis(sampling_Hz,NUM_SAMPLE)

    Circle_x = zeros(NUM_SAMPLE,num_logger)
    do i_i=1,num_logger
        Circle_x(:,i_i) = from_CSV("Array_A_"+zfill(i_i,4)+"_dim_3.txt",column=2)
    enddo

    Center_x = from_CSV("Array_A_0037_dim_3.txt",column=2)
    ! >>>>>>>> INPUT DATA INFO >>>>>>>>>>>


    FFT_SIZE = 1024*2*2

    freq =  to_frequency_axis(FFT_SIZE=FFT_SIZE,sampling_Hz=sampling_Hz)

    ! >>>>>>>> H/V spectra >>>>>>>>
    HoverV_spectra = to_HoverV_spectra(&
        H        =from_CSV("Array_A_0037_dim_1.txt",column=2),&
        V        =from_CSV("Array_A_0037_dim_3.txt",column=2),&
        FFT_SIZE        =FFT_SIZE)

    do i_i=1,100
        HoverV_spectra = moving_average(HoverV_spectra)
    enddo
    call f%open("HoverV-spectra.txt","w")
    call f%write(freq,HoverV_spectra)
    call f%close()
    call f%plot(option="with lines")
    ! >>>>>>>> H/V spectra >>>>>>>>



    
    ! >>>>>>>> SPAC coefficient >>>>>>>> 
    SPAC_COEFF = to_SPAC_COEFF(&
        Center_x=Center_x,&
        Circle_x=Circle_x,&
        FFT_SIZE=FFT_SIZE)
    
    ! smoothing
    do i_i=1,100
        SPAC_COEFF = moving_average(SPAC_COEFF)
    enddo
    
    call f%open("SPAC_COEFF.txt","w")
    call f%write(freq,SPAC_COEFF)
    call f%close()
    call f%plot(option="with lines")
    ! >>>>>>>> SPAC coefficient >>>>>>>> 



    

    ! >>>>>>>> Rayleigh wave dispersion curve >>>>>>>> 
    phase_velocity = to_phase_velocity(&
        Center_x        =Center_x,&
        Circle_x        =Circle_x,&
        FFT_SIZE        =FFT_SIZE,&
        radius          =radius,&
        sampling_Hz     =sampling_Hz,&
        learning_rate   =dble(1.0e-2), &
        tolerance       =dble(1.0e-5), &
        max_iter        =2000, &
        initial_phase_velocity =70.0d0 ,&
        debug           =.true. )
        
    ! smoothing
    do i_i=1,100
        phase_velocity = moving_average(phase_velocity)
    enddo
    call f%open("Rayl-Dispersion.txt","w")
    call f%write(freq,phase_velocity)
    call f%close()
    call f%plot(option="with lines")
    ! >>>>>>>> Rayleigh wave dispersion curve >>>>>>>> 

end program

