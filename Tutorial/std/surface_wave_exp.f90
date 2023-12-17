program main
    use plantfem
    implicit none
    
    type(IO_) :: f

    real(real64),allocatable :: t(:),freq(:), template_wave(:)
    real(real64),allocatable :: waves(:,:),positions(:),c_axis(:),I_matrix(:,:),&
        wave1(:,:),wave2(:,:),wave3(:,:),wave4(:,:),noize(:,:)
    real(real64) :: dt, true_c,start_time,true_freq
    integer(int32) :: num_data,num_logger, logger_idx,timestep,itr,fft_size
    integer(int32) :: i,j
    type(Math_) :: math
    type(Random_) :: random
    ! create synthetic data
    ! template_wave(:)
    num_data = 1000*60 
    num_logger = 50
    dt = 1.0d0/1000.0d0
    t = linspace([0.0d0,dble(num_data)*dt],num_data) ! 1kHz 60 sec
    positions = linspace([0.0d0,50.0d0],num_logger) ! 1m interval, 30 points

    true_freq = 30.0d0 ! Hz
    true_c = 150.0d0 !m/s
    template_wave = exp(-2*math%pi*0.10d0*true_freq*t)*cos(2*math%pi*true_freq*t)
    wave1 = zeros(num_data,num_logger)
    do logger_idx=1,size(positions)
        start_time = positions(logger_idx)/true_c
        itr = 0
        do timestep = 1,size(wave1,1)
            if(t(timestep) >= start_time )then
                itr = itr + 1
                wave1(timestep,logger_idx) = template_wave(itr)
            endif
        enddo
    enddo

    true_freq = 20.0d0 ! Hz
    true_c = 180.0d0 !m/s
    template_wave = exp(-2*math%pi*0.050d0*true_freq*t)*cos(2*math%pi*true_freq*t)
    wave2 = zeros(num_data,num_logger)
    do logger_idx=1,size(positions)
        start_time = positions(logger_idx)/true_c
        itr = 0
        do timestep = 1,size(wave2,1)
            if(t(timestep) >= start_time )then
                itr = itr + 1
                wave2(timestep,logger_idx) = template_wave(itr)
            endif
        enddo
    enddo
    

    true_freq = 10.0d0 ! Hz
    true_c = 300.0d0 !m/s
    template_wave = exp(-2*math%pi*0.050d0*true_freq*t)*cos(2*math%pi*true_freq*t)
    wave3 = zeros(num_data,num_logger)
    do logger_idx=1,size(positions)
        start_time = positions(logger_idx)/true_c
        itr = 0
        do timestep = 1,size(wave3,1)
            if(t(timestep) >= start_time )then
                itr = itr + 1
                wave3(timestep,logger_idx) = template_wave(itr)
            endif
        enddo
    enddo

    true_freq = 5.0d0 ! Hz
    true_c = 500.0d0 !m/s
    template_wave = exp(-2*math%pi*0.050d0*true_freq*t)*cos(2*math%pi*true_freq*t)
    wave4 = zeros(num_data,num_logger)
    do logger_idx=1,size(positions)
        start_time = positions(logger_idx)/true_c
        itr = 0
        do timestep = 1,size(wave4,1)
            if(t(timestep) >= start_time )then
                itr = itr + 1
                wave4(timestep,logger_idx) = template_wave(itr)
            endif
        enddo
    enddo

    noize = zeros(num_data,num_logger)
    do timestep = 1,size(wave4,1)
        do logger_idx=1,size(positions)
            noize(timestep,logger_idx) = random%gauss(mu=0.0d0,sigma=0.10d0)
        enddo
    enddo

    waves = wave1 + wave2 + wave3 + wave4 + noize

    call f%open("template_wave.txt","w")    
    call f%write(t .h.  template_wave )     
    call f%close()


    call f%open("waves.txt","w")
    call f%write(t .h. add_offsets(waves,col_interval=1.0d0))
    call f%close()

    ! inversion
    c_axis = linspace([0.0d0,1000.0d0],1000)

    fft_size = 8192
    freq = linspace([0.0d0,1.0d0/dt/2.0d0],fft_size/2  )
    I_matrix = SurfaceWave2DispersionCurve(waves,positions,c_axis,fft_size=fft_size,dt=dt)

    call f%open("swe_phv.txt","w")
    do i=1,size(I_matrix,1)
        do j=1,size(I_matrix,2)
            write(f%fh,*) freq(i),c_axis(j),I_matrix(i,j)
        enddo
        write(f%fh,*) " "
    enddo
    call f%close()

contains

!function dispersing_wave(phase_velocity,dt,wave) result(ret)
!    interface 
!        function phase_velocity(f) result(ret)
!            use iso_fortran_env
!            real(real64),intent(in) :: f
!            real(real64) :: ret
!
!        end function phase_velocity
!    end interface
!    real(real64),intent(in) :: wave(:),dt
!    real(real64) :: freq,c,w
!    complex(real64),allocatable :: Fw(:), ret_wave(:)
!    real(real64),allocatable :: ret(:),f_axis(:)
!    type(Math_) :: math
!
!    Fw = FFT(to_complex(wave) )
!    f_axis = linspace([0.0d0,1.0d0/dt/2.0],size(FFT)/2 )
!    do i=1,size(Fw)
!        freq = f_axis(i)
!        c    = phase_velocity(freq)
!        w    = 2.0d0*math%pi*freq
!        k    = w/c
!        ret_wave = ret_wave + 1.0d0/dble(size(Fw))*exp(math%i*(k*x - w*t ))*Fw(:) 
!    enddo
!
!end function

end program main