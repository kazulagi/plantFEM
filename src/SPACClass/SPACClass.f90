module SPACClass
   use iso_fortran_env
   use MathClass
   use ArrayClass
   use IOClass
   use SpectreAnalysisClass
   implicit none

   type :: SPAC_
      character(:), allocatable :: csv_wave_file
      character(:), allocatable :: json_metadata_file
      character(:), allocatable :: wave_data_format
      real(real64) :: sampling_Hz
      real(real64)   :: radius      !=!freal(f%parse
      integer(int32) :: num_logger  !!= fint(f
      integer(int32) :: fft_size    !!= fint(f
      real(real64)   :: Maximum_phase_velocity    !=!freal(f%parse
      integer(int32) :: Maximum_itr    !!= fint(f
      integer(int32) :: num_smoothing    !!= fint(f
      ![new!]
      real(real64)   :: cutoff_sd        != !freal(f
      integer(int32) :: taper_percent        !!= fint(f
      real(real32)   :: bandpass_low         != !freal(f
      real(real32)   :: bandpass_high        != !freal(f
      real(real64), allocatable :: observation(:, :)
      logical :: initialized = .false.
      logical :: inversion_is_done = .false.

      character(:), allocatable :: d_unit
      character(:), allocatable :: Rayleigh_Dispersion

      character(:), allocatable :: best_1
      character(:), allocatable :: best_2
      character(:), allocatable :: best_3
      character(:), allocatable :: best_4
      character(:), allocatable :: best_5

      character(:), allocatable :: best_1_HoverV
      character(:), allocatable :: best_2_HoverV
      character(:), allocatable :: best_3_HoverV
      character(:), allocatable :: best_4_HoverV
      character(:), allocatable :: best_5_HoverV

      character(:), allocatable :: best_1_layer_csv
      character(:), allocatable :: best_2_layer_csv
      character(:), allocatable :: best_3_layer_csv
      character(:), allocatable :: best_4_layer_csv
      character(:), allocatable :: best_5_layer_csv

      real(real64), allocatable ::  best_1_Vs(:)
      real(real64), allocatable ::  best_1_Vp(:)
      real(real64), allocatable ::  best_1_Density(:)
      real(real64), allocatable ::  best_1_Thickness(:)
      real(real64), allocatable ::  best_2_Vs(:)
      real(real64), allocatable ::  best_2_Vp(:)
      real(real64), allocatable ::  best_2_Density(:)
      real(real64), allocatable ::  best_2_Thickness(:)
      real(real64), allocatable ::  best_3_Vs(:)
      real(real64), allocatable ::  best_3_Vp(:)
      real(real64), allocatable ::  best_3_Density(:)
      real(real64), allocatable ::  best_3_Thickness(:)
      real(real64), allocatable ::  best_4_Vs(:)
      real(real64), allocatable ::  best_4_Vp(:)
      real(real64), allocatable ::  best_4_Density(:)
      real(real64), allocatable ::  best_4_Thickness(:)
      real(real64), allocatable ::  best_5_Vs(:)
      real(real64), allocatable ::  best_5_Vp(:)
      real(real64), allocatable ::  best_5_Density(:)
      real(real64), allocatable ::  best_5_Thickness(:)
   contains
      procedure, public :: init => init_SPAC
      procedure, public :: run => run_SPAC
      procedure, public :: pdf => pdf_SPAC
      procedure, public :: inversion => inversion_SPAC
   end type

contains

   function to_FOURIER_SPECTRUM(A,frequency,FFT_SIZE,log,num_block,taper_percent,NUM_MOVING_AVERAGE,delta_f) result(FourierSpectrum)
      real(real64), intent(in) :: A(:), frequency(:)

      integer(int32), intent(in) :: FFT_SIZE
      complex(real64), allocatable :: FFT_A(:), FFT_B(:), A_c(:), B_c(:)

      real(real64), allocatable :: FourierSpectrum(:), F_A(:)
      integer(int32), optional, intent(in) :: taper_percent, NUM_MOVING_AVERAGE
      real(real64), optional, intent(in) :: delta_f
      integer(int32) :: taper_percent_i, NUM_MOVING_AVERAGE_i
      integer(int32) :: n, num_bl, i, from, to, k
      character(*), optional, intent(in) :: log
      integer(int32), optional, intent(inout) :: num_block
      real(real64) :: dt

      ! Taper :: 5%
      type(IO_) :: f, debug

      taper_percent_i = input(default=5, option=taper_percent)
      NUM_MOVING_AVERAGE_i = input(default=5, option=NUM_MOVING_AVERAGE)
      FourierSpectrum = zeros(FFT_SIZE)
      num_bl = int(dble(size(A))/dble(FFT_SIZE*2))

      if (present(num_block)) then
         num_block = num_bl
      end if

      from = 1
      A_c = A

      if (present(log)) then
         call f%open(log + "_FFT_blocks.csv")
      end if
      do i = 1, num_bl
         A_c = A

         from = (i - 1)*FFT_SIZE*2 + 1
         to = (i - 1)*FFT_SIZE*2 + FFT_SIZE*2

         A_c = A_c(from:to)
         A_c(:) = A_c(:)*taper_function(size(A_c), percent=taper_percent_i)

         FFT_A = FFT(A_c(:))

         FFT_A = FFT_A(1:FFT_SIZE)

         !F_A = sqrt(dble(FFT_A*conjg(FFT_A)))
         F_A = sqrt(abs(FFT_A))
         F_A = moving_average(F_A, NUM_MOVING_AVERAGE_i)
         if (present(delta_f)) then
            F_A = F_A*(FFT_SIZE*dt)/2.0d0
         end if
         if (present(log)) then
            call f%write(frequency, F_A, separator=", ")
            call f%write(" ")
            call f%write(" ")
         end if
         FourierSpectrum = FourierSpectrum + F_A

      end do
      if (present(log)) then
         call f%close()
      end if
      FourierSpectrum = FourierSpectrum/num_bl

   end function

   function to_HoverV_spectra(H, V, FFT_SIZE, frequency, taper_percent) result(HoverV)
      real(real64), intent(in) :: H(:), V(:), frequency(:)
      integer(int32), intent(in) :: FFT_SIZE
      integer(int32), optional, intent(in) :: taper_percent
      integer(int32) :: i
      real(real64), allocatable :: HoverV(:), H_F(:), V_F(:)

      H_F = to_FOURIER_SPECTRUM( &
            A=H, &
            frequency=frequency, &
            FFT_SIZE=FFT_SIZE, &
            taper_percent=input(default=5, option=taper_percent))
      V_F = to_FOURIER_SPECTRUM( &
            A=V, &
            frequency=frequency, &
            FFT_SIZE=FFT_SIZE, &
            taper_percent=input(default=5, option=taper_percent))

      HoverV = 0.0d0*H_F
      do i = 1, size(HoverV)
         if (V_F(i) == 0.0d0) cycle
         HoverV(i) = H_F(i)/V_F(i)
      end do

   end function

   function to_CROSS_SPECTRUM(A, B, FFT_SIZE) result(Cross_Spectrum)
      real(real64), intent(in) :: A(:), B(:)

      integer(int32), intent(in) :: FFT_SIZE
      complex(real64), allocatable :: FFT_A(:), FFT_B(:), Cross_Spectrum(:), A_c(:), B_c(:)
      integer(int32) :: n, num_block, i, from, to
      type(math_) :: math

      Cross_Spectrum = zeros(FFT_SIZE)
      num_block = int(dble(size(A))/dble(FFT_SIZE*2))
      if (num_block == 0) stop
      from = 1
      A_c = A
      B_c = B
      do i = 1, num_block
         from = (i - 1)*FFT_SIZE*2 + 1
         to = (i - 1)*FFT_SIZE*2 + FFT_SIZE*2
         FFT_A = FFT(A_c(from:to))
         FFT_B = FFT(B_c(from:to))

         FFT_A = FFT_A(1:FFT_SIZE)
         FFT_B = FFT_B(1:FFT_SIZE)

         !Cross_Spectrum(:) = Cross_Spectrum(:) + FFT_A(:)*conjg(FFT_B(:))
         Cross_Spectrum = Cross_Spectrum + FFT_A*conjg(FFT_B)
      end do

      Cross_Spectrum = Cross_Spectrum/num_block

   end function

   function to_CCF(A, B, FFT_SIZE) result(CCF)
      real(real64), intent(in) :: A(:), B(:)

      integer(int32), intent(in) :: FFT_SIZE
      complex(real64), allocatable :: E_C_BB(:), CCF(:), FFT_A(:), FFT_B(:), &
                                      A_c(:), B_c(:), E_C_AB(:), E_C_AA(:)

      integer(int32) :: n, num_block, i, from, to
      type(Math_) :: math
      type(IO_) :: f

      ! < #1 EXPECTATION TO CCF>

      E_C_AB = to_CROSS_SPECTRUM(A, B, FFT_SIZE)
      E_C_AA = to_CROSS_SPECTRUM(A, A, FFT_SIZE)
      E_C_BB = to_CROSS_SPECTRUM(B, B, FFT_SIZE)

      ! SAME
      !E_C_AA = to_FourierSpectrum(A=A,FFT_SIZE=FFT_SIZE)
      !E_C_BB = to_FourierSpectrum(A=B,FFT_SIZE=FFT_SIZE)
      CCF = dble(E_C_AB)/sqrt(E_C_AA)/sqrt(E_C_BB)

!    ! < #2 CCF TO EXPECTATION>
!
!    E_C_AB = zeros(FFT_SIZE)
!    E_C_AA = zeros(FFT_SIZE)
!    E_C_BB = zeros(FFT_SIZE)
!    CCF    = zeros(FFT_SIZE)
!
!    num_block = int(dble(size(A))/dble(FFT_SIZE*2) )
!    from = 1
!    A_c = A
!    B_c = B
!    do i=1,num_block
!        from = (i-1)*FFT_SIZE*2 + 1
!        to   = (i-1)*FFT_SIZE*2 + FFT_SIZE*2
!
!        ! segment-wise FFT
!        FFT_A = FFT( A_c(from:to) )
!        FFT_B = FFT( B_c(from:to) )
!
!        FFT_A = FFT_A(1:FFT_SIZE)
!        FFT_B = FFT_B(1:FFT_SIZE)
!
!        E_C_AB = FFT_A*conjg(FFT_B)
!        E_C_AA = abs(FFT_A)
!        E_C_BB = abs(FFT_B)
!
!        CCF = CCF + dble(E_C_AB/sqrt(abs(E_C_AA))/sqrt(abs(E_C_BB)))
!    enddo
!    CCF = CCF/num_block
!

   end function

   function to_SPAC_COEFF(Center_x, Circle_x, FFT_SIZE) result(rho)
      real(real64), intent(in) :: Center_x(:), Circle_x(:, :) !,Angle(:)
      real(real64), allocatable :: rho(:), phi(:)
      real(real64) :: delta_angle
      integer(int32), intent(in) :: FFT_SIZE
      integer(int32) :: i, NUM_SAMPLE
      type(Math_) ::math

      NUM_SAMPLE = size(Circle_x, 2)

      ! Specification for Observation:
      ! Center_x(:)      = time series of center logger (r=0)
      ! Circle_x(:,i)    = time series of i-th logger
      ! Angle(i)         = angle of i-th logger
      rho = zeros(FFT_SIZE)
      !allocate(phi(0:size(Angle) ) )
      !phi(0) = 0
      !phi(1:) = Angle(:)
    !!!$OMP parallel do default(shared) reduction(+:rho)
      do i = 1, NUM_SAMPLE
         ! rho(r, omega)
         !delta_angle = phi(i) - phi(i-1)
         !rho(:) = rho(:) + dble(to_CCF(Center_x,Circle_x(:,i),FFT_SIZE=FFT_SIZE ))*radian(delta_angle)
         rho(:) = rho(:) + dble(to_CCF(Center_x, Circle_x(:, i), FFT_SIZE=FFT_SIZE))

         ! スペクトルホワイトニングを実装@2023/11/7
         !rho(:) = rho(:) + dble(to_CCF( SpectralWhitening(Center_x) ,  SpectralWhitening(Circle_x(:,i))  ,FFT_SIZE=FFT_SIZE ))

      end do
    !!!$OMP end parallel do

      rho(:) = rho(:)/NUM_SAMPLE

   end function

! ##############################################################
   function to_phase_velocity(Center_x, Circle_x, FFT_SIZE, radius, sampling_Hz, debug, &
                              max_c, max_itr, wave_type, num_smoothing) result(c)
      real(real64), intent(in) :: Center_x(:), Circle_x(:, :), radius
      real(real64), allocatable :: rho(:), c(:), freq(:), k(:), vbuf(:)
      character(*), optional, intent(in) :: wave_type
      character(:), allocatable :: target_wave_type
      logical, optional, intent(in) :: debug

      real(real64), intent(in) :: max_c
      integer(int32), intent(in) :: max_itr

      real(real64) :: residual, tangent_value, epsilon_val, tol, rf, rb, tr1, tr2, tr0, ctr, &
                      k_i, max_k, min_k, min_c
      integer(int32), intent(in) :: FFT_SIZE, sampling_Hz, num_smoothing
      integer(int32) :: i, NUM_SAMPLE, itr
      type(Math_) ::math

      if (present(wave_type)) then
         target_wave_type = wave_type
      else
         target_wave_type = "Rayleigh"
      end if

      if (index(target_wave_type, "Rayleigh") /= 0) then

         rho = to_SPAC_COEFF(Center_x=Center_x, Circle_x=Circle_x, FFT_SIZE=FFT_SIZE)

         ! fitting by gradient descent method
         freq = to_frequency_axis(FFT_SIZE=FFT_SIZE, sampling_Hz=sampling_Hz)
         !freq = linspace([0.0d0,dble(sampling_Hz)/2.0d0 ],FFT_SIZE)
         c = zeros(FFT_SIZE)
         !
         !k = zeros(FFT_SIZE)
         !k = freq*2.0d0*math%PI/c

         c(1) = 0.0d0
        !!$OMP parallel do default(shared) private(itr,ctr,residual,k_i,tr0)
         !do i=2,FFT_SIZE
         do i = 1, FFT_SIZE
            min_c = 0.50d0 ! m/s
            ! binary search

            !c(i) = minvalx(&
            !    fx = residual_from_bessel_SPAC_function,&
            !    params = [ rho(i), radius, freq(i) ],&
            !    x_range = [min_c , max_c ],&
            !    depth = max_itr)

            c(i) = (max_c)/max_itr
            ! grid search
            do itr = 1, max_itr
               ctr = itr*(max_c)/max_itr
               k_i = freq(i)*2.0d0*math%PI/ctr
               residual = (rho(i) - Bessel_J0(radius*k_i))**2

               if (itr == 1) then
                  tr0 = residual
               else
                  if (residual < tr0) then
                     c(i) = ctr
                     tr0 = residual
                  end if
               end if
            end do

         end do
        !!$OMP end parallel do
      else
         print *, "[ERROR] to_phase_velocity >> only for Rayleigh wave"
      end if

   end function
! ##############################################################

   function residual_from_bessel_SPAC_function(x, params) result(ret)
      real(real64), intent(in) :: x, params(:)
      real(real64) :: ret, radius, rho, k, freq
      type(math_) :: math

      ! c = x
      rho = params(1)
      radius = params(2)
      freq = params(3)
      k = freq*2.0d0*math%PI/x
      !k = x
      ret = (rho - Bessel_J0(radius*k))*(rho - Bessel_J0(radius*k))

   end function
! ##############################################################

   function to_frequency_axis(FFT_SIZE, sampling_Hz) result(f_axis)
      integer(int32), intent(in) :: FFT_SIZE, sampling_Hz
      real(real64), allocatable:: f_axis(:)

      f_axis = linspace([sampling_Hz/2.0d0/FFT_SIZE, sampling_Hz/2.0d0], FFT_SIZE)

   end function

   function to_time_axis(sampling_Hz, NUM_SAMPLE) result(t_axis)
      integer(int32), intent(in) :: sampling_Hz, NUM_SAMPLE
      real(real64), allocatable:: t_axis(:)

      t_axis = linspace([0.0d0, NUM_SAMPLE*1.0d0/dble(sampling_Hz)], NUM_SAMPLE)

   end function

   recursive function moving_average(indata, num) result(outdata)
      real(real64), intent(in) :: indata(:)
      real(real64), allocatable :: outdata(:), indata_clone(:)
      integer(int32), optional, intent(in) :: num
      integer(int32) :: n, i, j

      if (present(num)) then
         indata_clone = indata
         if (num == 0) then
            outdata = indata_clone
            return
         end if
         n = size(indata_clone)
         do i = 1, num
            outdata = moving_average(indata_clone)
            indata_clone = outdata
         end do
      else
         n = size(indata)
         outdata = zeros(n)
         do i = 2, n - 1
            outdata(i) = (indata(i - 1) + indata(i) + indata(i + 1))/3
         end do
      end if

   end function
! #################################################

   subroutine init_SPAC(this, csv_wave_file, json_metadata_file)
      class(SPAC_), intent(inout) :: this
      character(*), intent(in) :: csv_wave_file, json_metadata_file
      type(IO_)::f

      this%wave_data_format = "t,UD,EW,NS"
      this%csv_wave_file = csv_wave_file
      this%json_metadata_file = json_metadata_file

      this%Rayleigh_Dispersion = this%csv_wave_file + '_Rayl-Dispersion.csv'

      this%initialized = .true.

      this%num_logger = fint(f%parse(this%json_metadata_file, key1="num_logger")) != 4
   end subroutine
! #################################################

! #################################################
   subroutine run_SPAC(this, only_FFT, only_summary)
      class(SPAC_), intent(inout) :: this
      logical, optional, intent(in) :: only_FFT, only_summary
      real(real64), allocatable :: Angle(:), A(:), All_data(:, :), buf(:, :)
      real(real64), allocatable :: t(:), freq(:), SPAC_COEFF(:), phase_velocity(:), HoverV_spectra(:), &
                                   FourierSpectrum(:), A_buf(:), vbuf(:), position_xy(:, :)
      real(real64) :: radius, Maximum_phase_velocity, cutoff_sd
      integer(int32) :: FFT_SIZE, NUM_SAMPLE, num_logger, Maximum_itr, &
                        num_smoothing, NUM_MOVING_AVERAGE, i, j, logger_id, taper_percent, k
      type(IO_) :: f
      real(real32) :: sampling_Hz, bandpass_high, bandpass_low, theta
      character(50) :: fpath, data_unit
      character(:), allocatable :: filepath, config
      character(:), allocatable :: HoverV_spectra_EW
      character(:), allocatable :: HoverV_spectra_NS
      integer(int32), allocatable :: ids(:)
      type(IO_) :: logfile, debug
      type(SpectreAnalysis_) :: speana
      type(Math_) :: math

      logical :: stop_before_spac, skip_two_point_SPAC

      if (present(only_summary)) then
         skip_two_point_SPAC = only_summary
      else
         skip_two_point_SPAC = .false.
      end if

      stop_before_spac = input(default=.false., option=only_FFT)

      if (.not. this%initialized) then
         print *, "[ERROR] run this%init() prior to this operation."
         stop
      end if

      call logfile%open("SPAC_LOG.txt", "w")

      filepath = adjustl(this%csv_wave_file)
      config = adjustl(this%json_metadata_file)

      call logfile%write("wavedata.csv  :"+filepath)
      call logfile%write("metadata.json :"+config)
      call logfile%flush()

      ! >>>>>>>> INPUT DATA INFO >>>>>>>>>>>
      sampling_Hz = fint(f%parse(config, key1="sampling_hz")) != 250
      radius = freal(f%parse(config, key1="radius")) != 4.0d0
      num_logger = fint(f%parse(config, key1="num_logger")) != 4
      fft_size = fint(f%parse(config, key1="fft_size")) != 1024*2*2
      Maximum_phase_velocity = freal(f%parse(config, key1="maximum_phase_velocity")) != 5000.0
      Maximum_itr = fint(f%parse(config, key1="maximum_itr")) != 100000
      num_smoothing = fint(f%parse(config, key1="num_smoothing")) != 10
      ![new!]
      cutoff_sd = freal(f%parse(config, key1="cutoff_sd")) != 10
      taper_percent = fint(f%parse(config, key1="taper_percent")) != 10
      bandpass_low = freal(f%parse(config, key1="bandpass_low")) != 10
      bandpass_high = freal(f%parse(config, key1="bandpass_high")) != 10
      data_unit = f%parse(config, key1="data_unit")
      this%d_unit = data_unit

      this%sampling_Hz = sampling_Hz
      this%radius = radius
      this%num_logger = num_logger
      this%fft_size = fft_size
      this%Maximum_phase_velocity = Maximum_phase_velocity
      this%Maximum_itr = Maximum_itr
      this%num_smoothing = num_smoothing
      this%cutoff_sd = cutoff_sd
      this%taper_percent = taper_percent
      this%bandpass_low = bandpass_low
      this%bandpass_high = bandpass_high
      ! >>>>>>>> INPUT DATA INFO >>>>>>>>>>>
      call logfile%write("[ok] META DATA LOADED")
      call logfile%flush()
      write (logfile%fh, *) "sampling_Hz", sampling_Hz
      write (logfile%fh, *) "radius", radius
      write (logfile%fh, *) "num_logger", num_logger
      write (logfile%fh, *) "fft_size", fft_size
      write (logfile%fh, *) "Maximum_phase_velocity", Maximum_phase_velocity
      write (logfile%fh, *) "Maximum_itr", Maximum_itr
      write (logfile%fh, *) "num_smoothing", num_smoothing
      write (logfile%fh, *) "cutoff_sd", cutoff_sd
      write (logfile%fh, *) "taper_percent", taper_percent
      write (logfile%fh, *) "bandpass_low", bandpass_low
      write (logfile%fh, *) "bandpass_high", bandpass_high

      ! >>>>>>>> READ DATA INFO >>>>>>>>>>>
      NUM_SAMPLE = f%numLine(filepath)
      t = to_time_axis(int(sampling_Hz), NUM_SAMPLE)

      !All_data = zeros(f%numline(filepath), num_logger*3)
      !do i = 1, num_logger*3
      !   All_data(:, i) = from_CSV(filepath, column=1 + i)
      !   All_data(:, i) = All_data(:, i) - average(All_data(:, i))
      !end do
      All_data = f%to_array(filepath,column=[(i, i=2,num_logger*3+1)],header=0)
      !call print(shape(All_data))
      do i=1,size(All_data,2)
         All_data(:,i) = All_data(:,i) - average(All_data(:,i) )
      enddo

      ! >>>>>>>> READ DATA INFO >>>>>>>>>>>
      call logfile%write("[ok] WAVE DATA LOADED")
      call logfile%flush()

      call speana%init(sampling_Hz=sampling_Hz)

      ! >>>>>>>> READ DATA INFO >>>>>>>>>>>
      freq = to_frequency_axis(FFT_SIZE=FFT_SIZE, sampling_Hz=int(sampling_Hz))

      buf = speana%cutif(All_data, sigma=cutoff_sd, window_size=FFT_SIZE/2, log=filepath)

      call logfile%write("[ok] n : "+str(size(All_data, 1)) + "=>"+str(size(buf, 1)))
      All_data = buf

      ! >>>>>>>> READ DATA INFO >>>>>>>>>>>
      call logfile%write("[ok] PREPROCESSING DONE!")
      call logfile%flush()
      ! >>>>>>>> Fourier spectra >>>>>>>>
      do j = 1, num_logger*3
         call speana%init(sampling_Hz=sampling_Hz)
         A_buf = All_data(:, j)

         !stop
         A = speana%bandpass(A_buf, [bandpass_low, bandpass_high])

         ! >>>>>>>> Fourier spectra >>>>>>>>
         FourierSpectrum = to_FOURIER_SPECTRUM( &
                           A=A, &
                           frequency=freq, &
                           FFT_SIZE=FFT_SIZE, &
                           taper_percent=taper_percent, &
                           log=filepath + "_"+zfill(j, 3))
         ! >>>>>>>> Fourier spectra >>>>>>>>

         ! need debug >> NaN

         A_buf = moving_average(FourierSpectrum, num_smoothing)
         FourierSpectrum = A_buf

         call f%open(filepath + "_"+zfill(j, 3) + "_FFT.csv", "w")
         do k = 1, size(freq)
            write (f%fh, *) freq(k), ",", FourierSpectrum(k)
         end do
         call f%flush()
         call f%close()
      end do
      ! >>>>>>>> Fourier spectra >>>>>>>>
      call logfile%write("[ok] FOURIER SPECTRUM DONE!")
      call logfile%flush()

      ! >>>>>>>> H/V spectra (Ch2/Ch1)>>>>>>>>
      do i = 1, num_logger
         HoverV_spectra = &
            to_HoverV_spectra( &
            H=All_data(:, 3*(i - 1) + 2), &
            V=All_data(:, 3*(i - 1) + 1), &
            frequency=freq, &
            FFT_SIZE=FFT_SIZE, &
            taper_percent=0)

         vbuf = moving_average(HoverV_spectra, num_smoothing)
         HoverV_spectra = vbuf
         call f%open(filepath + "_"+zfill(i, 3) + "_HoverV-spectra_Ch2_Ch1.csv", "w")
         do k = 1, size(freq)
            write (f%fh, *) freq(k), ",", HoverV_spectra(k)
         end do
         call f%close()
      end do
      ! >>>>>>>> H/V spectra (Ch2/Ch1) >>>>>>>>
      call logfile%write("[ok] H/V (Ch2/Ch1) DONE!")
      call logfile%flush()

      ! >>>>>>>> H/V spectra (Ch3/Ch1) >>>>>>>>

      do i = 1, num_logger
         HoverV_spectra = to_HoverV_spectra( &
                          H=All_data(:, 3*(i - 1) + 3), &
                          V=All_data(:, 3*(i - 1) + 1), &
                          frequency=freq, &
                          FFT_SIZE=FFT_SIZE, &
                          taper_percent=0)

         vbuf = moving_average(HoverV_spectra, num_smoothing)
         HoverV_spectra = vbuf
         call f%open(filepath + "_"+zfill(i, 3) + "_HoverV-spectra_Ch3_Ch1.csv", "w")
         do k = 1, size(freq)
            write (f%fh, *) freq(k), ",", HoverV_spectra(k)
         end do
         call f%close()
      end do
      ! >>>>>>>> H/V spectra (Ch3/Ch1) >>>>>>>>
      call logfile%write("debug")
      call logfile%write("[ok] H/V (Ch3/Ch1) DONE!")
      call logfile%flush()
      if (stop_before_spac) then
         return
      end if

      ! debug
      !ids = [(i,i=4,num_logger*3,3)]

      ! >>>>>>>> create position >>>>>>>>
      allocate (position_xy(num_logger, 1:2))
      position_xy(1, 1:2) = 0.0d0
      do i = 2, num_logger
         theta = 2.0d0*math%pi/(num_logger - 1)*(i - 2)
         position_xy(i, 1) = this%radius*cos(theta)
         position_xy(i, 2) = this%radius*sin(theta)
      end do
      ! >>>>>>>> create position >>>>>>>>

      call logfile%write("[ok] Logger position")
      call logfile%flush()
      if (.not. skip_two_point_SPAC) then
         ! >>>>>>>> SPAC coefficient >>>>>>>>
         ! two-point SPAC
         ids = int(zeros(num_logger))
         ids(1) = 1
         do i = 1, num_logger - 1
            ids(i + 1) = ids(i) + 3
         end do

         do i = 1, num_logger
            do j = 1, i - 1

               ! debug
               ! input >> ok!
               !            call f%open("center_x.txt","w")
               !            do k=1,size(All_data,1)
               !                write(f%fh,*)All_data(k, ids(i))
               !            enddo
               !            call f%close()
               !            call f%open("circle_x.txt","w")
               !            do k=1,size(All_data,1)
               !                write(f%fh,*)All_data(k, ids(j))
               !            enddo
               !            call f%close()
               !            stop

               SPAC_COEFF = to_SPAC_COEFF( &
                            Center_x=All_data(:, ids(i)), &
                            Circle_x=All_data(:, [ids(j)]), &
                            FFT_SIZE=FFT_SIZE)

               call f%open(filepath + "_SPAC_COEFF_2pt_"+zfill(i, 3) + "_"+zfill(j, 3) + ".csv", "w")
               do k = 1, size(freq)
                  write (f%fh, *) freq(k), ",", SPAC_COEFF(k)
               end do
               call f%close()

               phase_velocity = to_phase_velocity( &
                                Center_x=All_data(:, ids(i)), &
                                Circle_x=All_data(:, [ids(j)]), &
                                FFT_SIZE=FFT_SIZE, &
                                radius=norm(position_xy(i, :) - position_xy(j, :)), &
                                sampling_Hz=int(sampling_Hz), &
                                max_c=Maximum_phase_velocity, & ! m/s
                                max_itr=Maximum_itr, &
                                num_smoothing=num_smoothing, &
                                debug=.true.)

               call f%open(filepath + "_Rayl-Dispersion_2pt_"+zfill(i, 3) + "_"+zfill(j, 3) + ".csv", "w")
               do k = 1, size(freq)
                  write (f%fh, *) freq(k), ",", phase_velocity(k)
               end do
               call f%close()

            end do
         end do
      end if

      ids = int(zeros(num_logger - 1))
      ids(1) = 4
      do i = 1, num_logger - 2
         ids(i + 1) = ids(i) + 3
      end do
      call logfile%write("[>>] SPAC COEFFICIENT")
      call logfile%flush()
      ! 4-point SPAC
      SPAC_COEFF = to_SPAC_COEFF( &
                   Center_x=All_data(:, 1), &
                   Circle_x=All_data(:, ids(:)), &
                   FFT_SIZE=FFT_SIZE)
      call f%open(filepath + "_SPAC_COEFF.csv", "w")

      do k = 1, size(freq)
         write (f%fh, *) freq(k), ",", SPAC_COEFF(k)
      end do
      call f%close()

      ! >>>>>>>> SPAC coefficient >>>>>>>>

      call logfile%write("[ok] SPAC COEFFICIENT DONE!")
      call logfile%flush()

      ! >>>>>>>> Rayleigh wave dispersion curve >>>>>>>>
      phase_velocity = to_phase_velocity( &
                       Center_x=All_data(:, 1), &
                       Circle_x=All_data(:, [(i, i=4, num_logger*3, 3)]), &
                       FFT_SIZE=FFT_SIZE, &
                       radius=radius, &
                       sampling_Hz=int(sampling_Hz), &
                       max_c=Maximum_phase_velocity, & ! m/s
                       max_itr=Maximum_itr, &
                       num_smoothing=num_smoothing, &
                       debug=.true.)

      ! smoothing

      !phase_velocity = moving_average(phase_velocity,num_smoothing)

      call f%open(filepath + "_Rayl-Dispersion.csv", "w")
      do k = 1, size(freq)
         write (f%fh, *) freq(k), ",", phase_velocity(k)
      end do
      call f%close()
      ! >>>>>>>> Rayleigh wave dispersion curve >>>>>>>>
      call logfile%write("[ok] PHASE VELOCITY DONE!")
      call logfile%flush()

   end subroutine

   subroutine pdf_SPAC(this, name)
      class(SPAC_), intent(in) :: this
      character(*), intent(in) :: name
      type(IO_) :: f, layer_csv
      integer(int32) :: logger_id, i, j, system_ret

      character(:), allocatable :: pdf_name
      if (index(name, ".pdf") == 0) then
         pdf_name = name + ".pdf"
      else
         pdf_name = name
      end if

      ! >>>>>>>> GNUPLOT file >>>>>>>>
      call f%open(this%csv_wave_file + "_SPAC_LOG.gp")
      call f%write('set terminal pdf')
      call f%write('set output "'+pdf_name + '"')
      call f%write('set title "'+name + '"')
      call f%write('set datafile separator ","')
      call f%write('set format y "10^{%L}"')
      call f%write('set grid')

      call f%write('Hsize = 210.0 ') ! A4
      call f%write('Vsize = 297.0 ') ! A4
      call f%write('set xr['+str(this%bandpass_low) + ':'+str(this%bandpass_high) + ']')
      call f%write('set size Vsize, Hsize')

      logger_id = 1
      do i = 1, this%num_logger/2 + 1
         if (logger_id > this%num_logger) exit
         call f%write('set multiplot layout 2,3 rowsfirst title &
 &            "Fourier spectrum ('+str(i) + '/'+str(this%num_logger/2 + 1) + ')" font "Times,10"')
         call f%write('set xtics font "Times,10" ')
         call f%write('set ytics font "Times,10" ')
         call f%write('set title font "Times,10"')
         call f%write('unset key')
         call f%write('set logscale')
         do j = 1, 2
            call f%write('set title "L'+str(logger_id) + ' (CH1)"')
            call f%write('set xlabel "Frequency (Hz)" font "Times,10"')
            call f%write('set ylabel "Fourier spectrum ('+this%d_unit + ') " font "Times,10"')
            call f%write('plot   "'+this%csv_wave_file + '_'+zfill(3*(logger_id - 1) + 1, 3) + '_FFT_blocks.csv" u 1:2 w l,&
&                 "'+this%csv_wave_file + '_'+zfill(3*(logger_id - 1) + 1, 3) + '_FFT.csv" u 1:2 w l ')

            call f%write('set title "L'+str(logger_id) + ' (CH2)"')
            call f%write('set xlabel "Frequency (Hz)" font "Times,10"')
            call f%write('set ylabel "Fourier spectrum ('+this%d_unit + ') " font "Times,10"')
            call f%write('plot   "'+this%csv_wave_file + '_'+zfill(3*(logger_id - 1) + 2, 3) + '_FFT_blocks.csv" u 1:2 w l,&
&                 "'+this%csv_wave_file + '_'+zfill(3*(logger_id - 1) + 2, 3) + '_FFT.csv" u 1:2 w l ')

            call f%write('set title "L'+str(logger_id) + ' (CH3)"')
            call f%write('set xlabel "Frequency (Hz)" font "Times,10"')
            call f%write('set ylabel "Fourier spectrum ('+this%d_unit + ') " font "Times,10"')
            call f%write('plot   "'+this%csv_wave_file + '_'+zfill(3*(logger_id - 1) + 3, 3) + '_FFT_blocks.csv" u 1:2 w l,&
&                 "'+this%csv_wave_file + '_'+zfill(3*(logger_id - 1) + 3, 3) + '_FFT.csv" u 1:2 w l ')
            logger_id = logger_id + 1
            if (logger_id > this%num_logger) exit
         end do
         call f%write('set logscale')
      end do

      call f%write('set multiplot layout 1,2 rowsfirst title "H/V spectral ratio" font "Times,10"')
      call f%write('set xtics font "Times,10" ')
      call f%write('set ytics font "Times,10" ')
      call f%write('set title font "Times,10"')
      call f%write('unset logscale')
      call f%write('unset format y')
      call f%write('unset key')
      call f%write('set logscale')
      call f%write('set title "H/V (NS)"')
      call f%write('set xlabel "Frequency (Hz)" font "Times,10"')
      call f%write('set ylabel "Spectral ratio" font "Times,10"')

      call f%write('plot  \')
      call f%write('"'+this%csv_wave_file + '_001_HoverV-spectra_Ch2_Ch1.csv" u 1:2 w l,\')
      do i = 2, this%num_logger - 1
         call f%write('"'+this%csv_wave_file + '_'+zfill(i, 3) + '_HoverV-spectra_Ch2_Ch1.csv" u 1:2 w l,\')
      end do
      call f%write('"'+this%csv_wave_file + '_'+zfill(this%num_logger, 3) + '_HoverV-spectra_Ch2_Ch1.csv" u 1:2 w l')

      call f%write('set title "H/V (EW)"')
      call f%write('set logscale')
      call f%write('set xlabel "Frequency (Hz)" font "Times,10"')
      call f%write('set ylabel "Spectral ratio" font "Times,10"')
      call f%write('plot  \')
      call f%write('"'+this%csv_wave_file + '_001_HoverV-spectra_Ch3_Ch1.csv" u 1:2 w l,\')
      do i = 2, this%num_logger - 1
         call f%write('"'+this%csv_wave_file + '_'+zfill(i, 3) + '_HoverV-spectra_Ch3_Ch1.csv" u 1:2 w l,\')
      end do
      call f%write('"'+this%csv_wave_file + '_'+zfill(this%num_logger, 3) + '_HoverV-spectra_Ch3_Ch1.csv" u 1:2 w l')
      call f%write('unset multiplot')

      do i = 1, this%num_logger
         do j = 1, i - 1
            if (f%exists(this%csv_wave_file + '_SPAC_COEFF_2pt_'+zfill(i, 3) + '_'+zfill(j, 3) + '.csv')) then
               call f%write('set multiplot layout 1,2 rowsfirst title "Two-point SPAC" font "Times,10"')
               call f%write('set xtics font "Times,10" ')
               call f%write('set ytics font "Times,10" ')
               call f%write('set title font "Times,10"')
               call f%write('unset logscale')
               call f%write('unset format y')
               call f%write('unset key')
               call f%write('set title "SPAC coefficient from two points (L'+str(i) + ' and L'+str(j) + ') " font "Times,10"')
               call f%write('set xlabel "Frequency (Hz)" font "Times,10"')
               call f%write('set ylabel "SPAC coefficient"')
               call f%write('plot   "'+this%csv_wave_file + '_SPAC_COEFF_2pt_' &
                            +zfill(i, 3) + '_'+zfill(j, 3) + '.csv" u 1:2  pointsize 0.2')
               call f%write('set title "Phase velocity from two points (L'+str(i) + ' and L'+str(j) + ') " font "Times,10"')
               call f%write('set xlabel "Frequency (Hz)" font "Times,10"')
               call f%write('set ylabel "Phase velocity (m/s)"')
               call f%write('plot   "'+this%csv_wave_file + '_Rayl-Dispersion_2pt_' &
                            +zfill(i, 3) + '_'+zfill(j, 3) + '.csv" u 1:2 pointsize 0.2')
               call f%write('unset multiplot')
            end if
         end do
      end do

      if (f%exists(this%csv_wave_file + '_SPAC_COEFF.csv')) then
         call f%write('set multiplot layout 1,2 rowsfirst title "SPAC" font "Times,10"')
         call f%write('set xtics font "Times,10" ')
         call f%write('set ytics font "Times,10" ')
         call f%write('set title font "Times,10"')
         call f%write('unset logscale')
         call f%write('unset format y')
         call f%write('unset key')
         call f%write('set title "SPAC coefficient" font "Times,10"')
         call f%write('set xlabel "Frequency (Hz)" font "Times,10"')
         call f%write('set ylabel "SPAC coefficient"')
         call f%write('plot   "'+this%csv_wave_file + '_SPAC_COEFF.csv" u 1:2  pointsize 0.2')
         call f%write('set title "Phase velocity" font "Times,10"')
         call f%write('set xlabel "Frequency (Hz)" font "Times,10"')
         call f%write('set ylabel "Phase velocity (m/s)"')

         if (this%inversion_is_done) then
            call f%write('plot   "'+this%csv_wave_file + '_Rayl-Dispersion.csv" u 1:2 pointsize 0.2 \')
            call f%write(", '"+this%best_1 + "' u 1:2 lt 12 w l \")
            call f%write(", '"+this%best_2 + "' u 1:2 lt 13 w l \")
            call f%write(", '"+this%best_3 + "' u 1:2 lt 14 w l ")

            call f%write('unset multiplot')

            ! H/V
            call f%write('set multiplot layout 1,2 rowsfirst title "H/V spectral ratio" font "Times,10"')
            call f%write('set xtics font "Times,10" ')
            call f%write('set ytics font "Times,10" ')
            call f%write('set title font "Times,10"')
            call f%write('unset logscale')
            call f%write('unset format y')
            call f%write('unset key')
            call f%write('set title "H/V (Ch1/Ch2)"')
            call f%write('set logscale')
            call f%write('set xlabel "Frequency (Hz)" font "Times,10"')
            call f%write('set ylabel "Spectral ratio" font "Times,10"')

            call f%write('plot  \')
            call f%write('"'+this%csv_wave_file + '_001_HoverV-spectra_Ch2_Ch1.csv" u 1:2 w l,\')
            do i = 2, this%num_logger - 1
               call f%write('"'+this%csv_wave_file + '_'+zfill(i, 3) + '_HoverV-spectra_Ch2_Ch1.csv" u 1:2 w l,\')
            end do
            call f%write('"'+this%csv_wave_file + '_'+zfill(this%num_logger, 3) + '_HoverV-spectra_Ch2_Ch1.csv" u 1:2 w l \')
            call f%write(", '"+this%best_1_HoverV + "' u 1:2 lt 12 w l \")
            call f%write(", '"+this%best_2_HoverV + "' u 1:2 lt 13 w l \")
            call f%write(", '"+this%best_3_HoverV + "' u 1:2 lt 14 w l ")

            call f%write('set title "H/V (NS)"')
            call f%write('set logscale')
            call f%write('set xlabel "Frequency (Hz)" font "Times,10"')
            call f%write('set ylabel "Spectral ratio" font "Times,10"')
            call f%write('plot  \')
            call f%write('"'+this%csv_wave_file + '_001_HoverV-spectra_CH3_Ch1.csv" u 1:2 w l,\')
            do i = 2, this%num_logger - 1
               call f%write('"'+this%csv_wave_file + '_'+zfill(i, 3) + '_HoverV-spectra_CH3_Ch1.csv" u 1:2 w l,\')
            end do
            call f%write('"'+this%csv_wave_file + '_'+zfill(this%num_logger, 3) + '_HoverV-spectra_CH3_Ch1.csv" u 1:2 w l \')
            call f%write(", '"+this%best_1_HoverV + "' u 1:2 lt 12 w l \")
            call f%write(", '"+this%best_2_HoverV + "' u 1:2 lt 13 w l \")
            call f%write(", '"+this%best_3_HoverV + "' u 1:2 lt 14 w l ")
            call f%write('unset multiplot')

            call f%write('set multiplot layout 1,3 rowsfirst title "Estimated layer structure" font "Times,10"')
            call f%write('set xtics font "Times,7" ')
            call f%write('set ytics font "Times,7" ')
            call f%write('set title font "Times,7"')
            call f%write('unset logscale')
            call f%write('unset format y')
            call f%write('unset key')
            call f%write('set title "Vs (m/s)" font "Times,10"')
            call f%write('set xlabel "Vs (m/s)" font "Times,10"')
            call f%write('set ylabel "Depth (m)"')
            call f%write('unset xr')
            call f%write('set xr[0:]')
            call f%write('plot "'+this%best_1_layer_csv + '" u 1:2 lt 12 w l, \')
            call f%write(' "'+this%best_2_layer_csv + '" u 1:2 lt 13 w l, \')
            call f%write(' "'+this%best_3_layer_csv + '" u 1:2 lt 14 w l ')

            call f%write('set title "Vp (m/s)" font "Times,10"')
            call f%write('set xlabel "Vp (m/s)" font "Times,10"')
            call f%write('set ylabel "Depth (m)"')
            call f%write('unset xr')
            call f%write('set xr[0:]')
            call f%write('plot "'+this%best_1_layer_csv + '" u 3:2 lt 12 w l, \')
            call f%write(' "'+this%best_2_layer_csv + '" u 3:2 lt 13 w l, \')
            call f%write(' "'+this%best_3_layer_csv + '" u 3:2 lt 14 w l ')

            call f%write('set title "Density (t/m^3)" font "Times,10"')
            call f%write('set xlabel "Density (t/m^3)" font "Times,10"')
            call f%write('set ylabel "Depth (m)"')
            call f%write('unset xr')
            call f%write('set xr[0:]')
            call f%write('plot "'+this%best_1_layer_csv + '" u 4:2 lt 12 w l, \')
            call f%write(' "'+this%best_2_layer_csv + '" u 4:2 lt 13 w l, \')
            call f%write(' "'+this%best_3_layer_csv + '" u 4:2 lt 14 w l ')

            call f%write('unset multiplot')

         else
            call f%write('plot   "'+this%csv_wave_file + '_Rayl-Dispersion.csv" u 1:2 pointsize 0.2')
            call f%write('unset multiplot')
         end if
      end if

      call f%close()
      ! >>>>>>>> GNUPLOT file >>>>>>>>

      system_ret = system("gnuplot "+this%csv_wave_file + "_SPAC_LOG.gp")

   end subroutine

! ##############################################################
   subroutine inversion_SPAC(this, db, db_shape, frequency_scope, name)
      class(SPAC_), intent(inout) :: this
      character(*), intent(in)  :: db, name
      integer(int32), intent(in):: db_shape(1:2)

      character(:), allocatable :: observation_file_name
      character(:), allocatable :: filepath, filename, buf, fpath
      character(300) :: my_filepath
      real(real64), allocatable :: trial_value(:, :), test_data(:, :), all_distance(:, :)
      real(real64), intent(in) ::frequency_scope(1:2)
      real(real64) :: realbuf, readbuf(1:8)
      integer(int32) :: proc_id, id, line, max_num_line, ids(1:2), max_ids(1:2), intbuf, i, j
      character(len=50) :: observation_file_name_50, charbuf
      character(:), allocatable :: fpath_all
      type(IO_) :: f, result, exact, chk, layer_csv

      ! $$$$$$$ SPECIFICATION OF DATABASE(DB) $$$$$$$
      ! (1) A DB repository should have multiple internal repositories and files
      ! (2) Each internal repository should be named as $DB_PATH // zfill(n,4)_"zfill(m,8)"
      ! (3) Each internal repository should have $DB_PATH // zfill(n,4)_"zfill(m,8)/Rayl-Dispersion.csv"
      !     which has two columns: frequency(Hz) and Phase velocity (m/s) with this order.
      ! (4) Each internal file should be named as $DB_PATH // zfill(n,4)_"zfill(m,8).csv"
      ! $$$$$$$ SPECIFICATION OF DATABASE(DB) $$$$$$$

      observation_file_name = this%Rayleigh_Dispersion

      max_num_line = f%numLine(observation_file_name) - 2
      test_data = zeros(max_num_line, 2)

      call exact%open(observation_file_name, "r")
      !call exact%goForward() !header
      do line = 1, max_num_line
         read (exact%fh, *) test_data(line, 1:2)
      end do
      call exact%close()

      filepath = db
      filename = zfill(1, 4) + "_"+zfill(1, 8) + "/Rayl-Dispersion.csv"
      max_num_line = f%numLine(filepath + filename) - 1

      ! open file and compute distance
      trial_value = zeros(max_num_line, 2)
      !call result%open(name,"w")
      all_distance = zeros(db_shape(1), db_shape(2))

      ! somehow openmp fails
    !!$OMP parallel do default(shared) private(buf,line,f,chk,trial_value,id,my_filepath,proc_id)
      do proc_id = 1, db_shape(1)
         do id = 1, db_shape(2)
            my_filepath = adjustl(db)
            !filename = zfill(proc_id,4)+"_"+zfill(id,8)+"/Rayl-Dispersion.csv"
            if (.not. chk%exists(my_filepath + zfill(proc_id, 4) + "_"+zfill(id, 8) + "/Rayl-Dispersion.csv")) cycle

            call f%open(my_filepath + zfill(proc_id, 4) + "_"+zfill(id, 8) + "/Rayl-Dispersion.csv", "r")
            buf = f%readline()
            do line = 1, max_num_line
               if (f%EOF) exit
               read (f%fh, *) trial_value(line, 1:2)
            end do
            call f%close()
            all_distance(proc_id, id) = distance(trial_value, test_data, scope=frequency_scope)
            !call result%write(str(all_distance(proc_id,id)) + ", "+ filepath + filename )

         end do
      end do
    !!$OMP end parallel do

      !call result%close()
      !call system("cat "+name+"| sort -gt, -k1 > sorted_"+name)

      max_ids(1:2) = maxvalID(all_distance)

      ids(1:2) = minvalID(all_distance)
      filepath = db
      filename = zfill(ids(1), 4) + "_"+zfill(ids(2), 8) + "/Rayl-Dispersion.csv"
      fpath_all = filepath + filename
      this%best_1 = fpath_all
      this%best_1_HoverV = db + zfill(ids(1), 4) + "_"+zfill(ids(2), 8) + "/HoverV-Spectra.csv"
      all_distance(ids(1), ids(2)) = all_distance(max_ids(1), max_ids(2))

      ids(1:2) = minvalID(all_distance)
      filepath = db
      filename = zfill(ids(1), 4) + "_"+zfill(ids(2), 8) + "/Rayl-Dispersion.csv"
      fpath_all = filepath + filename
      this%best_2 = fpath_all
      this%best_2_HoverV = db + zfill(ids(1), 4) + "_"+zfill(ids(2), 8) + "/HoverV-Spectra.csv"
      all_distance(ids(1), ids(2)) = all_distance(max_ids(1), max_ids(2))

      ids(1:2) = minvalID(all_distance)
      filepath = db
      filename = zfill(ids(1), 4) + "_"+zfill(ids(2), 8) + "/Rayl-Dispersion.csv"
      fpath_all = filepath + filename
      this%best_3 = fpath_all
      this%best_3_HoverV = db + zfill(ids(1), 4) + "_"+zfill(ids(2), 8) + "/HoverV-Spectra.csv"
      all_distance(ids(1), ids(2)) = all_distance(max_ids(1), max_ids(2))

      ids(1:2) = minvalID(all_distance)
      filepath = db
      filename = zfill(ids(1), 4) + "_"+zfill(ids(2), 8) + "/Rayl-Dispersion.csv"
      fpath_all = filepath + filename
      this%best_4 = fpath_all
      this%best_4_HoverV = db + zfill(ids(1), 4) + "_"+zfill(ids(2), 8) + "/HoverV-Spectra.csv"
      all_distance(ids(1), ids(2)) = all_distance(max_ids(1), max_ids(2))

      ids(1:2) = minvalID(all_distance)
      filepath = db
      filename = zfill(ids(1), 4) + "_"+zfill(ids(2), 8) + "/Rayl-Dispersion.csv"
      fpath_all = filepath + filename
      this%best_5 = fpath_all
      this%best_5_HoverV = db + zfill(ids(1), 4) + "_"+zfill(ids(2), 8) + "/HoverV-Spectra.csv"
      all_distance(ids(1), ids(2)) = all_distance(max_ids(1), max_ids(2))

      ! Parse structure data #1
      filename = this%best_1(:len(this%best_1) - 20) + ".csv"
      print *, filename
      call layer_csv%open(filename, "r")
      do i = 1, f%numline(filename)
         read (layer_csv%fh, *) charbuf
         if (index(charbuf, "NL") /= 0) then
            read (layer_csv%fh, *) intbuf

            this%best_1_Vs = zeros(intbuf)
            this%best_1_Vp = zeros(intbuf)
            this%best_1_Density = zeros(intbuf)
            this%best_1_Thickness = zeros(intbuf)
            read (layer_csv%fh, *) charbuf
            do j = 1, size(this%best_1_Vs)
               read (layer_csv%fh, *) intbuf, readbuf(1:8)
               this%best_1_Vs(j) = readbuf(5)
               this%best_1_Vp(j) = readbuf(2)
               this%best_1_Density(j) = readbuf(1)
               this%best_1_Thickness(j) = readbuf(8)
            end do
            exit
         end if
      end do
      call layer_csv%close()

      this%best_1_layer_csv = name + "_best_1_Vs_Thickness_Vp_Density.csv"

      call layer_csv%open(this%best_1_layer_csv, "w")
      write (layer_csv%fh, '(A)') str(this%best_1_Vs(1)) + ", "+str(0.0d0) + ", "+str(this%best_1_Vp(1)) + ", "+ &
         str(this%best_1_Density(1))
      do i = 1, size(this%best_1_Vs) - 1
         write (layer_csv%fh, '(A)') str(this%best_1_Vs(i)) + ", "+str(-sum(this%best_1_Thickness(:i))) + ", "+ &
            str(this%best_1_Vp(i)) + ", "+ &
            str(this%best_1_Density(i))
         write (layer_csv%fh, '(A)') str(this%best_1_Vs(i + 1)) + ", "+str(-sum(this%best_1_Thickness(:i))) + &
            ","+str(this%best_1_Vp(i + 1)) + ","+ &
            str(this%best_1_Density(i + 1))
      end do
      write (layer_csv%fh, '(A)') str(this%best_1_Vs(size(this%best_1_Vs))) + ","+str(-sum(this%best_1_Thickness)) + ","+ &
         str(this%best_1_Vp(size(this%best_1_Vs))) + ","+ &
         str(this%best_1_Density(size(this%best_1_Vs)))
      call layer_csv%close()

      ! Parse structure data #2
      filename = this%best_2(:len(this%best_2) - 20) + ".csv"
      print *, filename
      call layer_csv%open(filename, "r")
      do i = 1, f%numline(filename)
         read (layer_csv%fh, *) charbuf
         if (index(charbuf, "NL") /= 0) then
            read (layer_csv%fh, *) intbuf

            this%best_2_Vs = zeros(intbuf)
            this%best_2_Vp = zeros(intbuf)
            this%best_2_Density = zeros(intbuf)
            this%best_2_Thickness = zeros(intbuf)
            read (layer_csv%fh, *) charbuf
            do j = 1, size(this%best_2_Vs)
               read (layer_csv%fh, *) intbuf, readbuf(1:8)
               this%best_2_Vs(j) = readbuf(5)
               this%best_2_Vp(j) = readbuf(2)
               this%best_2_Density(j) = readbuf(1)
               this%best_2_Thickness(j) = readbuf(8)
            end do
            exit
         end if
      end do
      call layer_csv%close()

      this%best_2_layer_csv = name + "_best_2_Vs_Thickness_Vp_Density.csv"

      call layer_csv%open(this%best_2_layer_csv, "w")
      write (layer_csv%fh, '(A)') str(this%best_2_Vs(1)) + ", "+str(0.0d0) + ", "+str(this%best_2_Vp(1)) + ", "+ &
         str(this%best_2_Density(1))
      do i = 1, size(this%best_2_Vs) - 1
         write (layer_csv%fh, '(A)') str(this%best_2_Vs(i)) + ", "+str(-sum(this%best_2_Thickness(:i))) + ", "+ &
            str(this%best_2_Vp(i)) + ", "+ &
            str(this%best_2_Density(i))
         write (layer_csv%fh, '(A)') str(this%best_2_Vs(i + 1)) + ", "+str(-sum(this%best_2_Thickness(:i))) + &
            ","+str(this%best_2_Vp(i + 1)) + ","+ &
            str(this%best_2_Density(i + 1))
      end do
      write (layer_csv%fh, '(A)') str(this%best_2_Vs(size(this%best_2_Vs))) + ","+str(-sum(this%best_2_Thickness)) + ","+ &
         str(this%best_2_Vp(size(this%best_2_Vs))) + ","+ &
         str(this%best_2_Density(size(this%best_2_Vs)))
      call layer_csv%close()

      ! Parse structure data #3
      filename = this%best_3(:len(this%best_3) - 20) + ".csv"
      print *, filename
      call layer_csv%open(filename, "r")
      do i = 1, f%numline(filename)
         read (layer_csv%fh, *) charbuf
         if (index(charbuf, "NL") /= 0) then
            read (layer_csv%fh, *) intbuf

            this%best_3_Vs = zeros(intbuf)
            this%best_3_Vp = zeros(intbuf)
            this%best_3_Density = zeros(intbuf)
            this%best_3_Thickness = zeros(intbuf)
            read (layer_csv%fh, *) charbuf
            do j = 1, size(this%best_3_Vs)
               read (layer_csv%fh, *) intbuf, readbuf(1:8)
               this%best_3_Vs(j) = readbuf(5)
               this%best_3_Vp(j) = readbuf(2)
               this%best_3_Density(j) = readbuf(1)
               this%best_3_Thickness(j) = readbuf(8)
            end do
            exit
         end if
      end do
      call layer_csv%close()

      this%best_3_layer_csv = name + "_best_3_Vs_Thickness_Vp_Density.csv"

      call layer_csv%open(this%best_3_layer_csv, "w")
      write (layer_csv%fh, '(A)') str(this%best_3_Vs(1)) + ", "+str(0.0d0) + ", "+str(this%best_3_Vp(1)) + ", "+ &
         str(this%best_3_Density(1))
      do i = 1, size(this%best_3_Vs) - 1
         write (layer_csv%fh, '(A)') str(this%best_3_Vs(i)) + ", "+str(-sum(this%best_3_Thickness(:i))) + ", "+ &
            str(this%best_3_Vp(i)) + ", "+ &
            str(this%best_3_Density(i))
         write (layer_csv%fh, '(A)') str(this%best_3_Vs(i + 1)) + ", "+str(-sum(this%best_3_Thickness(:i))) + &
            ","+str(this%best_3_Vp(i + 1)) + ","+ &
            str(this%best_3_Density(i + 1))
      end do
      write (layer_csv%fh, '(A)') str(this%best_3_Vs(size(this%best_3_Vs))) + ","+str(-sum(this%best_3_Thickness)) + ","+ &
         str(this%best_3_Vp(size(this%best_3_Vs))) + ","+ &
         str(this%best_3_Density(size(this%best_3_Vs)))
      call layer_csv%close()

      ! Parse structure data #4
      filename = this%best_4(:len(this%best_4) - 20) + ".csv"
      print *, filename
      call layer_csv%open(filename, "r")
      do i = 1, f%numline(filename)
         read (layer_csv%fh, *) charbuf
         if (index(charbuf, "NL") /= 0) then
            read (layer_csv%fh, *) intbuf

            this%best_4_Vs = zeros(intbuf)
            this%best_4_Vp = zeros(intbuf)
            this%best_4_Density = zeros(intbuf)
            this%best_4_Thickness = zeros(intbuf)
            read (layer_csv%fh, *) charbuf
            do j = 1, size(this%best_4_Vs)
               read (layer_csv%fh, *) intbuf, readbuf(1:8)
               this%best_4_Vs(j) = readbuf(5)
               this%best_4_Vp(j) = readbuf(2)
               this%best_4_Density(j) = readbuf(1)
               this%best_4_Thickness(j) = readbuf(8)
            end do
            exit
         end if
      end do
      call layer_csv%close()

      this%best_4_layer_csv = name + "_best_4_Vs_Thickness_Vp_Density.csv"

      call layer_csv%open(this%best_4_layer_csv, "w")
      write (layer_csv%fh, '(A)') str(this%best_4_Vs(1)) + ", "+str(0.0d0) + ", "+str(this%best_4_Vp(1)) + ", "+ &
         str(this%best_4_Density(1))
      do i = 1, size(this%best_4_Vs) - 1
         write (layer_csv%fh, '(A)') str(this%best_4_Vs(i)) + ", "+str(-sum(this%best_4_Thickness(:i))) + ", "+ &
            str(this%best_4_Vp(i)) + ", "+ &
            str(this%best_4_Density(i))
         write (layer_csv%fh, '(A)') str(this%best_4_Vs(i + 1)) + ", "+str(-sum(this%best_4_Thickness(:i))) + &
            ","+str(this%best_4_Vp(i + 1)) + ","+ &
            str(this%best_4_Density(i + 1))
      end do
      write (layer_csv%fh, '(A)') str(this%best_4_Vs(size(this%best_4_Vs))) + ","+str(-sum(this%best_4_Thickness)) + ","+ &
         str(this%best_4_Vp(size(this%best_4_Vs))) + ","+ &
         str(this%best_4_Density(size(this%best_4_Vs)))
      call layer_csv%close()

      ! Parse structure data #5
      filename = this%best_5(:len(this%best_5) - 20) + ".csv"
      print *, filename
      call layer_csv%open(filename, "r")
      do i = 1, f%numline(filename)
         read (layer_csv%fh, *) charbuf
         if (index(charbuf, "NL") /= 0) then
            read (layer_csv%fh, *) intbuf

            this%best_5_Vs = zeros(intbuf)
            this%best_5_Vp = zeros(intbuf)
            this%best_5_Density = zeros(intbuf)
            this%best_5_Thickness = zeros(intbuf)
            read (layer_csv%fh, *) charbuf
            do j = 1, size(this%best_5_Vs)
               read (layer_csv%fh, *) intbuf, readbuf(1:8)
               this%best_5_Vs(j) = readbuf(5)
               this%best_5_Vp(j) = readbuf(2)
               this%best_5_Density(j) = readbuf(1)
               this%best_5_Thickness(j) = readbuf(8)
            end do
            exit
         end if
      end do
      call layer_csv%close()

      this%best_5_layer_csv = name + "_best_5_Vs_Thickness_Vp_Density.csv"

      call layer_csv%open(this%best_5_layer_csv, "w")
      write (layer_csv%fh, '(A)') str(this%best_5_Vs(1)) + ", "+str(0.0d0) + ", "+str(this%best_5_Vp(1)) + ", "+ &
         str(this%best_5_Density(1))
      do i = 1, size(this%best_5_Vs) - 1
         write (layer_csv%fh, '(A)') str(this%best_5_Vs(i)) + ", "+str(-sum(this%best_5_Thickness(:i))) + ", "+ &
            str(this%best_5_Vp(i)) + ", "+ &
            str(this%best_5_Density(i))
         write (layer_csv%fh, '(A)') str(this%best_5_Vs(i + 1)) + ", "+str(-sum(this%best_5_Thickness(:i))) + &
            ","+str(this%best_5_Vp(i + 1)) + ","+ &
            str(this%best_5_Density(i + 1))
      end do
      write (layer_csv%fh, '(A)') str(this%best_5_Vs(size(this%best_5_Vs))) + ","+str(-sum(this%best_5_Thickness)) + ","+ &
         str(this%best_5_Vp(size(this%best_5_Vs))) + ","+ &
         str(this%best_5_Density(size(this%best_5_Vs)))
      call layer_csv%close()

      this%inversion_is_done = .true.

   end subroutine

end module
