module SPACClass
    use iso_fortran_env
    use MathClass
    use ArrayClass
    use IOClass
    implicit none

    type :: SPAC_
        real(real64) :: sampling_Hz
        real(real64),allocatable :: observation(:,:)
    contains

    end type 


contains

function to_HoverV_spectra(H,V,FFT_SIZE) result(HoverV)
    real(real64),intent(in) :: H(:),V(:)
    integer(int32),intent(in) :: FFT_SIZE
    real(real64),allocatable :: HoverV(:)

    HoverV = sqrt(dble(to_CROSS_SPECTRUM(H,H,FFT_SIZE)))/sqrt(dble(to_CROSS_SPECTRUM(V,V,FFT_SIZE)))


end function

function to_CROSS_SPECTRUM(A,B,FFT_SIZE) result(Cross_Spectrum)
    real(real64),intent(in) :: A(:),B(:)

    integer(int32),intent(in) :: FFT_SIZE
    complex(real64),allocatable :: FFT_A(:),FFT_B(:),Cross_Spectrum(:),A_c(:),B_c(:)
    integer(int32) :: n,num_block,i,from,to

    Cross_Spectrum = zeros(FFT_SIZE)
    num_block = int(dble(size(A))/dble(FFT_SIZE*2) )
    from = 1
    A_c = A
    B_c = B
    do i=1,num_block
        from = (i-1)*FFT_SIZE*2 +1
        to   = i*FFT_SIZE*2
        FFT_A = FFT( A_c(from:to) )
        FFT_B = FFT( B_c(from:to) )
        FFT_A = FFT_A(1:FFT_SIZE)
        FFT_B = FFT_B(1:FFT_SIZE)
        Cross_Spectrum = Cross_Spectrum + FFT_A*conjg(FFT_B)
    enddo
    Cross_Spectrum = Cross_Spectrum/num_block

end function


function to_CCF(A,B,FFT_SIZE) result(CCF)
    real(real64),intent(in) :: A(:),B(:)

    integer(int32),intent(in) :: FFT_SIZE
    complex(real64),allocatable :: E_C_AB(:),E_C_AA(:),E_C_BB(:),CCF(:)
    integer(int32) :: n,num_block,i,from,to

    E_C_AB = to_CROSS_SPECTRUM(A,B,FFT_SIZE)
    E_C_AA = to_CROSS_SPECTRUM(A,A,FFT_SIZE)
    E_C_BB = to_CROSS_SPECTRUM(B,B,FFT_SIZE)
    
    CCF = dble(E_C_AB)/sqrt(E_C_AA)/sqrt(E_C_BB)
    
end function


function to_SPAC_COEFF(Center_x,Circle_x,FFT_SIZE) result(rho)
    real(real64),intent(in) :: Center_x(:),Circle_x(:,:) !,Angle(:)
    real(real64),allocatable :: rho(:),phi(:)
    real(real64) :: delta_angle
    integer(int32),intent(in) :: FFT_SIZE
    integer(int32) :: i,NUM_SAMPLE
    type(Math_) ::math

    NUM_SAMPLE = size(Circle_x,2)

    ! Specification for Observation:
    ! Center_x(:)      = time series of center logger (r=0)
    ! Circle_x(:,i)    = time series of i-th logger
    ! Angle(i)         = angle of i-th logger
    rho = zeros(FFT_SIZE)
    !allocate(phi(0:size(Angle) ) )
    !phi(0) = 0
    !phi(1:) = Angle(:)
    do i=1, NUM_SAMPLE
        ! rho(r, omega) 
        !delta_angle = phi(i) - phi(i-1)
        !rho(:) = rho(:) + dble(to_CCF(Center_x,Circle_x(:,i),FFT_SIZE=FFT_SIZE ))*radian(delta_angle)
        rho(:) = rho(:) + dble(to_CCF(Center_x,Circle_x(:,i),FFT_SIZE=FFT_SIZE ))
    enddo
    !rho(:) = rho(:)/2.0d0/math%pi/NUM_SAMPLE
    rho(:) = rho(:)/NUM_SAMPLE
    


end function

! ##############################################################
function to_phase_velocity(Center_x,Circle_x,FFT_SIZE,radius, sampling_Hz,debug,&
    learning_rate,tolerance,max_iter,initial_phase_velocity,wave_type) result(c)
    real(real64),intent(in) :: Center_x(:),Circle_x(:,:),radius
    real(real64),allocatable :: rho(:),c(:),freq(:),k(:)
    character(*),optional,intent(in) :: wave_type
    character(:),allocatable :: target_wave_type
    logical,optional,intent(in) :: debug

    real(real64),optional,intent(in) :: learning_rate,tolerance,initial_phase_velocity
    integer(int32),optional,intent(in) :: max_iter

    real(real64) :: residual,tangent_value,epsilon_val,tol,rf,rb
    integer(int32),intent(in) :: FFT_SIZE,sampling_Hz
    integer(int32) :: i,NUM_SAMPLE,max_itr,itr
    type(Math_) ::math

    if(present(wave_type) )then
        target_wave_type = wave_type
    else
        target_wave_type = "Rayleigh"
    endif

    if(index(target_wave_type,"Rayleigh")/=0)then

        rho = to_SPAC_COEFF(Center_x=Center_x,Circle_x=Circle_x,FFT_SIZE=FFT_SIZE)

        ! fitting by gradient descent method
        freq = to_frequency_axis(FFT_SIZE=FFT_SIZE,sampling_Hz=sampling_Hz)
        c = input(default=100.0d0,option=initial_phase_velocity)*eyes(FFT_SIZE)
        
        k = zeros(FFT_SIZE)
        k = freq*2.0d0*math%PI/c
        
        max_itr = input(default=1000,option=max_iter)
        epsilon_val=input(default=dble(1.0e-2),option=learning_rate)
        tol  = input(default=dble(1.0e-4),option=tolerance)

        do i=1,FFT_SIZE
            ! gradient descent
            do itr=1,max_itr
                residual = (rho(i) - Bessel_J0(radius*k(i) ) )**2
                if(abs(residual) < tol)then
                    c(i) = freq(i)*2.0d0*math%PI/k(i)
                    exit
                elseif(i==max_itr)then
                    c(i) = freq(i)*2.0d0*math%PI/k(i)
                    exit
                endif
                rf = (rho(i) - Bessel_J0(radius*(k(i)+epsilon_val) ) )**2
                rb = (rho(i) - Bessel_J0(radius*(k(i)-epsilon_val) ) )**2
                tangent_value = &
                    (rf - rb )/ &
                    (2.0d0*epsilon_val)
                if(debug)then
                    print *, i,itr,residual,k(i),rho(i) , Bessel_J0(radius*k(i) )
                endif
                k(i) = k(i) - epsilon_val*tangent_value

            enddo
        enddo
    else
        print *, "[ERROR] to_phase_velocity >> only for Rayleigh wave"
    endif

end function
! ##############################################################

function to_frequency_axis(FFT_SIZE,sampling_Hz) result(f_axis)
    integer(int32),intent(in) :: FFT_SIZE,sampling_Hz
    real(real64),allocatable:: f_axis(:)

    f_axis = linspace( [ sampling_Hz/2.0d0/FFT_SIZE , sampling_Hz/2.0d0],FFT_SIZE )

end function

function to_time_axis(sampling_Hz,NUM_SAMPLE) result(t_axis)
    integer(int32),intent(in) :: sampling_Hz,NUM_SAMPLE
    real(real64),allocatable:: t_axis(:)

    t_axis = linspace( [ 0.0d0 , sampling_Hz*NUM_SAMPLE*1.0d0],NUM_SAMPLE )

end function


function moving_average(indata) result(outdata)
    real(real64),intent(in) :: indata(:)
    real(real64),allocatable :: outdata(:)
    integer(int32) :: n,i

    n = size(indata)
    outdata = zeros(n)
    do i=2,n-1
        outdata(i) = (indata(i-1) + indata(i) + indata(i+1))/3
    enddo

end function

end module