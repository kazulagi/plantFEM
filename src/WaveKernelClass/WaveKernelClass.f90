module WaveKernelClass
    use SparseClass
    use FEMDomainClass
    use SpectreAnalysisClass
    implicit none

    type :: WaveKernel_
        type(CRS_) :: OmegaSqMatrix
        real(Real64),allocatable :: Mmatrix_diag(:)
        real(Real64),allocatable :: DampingRatio(:)
        real(real64) :: tol = dble(1.0e-25)
        real(real64),allocatable :: v_in1(:)
        real(real64),allocatable :: v_in2(:)
        real(real64),allocatable :: v_out1(:)
        real(real64),allocatable :: v_out2(:)
        real(real64),allocatable :: u_in1(:)
        real(real64),allocatable :: u_in2(:)
        real(real64),allocatable :: u_out1(:)
        real(real64),allocatable :: u_out2(:)
        real(real64),allocatable :: hanning_buffer(:,:)
        integer(int32) :: itrmax=100
    contains
        procedure :: init => initWaveKernel
        procedure :: bandpass => bandpass_WaveKernel
        procedure :: lowpass => lowpass_WaveKernel
        procedure :: movingAverage => movingAverage_WaveKernel

        ! >>> RECCOMENDED >>>
        procedure :: getDisplacement_and_Velocity &
            => getDisplacement_and_Velocity_WaveKernel
        ! <<< RECCOMENDED <<<

        procedure :: getDisplacement => getDisplacementWaveKernel
        procedure :: getVelocity => getVelocityWaveKernel

    end type

contains

subroutine initWaveKernel(this,FEMDomain,DOF,YoungModulus,PoissonRatio,&
        DampingRatio,Density)
    class(WaveKernel_),intent(inout) :: this
    integer(int32),intent(in) :: DOF
    type(FEMDomain_),intent(inout) :: FEMDomain
    real(real64),intent(in) :: YoungModulus(:),Density(:)
    real(real64),optional,intent(in) ::  PoissonRatio(:),DampingRatio(:)
    type(CRS_) :: Imatrix,Mmatrix,Cmatrix
    

    Mmatrix = FEMDomain%MassMatrix(DOF=DOF,Density=Density)
    
    if(DOF==1)then
        this%OmegaSqMatrix = FEMDomain%StiffnessMatrix( &
            YoungModulus=YoungModulus)
    else
        if(.not.present(PoissonRatio) )then
            print *, "[initWaveKernel] Please input PoissonRatio"
            stop
        endif    
        this%OmegaSqMatrix = FEMDomain%StiffnessMatrix( &
            YoungModulus=YoungModulus,PoissonRatio=PoissonRatio)
    endif
    this%Mmatrix_diag = Mmatrix%diag(cell_centered=.true.) 
    this%OmegaSqMatrix = this%OmegaSqMatrix%divide_by(this%Mmatrix_diag )
    
    this%DampingRatio = zeros( this%OmegaSqMatrix%size() )
    
    if(present(DampingRatio) )then
        !call Imatrix%eyes(this%OmegaSqMatrix%size() )
        Cmatrix = to_diag(DampingRatio*DampingRatio) ! vector to diagonal matrix
        this%OmegaSqMatrix = this%OmegaSqMatrix - Cmatrix
        this%DampingRatio = DampingRatio
    endif





end subroutine initWaveKernel
! ##############################################################

! ##############################################################
subroutine bandpass_WaveKernel(this,u_n, v_n,freq_range,dt,fix_idx)
    class(WaveKernel_),intent(inout) :: this
    real(real64),intent(inout) :: u_n(:),v_n(:)
    real(real64),intent(in) :: freq_range(1:2)
    integer(int32),optional,intent(in) :: fix_idx(:)
    real(real64),allocatable :: u(:)
    real(real64),intent(in) :: dt
    integer(int32) :: j
    real(real64) :: h

    
    if(.not.allocated(this%u_in1) )then
        this%u_in1 = u_n
        this%u_in2 = u_n
        this%u_out1 = u_n
        this%u_out2 = u_n
    endif

    if(.not.allocated(this%v_in1) )then
        this%v_in1 = v_n
        this%v_in2 = v_n
        this%v_out1 = v_n
        this%v_out2 = v_n
    endif
    
    u_n(fix_idx)=0.0d0
    !$OMP parallel do
    do j=1,size(u_n)
        u_n(j) = bandpass_filter(x=u_n(j),&
            freq_range=freq_range,&
            sampling_Hz=1.0d0/dt,&
            in1=this%u_in1(j),&
            in2=this%u_in2(j),&
            out1=this%u_out1(j),&
            out2=this%u_out2(j) &
            )
    enddo
    !$OMP end parallel do

    v_n(fix_idx)=0.0d0
    !$OMP parallel do
    do j=1,size(v_n)
        v_n(j) = bandpass_filter(x=v_n(j),&
            freq_range=freq_range,&
            sampling_Hz=1.0d0/dt,&
            in1=this%v_in1(j),&
            in2=this%v_in2(j),&
            out1=this%v_out1(j),&
            out2=this%v_out2(j) &
            )
    enddo
    !$OMP end parallel do
    
    

end subroutine
! ##############################################################



! ##############################################################
subroutine lowpass_WaveKernel(this,u_n, v_n,cutoff_frequency,dt,fix_idx)
    class(WaveKernel_),intent(inout) :: this
    real(real64),intent(inout) :: u_n(:),v_n(:)
    real(real64),intent(in) :: cutoff_frequency
    integer(int32),optional,intent(in) :: fix_idx(:)
    real(real64),allocatable :: u(:)
    real(real64),intent(in) :: dt
    integer(int32) :: j
    real(real64) :: h

    
    if(.not.allocated(this%u_in1) )then
        this%u_in1 = u_n
    endif

    if(.not.allocated(this%v_in1) )then
        this%v_in1 = v_n
    endif
    
    u_n(fix_idx)=0.0d0
    !$OMP parallel do
    do j=1,size(u_n)
        u_n(j) = lowpass_filter(x_n=u_n(j),&
            fc=cutoff_frequency,&
            sampling_Hz=1.0d0/dt,&
            buf=this%u_in1(j) &
            )
    enddo
    !$OMP end parallel do

    v_n(fix_idx)=0.0d0
    !$OMP parallel do
    do j=1,size(v_n)
        v_n(j) = lowpass_filter(x_n=v_n(j),&
            fc=cutoff_frequency,&
            sampling_Hz=1.0d0/dt,&
            buf=this%v_in1(j) &
            )
    enddo
    !$OMP end parallel do
    
    

end subroutine
! ##############################################################








! ##############################################################
subroutine movingAverage_WaveKernel(this,u_n, v_n,fix_idx)
    class(WaveKernel_),intent(inout) :: this
    real(real64),intent(inout) :: u_n(:),v_n(:)
    integer(int32),optional,intent(in) :: fix_idx(:)
    real(real64),allocatable :: u(:)
    integer(int32) :: j
    real(real64) :: h

    
    if(.not.allocated(this%u_in1) )then
        this%u_in1 = u_n
        this%u_in2 = u_n
        this%u_out1 = u_n
        this%u_out2 = u_n
    endif

    if(.not.allocated(this%v_in1) )then
        this%v_in1 = v_n
        this%v_in2 = v_n
        this%v_out1 = v_n
        this%v_out2 = v_n
    endif
    u_n(fix_idx)=0.0d0
    !$OMP parallel do
    do j=1,size(u_n)
        u_n(j) = movingAverage_filter(x=u_n(j),&
            in1=this%u_in1(j),&
            in2=this%u_in2(j) &
            )
    enddo
    !$OMP end parallel do

    v_n(fix_idx)=0.0d0
    !$OMP parallel do
    do j=1,size(v_n)
        v_n(j) = movingAverage_filter(x=v_n(j),&
            in1=this%v_in1(j),&
            in2=this%v_in2(j) &
            )
    enddo
    !$OMP end parallel do
    
    

end subroutine
! ##############################################################



! ##############################################################
subroutine getDisplacement_and_Velocity_WaveKernel(this,u_n,v_n,dt,&
    fix_idx,cutoff_frequency,debug_mode,u,v,RHS) 
    class(WaveKernel_),intent(inout) :: this
    real(real64),intent(in) :: u_n(:),v_n(:)
    real(real64),optional,intent(in) :: RHS(:)
    real(real64),allocatable :: du(:),dv(:)
    real(real64),allocatable :: u(:),v(:)
    integer(int32),optional,allocatable,intent(in) :: fix_idx(:)
    real(real64),intent(in) :: dt
    logical,optional,intent(in) :: debug_mode
    real(real64),optional,intent(in)  :: cutoff_frequency
    integer(int32) :: j,k
    
    
    

    ! [CAUTION!!] only undamped is implemented.
    
    du = u_n
    
    u = u_n
    v = 0.0d0*v_n
    do k=1,this%itrmax
        du = this%OmegaSqMatrix%matmul(du)
        if(norm(LPF_cos_sqrt_taylor_coefficient(  k=k  ,t=dt,f_c=cutoff_frequency)*du)&
            < this%tol )exit
        u = u + LPF_cos_sqrt_taylor_coefficient(  k=k  ,t=dt,f_c=cutoff_frequency)*du
        
        if(k==1)then
            v = - dt*du
        else
            v = v - LPF_t_sinc_sqrt_taylor_coefficient(k=k-1,t=dt,f_c=cutoff_frequency)*du
        endif
        
    enddo
    deallocate(du)

    dv = v_n
    ! k=1
    u = u + dt*dv
    v = v + dv
    do k=1,this%itrmax
        dv = this%OmegaSqMatrix%matmul(dv)
        if(norm(LPF_cos_sqrt_taylor_coefficient(  k=k  ,t=dt,f_c=cutoff_frequency)*dv)&
            < this%tol )exit
        u = u + LPF_t_sinc_sqrt_taylor_coefficient(k=k ,t=dt,f_c=cutoff_frequency)*dv
        v = v + LPF_cos_sqrt_taylor_coefficient(  k=k ,t=dt,f_c=cutoff_frequency)*dv
    enddo
    
    if(present(RHS) )then
        u = u + this%OmegaSqMatrix%tensor_wave_kernel_RHS(RHS=RHS/this%Mmatrix_diag,t=dt,tol=dble(1.0e-25),&
            itrmax=this%itrmax,cutoff_frequency=cutoff_frequency)
        v = v + LPF_damped_t_sinc_sqrt_WaveKernelFunction(Omega_sq_matrix=this%OmegaSqMatrix,&
            dt=dt, f_c=cutoff_frequency, v_n=RHS/this%Mmatrix_diag , &
            DampingRatio=this%DampingRatio,itrmax=this%itrmax,tol=this%tol)
    endif

    if(present(fix_idx) )then
        if(allocated(fix_idx) )then
            u(fix_idx)=0.0d0 
        endif
    endif


end subroutine
! ##############################################################



! ##############################################################
function getDisplacementWaveKernel(this,u_n,v_n,dt,fix_idx,cutoff_frequency,debug_mode,RHS) result(u)
    class(WaveKernel_),intent(inout) :: this
    real(real64),intent(in) :: u_n(:),v_n(:)
    real(real64),optional,intent(in) :: RHS(:)
    real(real64),allocatable :: u(:)
    integer(int32),optional,intent(in) :: fix_idx(:)
    real(real64),intent(in) :: dt
    logical,optional,intent(in) :: debug_mode
    real(real64),optional,intent(in)  :: cutoff_frequency
    integer(int32) :: j
    real(real64) :: h, ddt

    
    if(present(cutoff_frequency) )then
!        if(present(debug_mode) )then
!            if(debug_mode)then
!                ddt = 1.0d0/(cutoff_frequency*4.0d0)
!                ! Hanning Window
!                
!                u =  0.250d0*exp(-this%DampingRatio/2.0d0*(dt-ddt) )*this%OmegaSqMatrix%tensor_wave_kernel(&
!                        u0=u_n,v0=v_n,t=dt-ddt,&
!                        itrmax=this%itrmax,tol=this%tol,fix_idx=fix_idx)&
!                    + 0.50d0*exp(-this%DampingRatio/2.0d0*(dt) )*this%OmegaSqMatrix%tensor_wave_kernel(&
!                        u0=u_n,v0=v_n,t=dt,&
!                        itrmax=this%itrmax,tol=this%tol,fix_idx=fix_idx)&
!                    + 0.250d0*exp(-this%DampingRatio/2.0d0*(dt-ddt) )*this%OmegaSqMatrix%tensor_wave_kernel(&
!                        u0=u_n,v0=v_n,t=dt+ddt,&
!                        itrmax=this%itrmax,tol=this%tol,fix_idx=fix_idx)        
!            return
!            endif
!        endif

        u =   LPF_Damped_cos_sqrt_WaveKernelFunction(Omega_sq_matrix=this%OmegaSqMatrix,&
                dt=dt, f_c=cutoff_frequency, u_n=u_n,DampingRatio=this%DampingRatio,&
                fix_idx=fix_idx, itrmax=this%itrmax,tol=this%tol) &
            + LPF_Damped_t_sinc_sqrt_WaveKernelFunction(Omega_sq_matrix=this%OmegaSqMatrix,&
                dt=dt, f_c=cutoff_frequency, v_n=v_n,DampingRatio=this%DampingRatio,&
                fix_idx=fix_idx, itrmax=this%itrmax,tol=this%tol)
        ! cutoff = - 10 dB
        

    else
        u = this%OmegaSqMatrix%tensor_wave_kernel(&
            u0=u_n,v0=v_n,t=dt,h=0.0d0,&
            itrmax=this%itrmax,tol=this%tol,fix_idx=fix_idx)
        u = exp(-this%DampingRatio*dt )*u
    endif


    if(present(RHS) )then
        u = u + this%OmegaSqMatrix%tensor_wave_kernel_RHS(RHS=RHS/this%Mmatrix_diag,t=dt,tol=dble(1.0e-25),&
            itrmax=this%itrmax,cutoff_frequency=cutoff_frequency)
    endif

end function
! ##############################################################


! ##############################################################
function getVelocityWaveKernel(this,u_n, v_n,RHS,dt,fix_idx,cutoff_frequency,debug_mode) result(v)
    class(WaveKernel_),intent(inout) :: this
    real(real64),intent(in) :: u_n(:),v_n(:)
    real(real64),optional,intent(in) :: RHS(:)
    integer(int32),optional,intent(in) :: fix_idx(:)
    real(real64),optional,intent(in)  :: cutoff_frequency
    logical,optional,intent(in) :: debug_mode
    real(real64),allocatable :: v(:)
    real(real64),intent(in) :: dt
    integer(int32) :: j
    real(real64) :: h,ddt

    if(present(cutoff_frequency) )then
!        if(present(debug_mode)  )then
!            if( debug_mode )then    
!                ! cutoff = - 3 dB
!                ddt = 1.0d0/(cutoff_frequency*4.0d0)
!        
!                v =   0.250d0*this%OmegaSqMatrix%tensor_wave_kernel(&
!                        u0=-h*(dt-ddt)*u_n+v_n, &
!                        v0=-this%OmegaSqMatrix%matmul(u_n)-h*(dt-ddt)*v_n, &
!                        h=h,t=(dt-ddt),itrmax=this%itrmax,tol=this%tol,fix_idx=fix_idx)&
!                    + 0.500d0*this%OmegaSqMatrix%tensor_wave_kernel(&
!                        u0=-h*dt*u_n+v_n, &
!                        v0=-this%OmegaSqMatrix%matmul(u_n)-h*dt*v_n, &
!                        h=h,t=dt,itrmax=this%itrmax,tol=this%tol,fix_idx=fix_idx)&
!                    + 0.250d0*this%OmegaSqMatrix%tensor_wave_kernel(&
!                        u0=-h*(dt+ddt)*u_n+v_n, &
!                        v0=-this%OmegaSqMatrix%matmul(u_n)-h*(dt+ddt)*v_n, &
!                        h=h,t=(dt+ddt),itrmax=this%itrmax,tol=this%tol,fix_idx=fix_idx)
!
!                return
!            endif
!        endif
!

!        if( this%DampingRatio/=0.0d0 )then    
!            ! cutoff = - 3 dB
!            ddt = 1.0d0/(cutoff_frequency*4.0d0)
!    
!            v =   0.250d0*this%OmegaSqMatrix%tensor_wave_kernel(&
!                    u0=-h*(dt-ddt)*u_n+v_n, &
!                    v0=-this%OmegaSqMatrix%matmul(u_n)-h*(dt-ddt)*v_n, &
!                    h=h,t=(dt-ddt),itrmax=this%itrmax,tol=this%tol,fix_idx=fix_idx)&
!                + 0.500d0*this%OmegaSqMatrix%tensor_wave_kernel(&
!                    u0=-h*dt*u_n+v_n, &
!                    v0=-this%OmegaSqMatrix%matmul(u_n)-h*dt*v_n, &
!                    h=h,t=dt,itrmax=this%itrmax,tol=this%tol,fix_idx=fix_idx)&
!                + 0.250d0*this%OmegaSqMatrix%tensor_wave_kernel(&
!                    u0=-h*(dt+ddt)*u_n+v_n, &
!                    v0=-this%OmegaSqMatrix%matmul(u_n)-h*(dt+ddt)*v_n, &
!                    h=h,t=(dt+ddt),itrmax=this%itrmax,tol=this%tol,fix_idx=fix_idx)
!            return
!        endif
        

        v =   LPF_damped_cos_sqrt_WaveKernelFunction(Omega_sq_matrix=this%OmegaSqMatrix,&
                dt=dt, f_c=cutoff_frequency, u_n=v_n,DampingRatio=this%DampingRatio,&
                fix_idx=fix_idx, itrmax=this%itrmax,tol=this%tol) &
            + LPF_damped_t_sinc_sqrt_WaveKernelFunction(Omega_sq_matrix=this%OmegaSqMatrix,&
                dt=dt, f_c=cutoff_frequency, v_n=-this%OmegaSqMatrix%matmul(u_n), &
                DampingRatio=this%DampingRatio,fix_idx=fix_idx,itrmax=this%itrmax,tol=this%tol)

    else
        v = exp(-this%DampingRatio*dt )*this%OmegaSqMatrix%tensor_wave_kernel(&
            u0=-this%DampingRatio*dt*u_n+v_n, &
            v0=-this%OmegaSqMatrix%matmul(u_n)-this%DampingRatio*dt*v_n, &
            h=0.0d0,t=dt,itrmax=this%itrmax,tol=this%tol,fix_idx=fix_idx)
    endif


    if(present(RHS) )then
        v = v + LPF_damped_t_sinc_sqrt_WaveKernelFunction(Omega_sq_matrix=this%OmegaSqMatrix,&
            dt=dt, f_c=cutoff_frequency, v_n=RHS/this%Mmatrix_diag , &
            DampingRatio=this%DampingRatio,itrmax=this%itrmax,tol=this%tol)
    endif
end function
! ##############################################################



! ##############################################################
!subroutine hanning_WaveKernel(this,u_n, v_n,cutoff_frequency,dt,fix_idx)
!    class(WaveKernel_),intent(inout) :: this
!    real(real64),intent(inout) :: u_n(:),v_n(:)
!    real(real64),intent(in) :: cutoff_frequency
!    integer(int32),optional,intent(in) :: fix_idx(:)
!    real(real64),allocatable :: u(:)
!    real(real64),intent(in) :: dt
!    integer(int32) :: j
!    real(real64) :: h
!
!    if(.not.allocated(this%hanning_buffer) )then
!        this%hanning_buffer = zeros(size(u_n),this%hanning_buffer_size )
!        do j=1,this%hanning_buffer_size
!            this%hanning_buffer(:,j) = u_n(:)
!        enddo
!    endif
!
!    
!    if(.not.allocated(this%u_in1) )then
!        this%u_in1 = u_n
!    endif
!
!    if(.not.allocated(this%v_in1) )then
!        this%v_in1 = v_n
!    endif
!    
!    u_n(fix_idx)=0.0d0
!    !$OMP parallel do
!    do j=1,size(u_n)
!        u_n(j) = hanning_filter(x_n=u_n(j),&
!            fc=cutoff_frequency,&
!            sampling_Hz=1.0d0/dt,&
!            buf=this%hanning_buffer(:,j) &
!            )
!    enddo
!    !$OMP end parallel do
!
!    v_n(fix_idx)=0.0d0
!    !$OMP parallel do
!    do j=1,size(v_n)
!        v_n(j) = hanning_filter(x_n=v_n(j),&
!            fc=cutoff_frequency,&
!            sampling_Hz=1.0d0/dt,&
!            buf=this%hanning_buffer(:,j) &
!            )
!    enddo
!    !$OMP end parallel do
!    
!    
!
!end subroutine
!! ##############################################################
!
!function hanning_filter_wavekernel(x_n,fc,sampling_Hz,buf) result(ret)
!    real(real64),intent(in) :: x_n,fc,sampling_Hz,buf(:)
!    real(real64) :: ret
!    real(real64) :: t,period_T,dt
!    integer(int32) :: i
!    type(Math_) :: math
!
!    ret = 0.0d0*x_n
!    dt = sampling_Hz
!    t = - dt*size(buf,1)
!    period_T = 1.0d0/fc
!    do i=1,size(buf,1)
!        t = t + dt
!        ret = ret + buf(i)*(0.50d0 - 0.50d0*cos(2.0d0*math%pi*t/period_T) )
!    enddo
!
!end function

pure function LPF_cos_sqrt_taylor_coefficient(k,t,f_c) result(c_k)
    integer(int32),intent(in) :: k
    real(real64),intent(in) :: t
    real(real64),intent(in) :: f_c
    real(real64) :: c_k
    real(real64) :: t_hat,dt

    if(k==0)then
        c_k = 1.0d0
        return
    endif
    
    dt = 1.0d0/(f_c*4.0d0)
    t_hat = 0.250d0*((t-dt)**(2*k) ) + 0.50d0*((t)**(2*k) ) + 0.250d0*((t+dt)**(2*k) )

    if(mod(k,2)==0 )then
        ! k=even
        c_k = t_hat/Gamma(2.0d0*k+1.0d0)
    else
        ! k=odd
        c_k = -1.0d0*t_hat/Gamma(2.0d0*k+1.0d0)
    endif
    
end function

pure function LPF_damped_cos_sqrt_taylor_coefficient(k,t,f_c,DampingRatio) result(c_k)
    integer(int32),intent(in) :: k
    real(real64),intent(in) :: t
    real(real64),intent(in) :: f_c
    real(real64),intent(in) :: DampingRatio(:)
    real(real64) :: dt
    real(real64),allocatable:: c_k(:)
    real(real64),allocatable:: t_tilde(:)

    if(k==0)then
        c_k = ones(size(DampingRatio) )
        return
    endif
    t_tilde = zeros(size(DampingRatio) ) 
    
    
    dt = 1.0d0/(f_c*4.0d0)
    t_tilde =   0.250d0*((t-dt)**(2*k) )*exp(-DampingRatio*(t-dt) ) &
            + 0.500d0*((t   )**(2*k) )*exp(-DampingRatio*(t   ) ) &
            + 0.250d0*((t+dt)**(2*k) )*exp(-DampingRatio*(t+dt) )
    
    if(mod(k,2)==0 )then
        ! k=even
        c_k = t_tilde/Gamma(2.0d0*k+1.0d0)
    else
        ! k=odd
        c_k = -1.0d0*t_tilde/Gamma(2.0d0*k+1.0d0)
    endif
    
end function




pure function LPF_t_sinc_sqrt_taylor_coefficient(k,t,f_c) result(s_k)
    integer(int32),intent(in) :: k
    real(real64),intent(in) :: t
    real(real64),intent(in) :: f_c
    real(real64) :: s_k
    real(real64) :: t_hat,dt
    type(Math_) :: math

    if(k==0)then
        s_k = t
        return
    endif
    
    dt = 1.0d0/(f_c*4.0d0)

    t_hat = 0.250d0*((t-dt)**(2*k+1) ) + 0.50d0*((t)**(2*k+1) ) &
        + 0.250d0*((t+dt)**(2*k+1) )
    
    if(mod(k,2)==0 )then
        ! k=even
        s_k = (2.0d0**(-1-2*k))*sqrt(math%pi)/Gamma(1.0d0+dble(k))/Gamma(1.5d0+dble(k))&
            *t_hat
    else
        ! k=odd
        s_k = -1.0d0*&
              (2.0d0**(-1-2*k))*sqrt(math%pi)/Gamma(1.0d0+dble(k))/Gamma(1.5d0+dble(k))&
            *t_hat
    endif
    
end function





pure function LPF_damped_t_sinc_sqrt_taylor_coefficient(k,t,f_c,DampingRatio) result(s_k)
    integer(int32),intent(in) :: k
    real(real64),intent(in) :: t
    real(real64),intent(in) :: f_c
    real(real64),allocatable :: s_k(:)
    real(real64),allocatable :: t_tilde(:)
    real(real64) :: dt
    real(real64),intent(in) :: DampingRatio(:)
    type(Math_) :: math

    if(k==0)then
        s_k = t*ones(size(DampingRatio) )
        return
    endif
    t_tilde = zeros(size(DampingRatio) ) 
    
    dt = 1.0d0/(f_c*4.0d0)

    t_tilde =   0.250d0*((t-dt)**(2*k+1) )*exp(-DampingRatio*(t-dt) ) &
              + 0.500d0*((t   )**(2*k+1) )*exp(-DampingRatio*(t   ) ) &
              + 0.250d0*((t+dt)**(2*k+1) )*exp(-DampingRatio*(t+dt) )
    
    if(mod(k,2)==0 )then
        ! k=even
        s_k = (2.0d0**(-1-2*k))*sqrt(math%pi)/Gamma(1.0d0+dble(k))/Gamma(1.5d0+dble(k))&
            *t_tilde
    else
        ! k=odd
        s_k = -1.0d0*&
              (2.0d0**(-1-2*k))*sqrt(math%pi)/Gamma(1.0d0+dble(k))/Gamma(1.5d0+dble(k))&
            *t_tilde
    endif
    
end function


! #########################################################
function LPF_cos_sqrt_WaveKernelFunction(Omega_sq_matrix,dt,f_c,u_n,itrmax,tol,fix_idx) result(ret)
    type(CRS_),intent(inout) :: Omega_sq_matrix
    real(real64),intent(in) :: dt, f_c, u_n(:),tol
    real(real64),allocatable :: ret(:),du(:)

    integer(int32),intent(in) :: itrmax,fix_idx(:)
    integer(int32) :: k

    k=0
    du = u_n
    du(fix_idx) = 0.0d0
    ret = LPF_cos_sqrt_taylor_coefficient(k=k,t=dt,f_c=f_c)*du
    
    do k=1,itrmax
        du = Omega_sq_matrix%matmul(du)  
        if(norm(LPF_cos_sqrt_taylor_coefficient(k=k,t=dt,f_c=f_c)*du)<tol )exit  
        ret = ret + LPF_cos_sqrt_taylor_coefficient(k=k,t=dt,f_c=f_c)*du
    enddo

end function
! #########################################################



! #########################################################
function LPF_damped_cos_sqrt_WaveKernelFunction(Omega_sq_matrix,dt,f_c,u_n,DampingRatio, itrmax,tol,fix_idx) result(ret)
    type(CRS_),intent(inout) :: Omega_sq_matrix
    real(real64),intent(in) :: dt, f_c, u_n(:),tol,DampingRatio(:)
    real(real64),allocatable :: ret(:),du(:)

    integer(int32),intent(in) :: itrmax
    integer(int32),optional,intent(in) :: fix_idx(:)
    integer(int32) :: k

    k=0
    du = u_n
    if(present(fix_idx) )then
        du(fix_idx) = 0.0d0
    endif
    ret = LPF_damped_cos_sqrt_taylor_coefficient(k=k,t=dt,f_c=f_c,DampingRatio=DampingRatio)*du
    
    do k=1,itrmax
        du = Omega_sq_matrix%matmul(du)  
        if(norm(LPF_damped_cos_sqrt_taylor_coefficient(k=k,t=dt,f_c=f_c,DampingRatio=DampingRatio)*du)<tol )exit  
        ret = ret + LPF_damped_cos_sqrt_taylor_coefficient(k=k,t=dt,f_c=f_c,DampingRatio=DampingRatio)*du
    enddo

end function
! #########################################################

! #########################################################
function LPF_t_sinc_sqrt_WaveKernelFunction(Omega_sq_matrix,dt,f_c,v_n,itrmax,tol,fix_idx) result(ret)
    type(CRS_),intent(inout) :: Omega_sq_matrix
    real(real64),intent(in) :: dt, f_c, v_n(:),tol
    real(real64),allocatable :: ret(:),dv(:)
    integer(int32),intent(in) :: itrmax,fix_idx(:)
    integer(int32) :: k

    k=0
    dv = v_n
    dv(fix_idx) = 0.0d0
    ret = LPF_t_sinc_sqrt_taylor_coefficient(k=k,t=dt,f_c=f_c)*dv
    
    do k=1,itrmax
        dv = Omega_sq_matrix%matmul(dv)    
        if(norm(LPF_t_sinc_sqrt_taylor_coefficient(k=k,t=dt,f_c=f_c)*dv)<tol )exit
        ret = ret + LPF_t_sinc_sqrt_taylor_coefficient(k=k,t=dt,f_c=f_c)*dv
    enddo

end function
! #########################################################


! #########################################################
function LPF_damped_t_sinc_sqrt_WaveKernelFunction(Omega_sq_matrix,dt,f_c,v_n,itrmax,tol,fix_idx,DampingRatio) result(ret)
    type(CRS_),intent(inout) :: Omega_sq_matrix
    real(real64),intent(in) :: dt, f_c, v_n(:),tol,DampingRatio(:)
    real(real64),allocatable :: ret(:),dv(:)
    integer(int32),intent(in) :: itrmax
    integer(int32),optional,intent(in) :: fix_idx(:)
    integer(int32) :: k

    k=0
    dv = v_n
    if(present(fix_idx) )then
        dv(fix_idx) = 0.0d0
    endif
    ret = LPF_damped_t_sinc_sqrt_taylor_coefficient(k=k,t=dt,f_c=f_c,DampingRatio=DampingRatio)*dv
    
    do k=1,itrmax
        dv = Omega_sq_matrix%matmul(dv)    
        if(norm(LPF_damped_t_sinc_sqrt_taylor_coefficient(k=k,t=dt,f_c=f_c,DampingRatio=DampingRatio)*dv)<tol )exit
        ret = ret + LPF_damped_t_sinc_sqrt_taylor_coefficient(k=k,t=dt,f_c=f_c,DampingRatio=DampingRatio)*dv
    enddo

end function
! #########################################################

end module

