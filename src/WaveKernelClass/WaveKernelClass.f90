module WaveKernelClass
    use SparseClass
    use FEMDomainClass
    use SpectreAnalysisClass
    implicit none

    type :: WaveKernel_
        type(CRS_) :: OmegaSqMatrix
        real(Real64) :: DampingRatio=0.0d0
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
        procedure :: getDisplacement => getDisplacementWaveKernel
        procedure :: getVelocity => getVelocityWaveKernel
        procedure :: bandpass => bandpass_WaveKernel
        procedure :: lowpass => lowpass_WaveKernel
        procedure :: movingAverage => movingAverage_WaveKernel
        !procedure :: hanning => hanning_WaveKernel
        ! procedure :: filter => filter_WaveKernel
    end type

    !interface hanning_filter
    !    module procedure hanning_filter_wavekernel
    !end interface

contains

subroutine initWaveKernel(this,FEMDomain,DOF,YoungModulus,PoissonRatio,&
        DampingRatio,Density)
    class(WaveKernel_),intent(inout) :: this
    integer(int32),intent(in) :: DOF
    type(FEMDomain_),intent(inout) :: FEMDomain
    real(real64),intent(in) :: YoungModulus(:),Density(:)
    real(real64),optional,intent(in) ::  PoissonRatio(:),DampingRatio
    type(CRS_) :: Imatrix,Mmatrix
    

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
    this%OmegaSqMatrix = this%OmegaSqMatrix%divide_by(Mmatrix%diag(cell_centered=.true.) )
    
    if(present(DampingRatio) )then
        call Imatrix%eyes(this%OmegaSqMatrix%size() )
        this%OmegaSqMatrix = this%OmegaSqMatrix - DampingRatio*DampingRatio*Imatrix
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
function getDisplacementWaveKernel(this,u_n,v_n,dt,fix_idx,cutoff_frequency) result(u)
    class(WaveKernel_),intent(inout) :: this
    real(real64),intent(in) :: u_n(:),v_n(:)
    real(real64),allocatable :: u(:)
    integer(int32),optional,intent(in) :: fix_idx(:)
    real(real64),intent(in) :: dt
    real(real64),optional,intent(in)  :: cutoff_frequency
    integer(int32) :: j
    real(real64) :: h, ddt

    h  = this%DampingRatio
    
    if(present(cutoff_frequency) )then
        
        ! cutoff = - 10 dB
        ddt = 1.0d0/(cutoff_frequency*4.0d0)
        ! Hanning Window
        u =  0.250d0*this%OmegaSqMatrix%tensor_wave_kernel(&
                u0=u_n,v0=v_n,h=this%DampingRatio,t=dt-ddt,&
                itrmax=this%itrmax,tol=this%tol,fix_idx=fix_idx)&
            + 0.50d0*this%OmegaSqMatrix%tensor_wave_kernel(&
                u0=u_n,v0=v_n,h=this%DampingRatio,t=dt,&
                itrmax=this%itrmax,tol=this%tol,fix_idx=fix_idx)&
            + 0.250d0*this%OmegaSqMatrix%tensor_wave_kernel(&
                u0=u_n,v0=v_n,h=this%DampingRatio,t=dt+ddt,&
                itrmax=this%itrmax,tol=this%tol,fix_idx=fix_idx)

    else
        u = this%OmegaSqMatrix%tensor_wave_kernel(&
            u0=u_n,v0=v_n,h=this%DampingRatio,t=dt,&
            itrmax=this%itrmax,tol=this%tol,fix_idx=fix_idx)
    endif

end function
! ##############################################################


! ##############################################################
function getVelocityWaveKernel(this,u_n, v_n,dt,fix_idx,cutoff_frequency) result(v)
    class(WaveKernel_),intent(inout) :: this
    real(real64),intent(in) :: u_n(:),v_n(:)
    
    integer(int32),optional,intent(in) :: fix_idx(:)
    real(real64),optional,intent(in)  :: cutoff_frequency
    real(real64),allocatable :: v(:)
    real(real64),intent(in) :: dt
    integer(int32) :: j
    real(real64) :: h,ddt

    h  = this%DampingRatio
    if(present(cutoff_frequency) )then
!        v = this%OmegaSqMatrix%tensor_wave_kernel_LPF(&
!            u0=u_n,v0=v_n,h=this%DampingRatio,t=dt,&
!            itrmax=this%itrmax,tol=this%tol,fix_idx=fix_idx,&
!            cutoff_frequency=cutoff_frequency)
!
        ! cutoff = - 3 dB
        ddt = 1.0d0/(cutoff_frequency*4.0d0)
        
        v =   0.250d0*this%OmegaSqMatrix%tensor_wave_kernel(&
                u0=-h*(dt-ddt)*u_n+v_n, &
                v0=-this%OmegaSqMatrix%matmul(u_n)-h*(dt-ddt)*v_n, &
                h=h,t=(dt-ddt),itrmax=this%itrmax,tol=this%tol,fix_idx=fix_idx)&
            + 0.500d0*this%OmegaSqMatrix%tensor_wave_kernel(&
                u0=-h*dt*u_n+v_n, &
                v0=-this%OmegaSqMatrix%matmul(u_n)-h*dt*v_n, &
                h=h,t=dt,itrmax=this%itrmax,tol=this%tol,fix_idx=fix_idx)&
            + 0.250d0*this%OmegaSqMatrix%tensor_wave_kernel(&
                u0=-h*(dt+ddt)*u_n+v_n, &
                v0=-this%OmegaSqMatrix%matmul(u_n)-h*(dt+ddt)*v_n, &
                h=h,t=(dt+ddt),itrmax=this%itrmax,tol=this%tol,fix_idx=fix_idx)
    else
        v = this%OmegaSqMatrix%tensor_wave_kernel(&
            u0=-h*dt*u_n+v_n, &
            v0=-this%OmegaSqMatrix%matmul(u_n)-h*dt*v_n, &
            h=h,t=dt,itrmax=this%itrmax,tol=this%tol,fix_idx=fix_idx)
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


end module
