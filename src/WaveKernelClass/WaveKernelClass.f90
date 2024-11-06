module WaveKernelClass
   use SparseClass
   use FEMDomainClass
   use FEMSolverClass
   use DEMDomainClass
   use SpectreAnalysisClass
   implicit none

   type :: WaveKernel_
      type(CRS_) :: OmegaSqMatrix
      real(Real64), allocatable :: Mmatrix_diag(:)
      real(Real64), allocatable :: DampingRatio(:)
      real(real64) :: tol = dble(1.0e-25)
      real(real64), allocatable :: v_in1(:)
      real(real64), allocatable :: v_in2(:)
      real(real64), allocatable :: v_out1(:)
      real(real64), allocatable :: v_out2(:)
      real(real64), allocatable :: u_in1(:)
      real(real64), allocatable :: u_in2(:)
      real(real64), allocatable :: u_out1(:)
      real(real64), allocatable :: u_out2(:)
      real(real64), allocatable :: hanning_buffer(:, :)
      integer(int32) :: itrmax = 100

      ! EbO-Scheme
      integer(int32) :: EbO_Algorithm = FEMDomain_Overset_GPP
      real(real64)   :: EbO_relative_penalty = 1000.0d0

      ! custom low-pass filter
      real(real64), allocatable, private :: weights(:)

   contains

      procedure, pass :: initWaveKernel
      procedure, pass :: init_by_femdomain_pointers_WK
      !procedure, pass :: initWaveKernel_DEM
      generic :: init => initWaveKernel, init_by_femdomain_pointers_WK !,initWaveKernel_DEM

      procedure :: setLPF => setLPF_WaveKernel

      procedure :: bandpass => bandpass_WaveKernel
      procedure :: lowpass => lowpass_WaveKernel
      procedure :: movingAverage => movingAverage_WaveKernel

      ! >>> RECCOMENDED >>>
      procedure, pass :: getDisplacement_and_Velocity_WaveKernel
      procedure, pass :: getDisplacement_and_Velocity_MPI_WaveKernel
      procedure, pass :: getDisplacement_and_Velocity_complex64_WaveKernel
      generic :: getDisplacement_and_Velocity &
         => getDisplacement_and_Velocity_WaveKernel, &
         getDisplacement_and_Velocity_complex64_WaveKernel, &
         getDisplacement_and_Velocity_MPI_WaveKernel
      ! <<< RECCOMENDED <<<

      procedure :: getDisplacement => getDisplacementWaveKernel
      procedure :: getVelocity => getVelocityWaveKernel

   end type

contains

! ##############################################################
   subroutine initWaveKernel(this, FEMDomain, DOF, YoungModulus, PoissonRatio, &
                             DampingRatio, Density)
      class(WaveKernel_), intent(inout) :: this
      integer(int32), intent(in) :: DOF
      type(FEMDomain_), intent(inout) :: FEMDomain
      real(real64), intent(in) :: YoungModulus(:), Density(:)
      real(real64), optional, intent(in) ::  PoissonRatio(:), DampingRatio(:)
      type(CRS_) :: Imatrix, Mmatrix, Cmatrix

      Mmatrix = FEMDomain%MassMatrix(DOF=DOF, Density=Density)

      if (DOF == 1) then
         this%OmegaSqMatrix = FEMDomain%StiffnessMatrix( &
                              YoungModulus=YoungModulus)

      else
         if (.not. present(PoissonRatio)) then
            print *, "[initWaveKernel] Please input PoissonRatio"
            stop
         end if
         this%OmegaSqMatrix = FEMDomain%StiffnessMatrix( &
                              YoungModulus=YoungModulus, PoissonRatio=PoissonRatio)

      end if

      this%Mmatrix_diag = Mmatrix%diag(cell_centered=.true.)
      this%OmegaSqMatrix = this%OmegaSqMatrix%divide_by(this%Mmatrix_diag)

      this%DampingRatio = zeros(this%OmegaSqMatrix%size())

      if (present(DampingRatio)) then
         !call Imatrix%eyes(this%OmegaSqMatrix%size() )
         Cmatrix = to_diag(DampingRatio*DampingRatio) ! vector to diagonal matrix
         this%OmegaSqMatrix = this%OmegaSqMatrix - Cmatrix
         this%DampingRatio = DampingRatio
      end if

   end subroutine initWaveKernel
! ##############################################################


!! ##############################################################
!subroutine initWaveKernel_DEM(this, DEMDomain, DOF, YoungModulus, PoissonRatio, &
!      DampingRatio, Density)
!class(WaveKernel_), intent(inout) :: this
!integer(int32), intent(in) :: DOF
!type(DEMDomain_), intent(inout) :: DEMDomain
!real(real64), intent(in) :: YoungModulus(:), Density(:)
!real(real64), optional, intent(in) ::  PoissonRatio(:), DampingRatio(:)
!type(CRS_) :: Imatrix, Mmatrix, Cmatrix
!
!Mmatrix = DEMDomain%MassMatrix(DOF=DOF, Density=Density)
!
!if (DOF == 1) then
!this%OmegaSqMatrix = DEMDomain%StiffnessMatrix( &
!       YoungModulus=YoungModulus)
!
!else
!if (.not. present(PoissonRatio)) then
!print *, "[initWaveKernel] Please input PoissonRatio"
!stop
!end if
!this%OmegaSqMatrix = DEMDomain%StiffnessMatrix( &
!       YoungModulus=YoungModulus, PoissonRatio=PoissonRatio)
!
!end if
!
!this%Mmatrix_diag = Mmatrix%diag(cell_centered=.true.)
!this%OmegaSqMatrix = this%OmegaSqMatrix%divide_by(this%Mmatrix_diag)
!
!this%DampingRatio = zeros(this%OmegaSqMatrix%size())
!
!if (present(DampingRatio)) then
!!call Imatrix%eyes(this%OmegaSqMatrix%size() )
!Cmatrix = to_diag(DampingRatio*DampingRatio) ! vector to diagonal matrix
!this%OmegaSqMatrix = this%OmegaSqMatrix - Cmatrix
!this%DampingRatio = DampingRatio
!end if
!
!end subroutine initWaveKernel
! ##############################################################


! ##############################################################
   subroutine init_by_femdomain_pointers_WK(this, FEMDomainPointers, DOF, YoungModulus, PoissonRatio, &
                                            DampingRatio, Density)
      class(WaveKernel_), intent(inout) :: this
      integer(int32), intent(in) :: DOF
      type(FEMDomainp_), intent(inout) :: FEMDomainPointers(:)
      type(FEMSolver_) :: solver
      real(real64), intent(in) :: YoungModulus(:), Density(:)
      real(real64), optional, intent(in) ::  PoissonRatio(:), DampingRatio(:)
      integer(int32) :: DomainID, ElementID
      type(CRS_) :: Imatrix, Mmatrix, Cmatrix, Kmatrix

! initialize wave kernel function by given FEMDomain-pointers
      call solver%init(NumDomain=size(FEMDomainPointers))
      call solver%setDomain(FEMDomainPointers=FEMDomainPointers)
      call solver%setCRS(DOF=DOF)

!$OMP parallel do private(ElementID)
      do DomainID = 1, size(FEMDomainPointers)
         do ElementID = 1, FEMDomainPointers(DomainID)%femdomainp%ne()
            call solver%setMatrix(DomainID=DomainID, ElementID=ElementID, DOF=DOF, &
                                  Matrix=FEMDomainPointers(DomainID)%femdomainp%StiffnessMatrix( &
                                  ElementID=ElementID, &
                                  E=YoungModulus(get_element_idx(FEMDomainPointers, DomainID, ElementID)), &
                                  v=PoissonRatio(get_element_idx(FEMDomainPointers, DomainID, ElementID))))
         end do
      end do
!$OMP end parallel do

      call solver%setEbOM(penalty=this%EbO_relative_penalty, DOF=3)
      Kmatrix = solver%getCRS()
      call solver%zeros()

! mass matrix
!$OMP parallel do private(ElementID)
      do DomainID = 1, size(FEMDomainPointers)
         do ElementID = 1, FEMDomainPointers(DomainID)%femdomainp%ne()
            call solver%setMatrix(DomainID=DomainID, ElementID=ElementID, DOF=DOF, &
                                  Matrix=FEMDomainPointers(DomainID)%femdomainp%MassMatrix( &
                                  ElementID=ElementID, &
                                  Density=Density(get_element_idx(FEMDomainPointers, DomainID, ElementID)), &
                                  DOF=3))
         end do
      end do
!$OMP end parallel do
      Mmatrix = solver%getCRS()

      this%OmegaSqMatrix = Kmatrix
      this%Mmatrix_diag = Mmatrix%diag(cell_centered=.true.)
      this%OmegaSqMatrix = this%OmegaSqMatrix%divide_by(this%Mmatrix_diag)

      this%DampingRatio = zeros(this%OmegaSqMatrix%size())

      if (present(DampingRatio)) then
         !call Imatrix%eyes(this%OmegaSqMatrix%size() )
         Cmatrix = to_diag(DampingRatio*DampingRatio) ! vector to diagonal matrix
         this%OmegaSqMatrix = this%OmegaSqMatrix - Cmatrix
         this%DampingRatio = DampingRatio
      end if

   end subroutine init_by_femdomain_pointers_WK
! ##############################################################

! ##############################################################
   subroutine bandpass_WaveKernel(this, u_n, v_n, freq_range, dt, fix_idx)
      class(WaveKernel_), intent(inout) :: this
      real(real64), intent(inout) :: u_n(:), v_n(:)
      real(real64), intent(in) :: freq_range(1:2)
      integer(int32), optional, intent(in) :: fix_idx(:)
      real(real64), allocatable :: u(:)
      real(real64), intent(in) :: dt
      integer(int32) :: j
      real(real64) :: h

      if (.not. allocated(this%u_in1)) then
         this%u_in1 = u_n
         this%u_in2 = u_n
         this%u_out1 = u_n
         this%u_out2 = u_n
      end if

      if (.not. allocated(this%v_in1)) then
         this%v_in1 = v_n
         this%v_in2 = v_n
         this%v_out1 = v_n
         this%v_out2 = v_n
      end if

      u_n(fix_idx) = 0.0d0
      !$OMP parallel do
      do j = 1, size(u_n)
         u_n(j) = bandpass_filter(x=u_n(j), &
                                  freq_range=freq_range, &
                                  sampling_Hz=1.0d0/dt, &
                                  in1=this%u_in1(j), &
                                  in2=this%u_in2(j), &
                                  out1=this%u_out1(j), &
                                  out2=this%u_out2(j) &
                                  )
      end do
      !$OMP end parallel do

      v_n(fix_idx) = 0.0d0
      !$OMP parallel do
      do j = 1, size(v_n)
         v_n(j) = bandpass_filter(x=v_n(j), &
                                  freq_range=freq_range, &
                                  sampling_Hz=1.0d0/dt, &
                                  in1=this%v_in1(j), &
                                  in2=this%v_in2(j), &
                                  out1=this%v_out1(j), &
                                  out2=this%v_out2(j) &
                                  )
      end do
      !$OMP end parallel do

   end subroutine
! ##############################################################

! ##############################################################
   subroutine lowpass_WaveKernel(this, u_n, v_n, cutoff_frequency, dt, fix_idx)
      class(WaveKernel_), intent(inout) :: this
      real(real64), intent(inout) :: u_n(:), v_n(:)
      real(real64), intent(in) :: cutoff_frequency
      integer(int32), optional, intent(in) :: fix_idx(:)
      real(real64), allocatable :: u(:)
      real(real64), intent(in) :: dt
      integer(int32) :: j
      real(real64) :: h

      if (.not. allocated(this%u_in1)) then
         this%u_in1 = u_n
      end if

      if (.not. allocated(this%v_in1)) then
         this%v_in1 = v_n
      end if

      u_n(fix_idx) = 0.0d0
      !$OMP parallel do
      do j = 1, size(u_n)
         u_n(j) = lowpass_filter(x_n=u_n(j), &
                                 fc=cutoff_frequency, &
                                 sampling_Hz=1.0d0/dt, &
                                 buf=this%u_in1(j) &
                                 )
      end do
      !$OMP end parallel do

      v_n(fix_idx) = 0.0d0
      !$OMP parallel do
      do j = 1, size(v_n)
         v_n(j) = lowpass_filter(x_n=v_n(j), &
                                 fc=cutoff_frequency, &
                                 sampling_Hz=1.0d0/dt, &
                                 buf=this%v_in1(j) &
                                 )
      end do
      !$OMP end parallel do

   end subroutine
! ##############################################################

! ##############################################################
   subroutine movingAverage_WaveKernel(this, u_n, v_n, fix_idx)
      class(WaveKernel_), intent(inout) :: this
      real(real64), intent(inout) :: u_n(:), v_n(:)
      integer(int32), optional, intent(in) :: fix_idx(:)
      real(real64), allocatable :: u(:)
      integer(int32) :: j
      real(real64) :: h

      if (.not. allocated(this%u_in1)) then
         this%u_in1 = u_n
         this%u_in2 = u_n
         this%u_out1 = u_n
         this%u_out2 = u_n
      end if

      if (.not. allocated(this%v_in1)) then
         this%v_in1 = v_n
         this%v_in2 = v_n
         this%v_out1 = v_n
         this%v_out2 = v_n
      end if
      u_n(fix_idx) = 0.0d0
      !$OMP parallel do
      do j = 1, size(u_n)
         u_n(j) = movingAverage_filter(x=u_n(j), &
                                       in1=this%u_in1(j), &
                                       in2=this%u_in2(j) &
                                       )
      end do
      !$OMP end parallel do

      v_n(fix_idx) = 0.0d0
      !$OMP parallel do
      do j = 1, size(v_n)
         v_n(j) = movingAverage_filter(x=v_n(j), &
                                       in1=this%v_in1(j), &
                                       in2=this%v_in2(j) &
                                       )
      end do
      !$OMP end parallel do

   end subroutine
! ##############################################################

! ##############################################################
   subroutine getDisplacement_and_Velocity_WaveKernel(this, u_n, v_n, dt, &
                                                      fix_idx, cutoff_frequency, debug_mode, u, v, RHS)
      class(WaveKernel_), intent(inout) :: this
      real(real64), intent(in) :: u_n(:), v_n(:)
      real(real64), optional, intent(in) :: RHS(:)
      real(real64), allocatable :: du(:), dv(:)
      real(real64), allocatable :: u(:), v(:)

      integer(int32), optional, allocatable, intent(in) :: fix_idx(:)
      real(real64), intent(in) :: dt
      logical, optional, intent(in) :: debug_mode
      real(real64), optional, intent(in)  :: cutoff_frequency

      integer(int32) :: j, k

      ! [CAUTION!!] only undamped is implemented.

      du = u_n

      if (present(fix_idx)) then
         if (allocated(fix_idx)) then
            du(fix_idx) = 0.0d0
         end if
      end if

      ! U_n から U?
      u = u_n
      v = 0.0d0*v_n

      do k = 1, this%itrmax

         if (present(fix_idx)) then
            if (allocated(fix_idx)) then
               du(fix_idx) = 0.0d0
            end if
         end if
         du = this%OmegaSqMatrix%matmul(du)

         if (present(fix_idx)) then
            if (allocated(fix_idx)) then
               du(fix_idx) = 0.0d0
            end if
         end if

         if (norm(LPF_cos_sqrt_taylor_coefficient(k=k, t=dt, f_c=cutoff_frequency)*du) &
             < this%tol) exit

         u = u + LPF_cos_sqrt_taylor_coefficient(k=k, t=dt, f_c=cutoff_frequency)*du

         if (k == 1) then
            v = -dt*du
         else
            v = v - LPF_t_sinc_sqrt_taylor_coefficient(k=k - 1, t=dt, f_c=cutoff_frequency)*du
         end if

      end do

      ! V_n から V?
      dv = v_n

      if (present(fix_idx)) then
         if (allocated(fix_idx)) then
            dv(fix_idx) = 0.0d0
         end if
      end if
      ! k=1
      u = u + dt*dv
      v = v + dv

      if (present(fix_idx)) then
         if (allocated(fix_idx)) then
            u(fix_idx) = 0.0d0
         end if
      end if

      if (present(fix_idx)) then
         if (allocated(fix_idx)) then
            v(fix_idx) = 0.0d0
         end if
      end if
      do k = 1, this%itrmax

         if (present(fix_idx)) then
            if (allocated(fix_idx)) then
               dv(fix_idx) = 0.0d0
            end if
         end if
         dv = this%OmegaSqMatrix%matmul(dv)

         if (present(fix_idx)) then
            if (allocated(fix_idx)) then
               dv(fix_idx) = 0.0d0
            end if
         end if
         if (norm(LPF_cos_sqrt_taylor_coefficient(k=k, t=dt, f_c=cutoff_frequency)*dv) &
             < this%tol) exit
         u = u + LPF_t_sinc_sqrt_taylor_coefficient(k=k, t=dt, f_c=cutoff_frequency)*dv
         v = v + LPF_cos_sqrt_taylor_coefficient(k=k, t=dt, f_c=cutoff_frequency)*dv
      end do

      if (present(RHS)) then
         if (allocated(this%weights)) then
            u = u + this%OmegaSqMatrix%tensor_wave_kernel_RHS(RHS=RHS/this%Mmatrix_diag, t=dt, tol=dble(1.0e-25), &
                                                        itrmax=this%itrmax, cutoff_frequency=cutoff_frequency, weights=this%weights)
            v = v + LPF_damped_t_sinc_sqrt_WaveKernelFunction(Omega_sq_matrix=this%OmegaSqMatrix, &
                                                              dt=dt, f_c=cutoff_frequency, v_n=RHS/this%Mmatrix_diag, &
                                             DampingRatio=this%DampingRatio, itrmax=this%itrmax, tol=this%tol, weights=this%weights)
         else
            u = u + this%OmegaSqMatrix%tensor_wave_kernel_RHS(RHS=RHS/this%Mmatrix_diag, t=dt, tol=dble(1.0e-25), &
                                                              itrmax=this%itrmax, cutoff_frequency=cutoff_frequency)
            v = v + LPF_damped_t_sinc_sqrt_WaveKernelFunction(Omega_sq_matrix=this%OmegaSqMatrix, &
                                                              dt=dt, f_c=cutoff_frequency, v_n=RHS/this%Mmatrix_diag, &
                                                              DampingRatio=this%DampingRatio, itrmax=this%itrmax, tol=this%tol)
         end if

      end if

      if (present(fix_idx)) then
         if (allocated(fix_idx)) then
            u(fix_idx) = 0.0d0
         end if
      end if

!    u = gain_value*u
!    v = gain_value*v

   end subroutine
! ##############################################################

! ##############################################################
   subroutine getDisplacement_and_Velocity_MPI_WaveKernel(this, u_n, v_n, dt, &
                                                          fix_idx, cutoff_frequency, debug_mode, u, v, RHS, MPID, FEMDomain)
      class(WaveKernel_), intent(inout) :: this
      real(real64), intent(in) :: u_n(:), v_n(:)
      real(real64), optional, intent(in) :: RHS(:)
      real(real64), allocatable :: du(:), dv(:)
      real(real64), allocatable :: u(:), v(:)

      type(MPI_), intent(inout) :: MPID
      type(FEMDomain_), intent(inout) :: FEMDomain

      integer(int32), optional, allocatable, intent(in) :: fix_idx(:)
      real(real64), intent(in) :: dt
      logical, optional, intent(in) :: debug_mode
      real(real64), intent(in)  :: cutoff_frequency
      real(real64) :: error_norm(1), all_error_norm(1)
      integer(int32) :: j, k

      ! [CAUTION!!] only undamped is implemented.

      du = u_n

      u = u_n
      v = 0.0d0*v_n
      do k = 1, this%itrmax

         !<<< du = this%OmegaSqMatrix%matmul(du) >>> for MPI
         du = FEMDomain%mpi_matmul(this%OmegaSqMatrix, du, MPID)
         error_norm = norm(LPF_cos_sqrt_taylor_coefficient(k=k, t=dt, f_c=cutoff_frequency)*du)
         call MPID%AllReduce(sendobj=error_norm, recvobj=all_error_norm, count=1, sum=.true.)

         if (all_error_norm(1) < this%tol) then
            exit
         end if
         u = u + LPF_cos_sqrt_taylor_coefficient(k=k, t=dt, f_c=cutoff_frequency)*du

         if (k == 1) then
            v = -dt*du
         else
            v = v - LPF_t_sinc_sqrt_taylor_coefficient(k=k - 1, t=dt, f_c=cutoff_frequency)*du
         end if

      end do
      deallocate (du)

      dv = v_n
      ! k=1
      u = u + dt*dv
      v = v + dv
      do k = 1, this%itrmax
         !<<< dv = this%OmegaSqMatrix%matmul(dv) >>> for MPI
         dv = FEMDomain%mpi_matmul(this%OmegaSqMatrix, dv, MPID)

         error_norm = norm(LPF_cos_sqrt_taylor_coefficient(k=k, t=dt, f_c=cutoff_frequency)*dv)
         call MPID%AllReduce(sendobj=error_norm, recvobj=all_error_norm, count=1, sum=.true.)

         if (all_error_norm(1) < this%tol) then
            exit
         end if
         u = u + LPF_t_sinc_sqrt_taylor_coefficient(k=k, t=dt, f_c=cutoff_frequency)*dv
         v = v + LPF_cos_sqrt_taylor_coefficient(k=k, t=dt, f_c=cutoff_frequency)*dv

      end do

      ! RHS (=M^{-1}F)
      if (present(RHS)) then
         if (allocated(this%weights)) then
            u = u + this%OmegaSqMatrix%tensor_wave_kernel_RHS(RHS=RHS/this%Mmatrix_diag, t=dt, tol=dble(1.0e-25), &
                                                        itrmax=this%itrmax, cutoff_frequency=cutoff_frequency, weights=this%weights)
            v = v + LPF_damped_t_sinc_sqrt_WaveKernelFunction(Omega_sq_matrix=this%OmegaSqMatrix, &
                                                              dt=dt, f_c=cutoff_frequency, v_n=RHS/this%Mmatrix_diag, &
                                             DampingRatio=this%DampingRatio, itrmax=this%itrmax, tol=this%tol, weights=this%weights)
         else
            u = u + this%OmegaSqMatrix%tensor_wave_kernel_RHS(RHS=RHS/this%Mmatrix_diag, t=dt, tol=dble(1.0e-25), &
                                                              itrmax=this%itrmax, cutoff_frequency=cutoff_frequency)
            v = v + LPF_damped_t_sinc_sqrt_WaveKernelFunction(Omega_sq_matrix=this%OmegaSqMatrix, &
                                                              dt=dt, f_c=cutoff_frequency, v_n=RHS/this%Mmatrix_diag, &
                                                              DampingRatio=this%DampingRatio, itrmax=this%itrmax, tol=this%tol)
         end if

      end if

      if (present(fix_idx)) then
         if (allocated(fix_idx)) then
            u(fix_idx) = 0.0d0
         end if
      end if

   end subroutine
! ##############################################################

! ##############################################################
   subroutine getDisplacement_and_Velocity_complex64_WaveKernel(this, u_n, v_n, dt, &
                                                                fix_idx, cutoff_frequency, debug_mode, u, v, RHS)
      class(WaveKernel_), intent(inout) :: this
      complex(real64), intent(in) :: u_n(:), v_n(:)
      real(real64), optional, intent(in) :: RHS(:)
      complex(real64), allocatable :: du(:), dv(:)
      complex(real64), allocatable :: u(:), v(:)
      integer(int32), optional, allocatable, intent(in) :: fix_idx(:)
      real(real64), intent(in) :: dt
      logical, optional, intent(in) :: debug_mode
      real(real64), optional, intent(in)  :: cutoff_frequency
      integer(int32) :: j, k

      ! [CAUTION!!] only undamped is implemented.

      du = u_n

      u = u_n
      v = 0.0d0*v_n
      do k = 1, this%itrmax
         du = this%OmegaSqMatrix%matmul(du)
         if (norm(LPF_cos_sqrt_taylor_coefficient(k=k, t=dt, f_c=cutoff_frequency)*du) &
             < this%tol) exit
         u = u + LPF_cos_sqrt_taylor_coefficient(k=k, t=dt, f_c=cutoff_frequency)*du

         if (k == 1) then
            v = -dt*du
         else
            v = v - LPF_t_sinc_sqrt_taylor_coefficient(k=k - 1, t=dt, f_c=cutoff_frequency)*du
         end if

      end do
      deallocate (du)

      dv = v_n
      ! k=1
      u = u + dt*dv
      v = v + dv
      do k = 1, this%itrmax
         dv = this%OmegaSqMatrix%matmul(dv)
         if (norm(LPF_cos_sqrt_taylor_coefficient(k=k, t=dt, f_c=cutoff_frequency)*dv) &
             < this%tol) exit
         u = u + LPF_t_sinc_sqrt_taylor_coefficient(k=k, t=dt, f_c=cutoff_frequency)*dv
         v = v + LPF_cos_sqrt_taylor_coefficient(k=k, t=dt, f_c=cutoff_frequency)*dv
      end do

      if (present(RHS)) then
         if (allocated(this%weights)) then
            u = u + this%OmegaSqMatrix%tensor_wave_kernel_RHS(RHS=RHS/this%Mmatrix_diag, t=dt, tol=dble(1.0e-25), &
                                                        itrmax=this%itrmax, cutoff_frequency=cutoff_frequency, weights=this%weights)
            v = v + LPF_damped_t_sinc_sqrt_WaveKernelFunction(Omega_sq_matrix=this%OmegaSqMatrix, &
                                                              dt=dt, f_c=cutoff_frequency, v_n=RHS/this%Mmatrix_diag, &
                                             DampingRatio=this%DampingRatio, itrmax=this%itrmax, tol=this%tol, weights=this%weights)
         else
            u = u + this%OmegaSqMatrix%tensor_wave_kernel_RHS(RHS=RHS/this%Mmatrix_diag, t=dt, tol=dble(1.0e-25), &
                                                              itrmax=this%itrmax, cutoff_frequency=cutoff_frequency)
            v = v + LPF_damped_t_sinc_sqrt_WaveKernelFunction(Omega_sq_matrix=this%OmegaSqMatrix, &
                                                              dt=dt, f_c=cutoff_frequency, v_n=RHS/this%Mmatrix_diag, &
                                                              DampingRatio=this%DampingRatio, itrmax=this%itrmax, tol=this%tol)
         end if
      end if

      if (present(fix_idx)) then
         if (allocated(fix_idx)) then
            u(fix_idx) = 0.0d0
         end if
      end if

   end subroutine
! ##############################################################

! ##############################################################
   function getDisplacementWaveKernel(this, u_n, v_n, dt, fix_idx, cutoff_frequency, debug_mode, RHS, gain) result(u)
      class(WaveKernel_), intent(inout) :: this
      real(real64), intent(in) :: u_n(:), v_n(:)

      real(real64), optional, intent(in) :: RHS(:)
      real(real64), allocatable :: u(:)
      integer(int32), optional, intent(in) :: fix_idx(:)
      real(real64), intent(in) :: dt
      logical, optional, intent(in) :: debug_mode
      real(real64), optional, intent(in)  :: cutoff_frequency, gain
      real(real64) :: gain_value
      integer(int32) :: j
      real(real64) :: h, ddt

      gain_value = input(default=1.0d0, option=gain)

      if (present(cutoff_frequency)) then
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
         if (allocated(this%weights)) then
            u = LPF_Damped_cos_sqrt_WaveKernelFunction(Omega_sq_matrix=this%OmegaSqMatrix, &
                                                       dt=dt, f_c=cutoff_frequency, u_n=u_n, DampingRatio=this%DampingRatio, &
                                                       fix_idx=fix_idx, itrmax=this%itrmax, tol=this%tol, weights=this%weights) &
                + LPF_Damped_t_sinc_sqrt_WaveKernelFunction(Omega_sq_matrix=this%OmegaSqMatrix, &
                                                            dt=dt, f_c=cutoff_frequency, v_n=v_n, DampingRatio=this%DampingRatio, &
                                                            fix_idx=fix_idx, itrmax=this%itrmax, tol=this%tol, weights=this%weights)
         else
            u = LPF_Damped_cos_sqrt_WaveKernelFunction(Omega_sq_matrix=this%OmegaSqMatrix, &
                                                       dt=dt, f_c=cutoff_frequency, u_n=u_n, DampingRatio=this%DampingRatio, &
                                                       fix_idx=fix_idx, itrmax=this%itrmax, tol=this%tol) &
                + LPF_Damped_t_sinc_sqrt_WaveKernelFunction(Omega_sq_matrix=this%OmegaSqMatrix, &
                                                            dt=dt, f_c=cutoff_frequency, v_n=v_n, DampingRatio=this%DampingRatio, &
                                                            fix_idx=fix_idx, itrmax=this%itrmax, tol=this%tol)
         end if
         ! cutoff = - 10 dB

      else
         ! cutoff周波数なし，ただのWKF
         u = this%OmegaSqMatrix%tensor_wave_kernel( &
             u0=u_n, v0=v_n, t=dt, h=0.0d0, &
             itrmax=this%itrmax, tol=this%tol, fix_idx=fix_idx)
         u = exp(-this%DampingRatio*dt)*u
      end if

      if (present(RHS)) then
         if (allocated(this%weights)) then
            u = u + this%OmegaSqMatrix%tensor_wave_kernel_RHS(RHS=RHS/this%Mmatrix_diag, t=dt, tol=dble(1.0e-25), &
                                                        itrmax=this%itrmax, cutoff_frequency=cutoff_frequency, weights=this%weights)
         else
            u = u + this%OmegaSqMatrix%tensor_wave_kernel_RHS(RHS=RHS/this%Mmatrix_diag, t=dt, tol=dble(1.0e-25), &
                                                              itrmax=this%itrmax, cutoff_frequency=cutoff_frequency)
         end if
      end if

      u = gain_value*u
   end function
! ##############################################################

! ##############################################################
   function getVelocityWaveKernel(this, u_n, v_n, RHS, dt, fix_idx, cutoff_frequency, debug_mode, gain) result(v)
      class(WaveKernel_), intent(inout) :: this
      real(real64), intent(in) :: u_n(:), v_n(:)
      real(real64), optional, intent(in) :: RHS(:)
      integer(int32), optional, intent(in) :: fix_idx(:)
      real(real64), optional, intent(in)  :: cutoff_frequency, gain
      logical, optional, intent(in) :: debug_mode
      real(real64), allocatable :: v(:)
      real(real64), intent(in) :: dt
      integer(int32) :: j
      real(real64) :: h, ddt, gain_value

      gain_value = input(default=1.0d0, option=gain)

      if (present(cutoff_frequency)) then
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
         if (allocated(this%weights)) then
            v = LPF_damped_cos_sqrt_WaveKernelFunction(Omega_sq_matrix=this%OmegaSqMatrix, &
                                                       dt=dt, f_c=cutoff_frequency, u_n=v_n, DampingRatio=this%DampingRatio, &
                                                       fix_idx=fix_idx, itrmax=this%itrmax, tol=this%tol, weights=this%weights) &
                + LPF_damped_t_sinc_sqrt_WaveKernelFunction(Omega_sq_matrix=this%OmegaSqMatrix, &
                                                            dt=dt, f_c=cutoff_frequency, v_n=-this%OmegaSqMatrix%matmul(u_n), &
                                                DampingRatio=this%DampingRatio, fix_idx=fix_idx, itrmax=this%itrmax, tol=this%tol, &
                                                            weights=this%weights)
         else
            v = LPF_damped_cos_sqrt_WaveKernelFunction(Omega_sq_matrix=this%OmegaSqMatrix, &
                                                       dt=dt, f_c=cutoff_frequency, u_n=v_n, DampingRatio=this%DampingRatio, &
                                                       fix_idx=fix_idx, itrmax=this%itrmax, tol=this%tol) &
                + LPF_damped_t_sinc_sqrt_WaveKernelFunction(Omega_sq_matrix=this%OmegaSqMatrix, &
                                                            dt=dt, f_c=cutoff_frequency, v_n=-this%OmegaSqMatrix%matmul(u_n), &
                                                  DampingRatio=this%DampingRatio, fix_idx=fix_idx, itrmax=this%itrmax, tol=this%tol)
         end if
      else
         ! cutoff周波数なし，ただのWKF
         v = exp(-this%DampingRatio*dt)*this%OmegaSqMatrix%tensor_wave_kernel( &
             u0=-this%DampingRatio*dt*u_n + v_n, &
             v0=-this%OmegaSqMatrix%matmul(u_n) - this%DampingRatio*dt*v_n, &
             h=0.0d0, t=dt, itrmax=this%itrmax, tol=this%tol, fix_idx=fix_idx)
      end if

      if (present(RHS)) then
         if (allocated(this%weights)) then
            v = v + LPF_damped_t_sinc_sqrt_WaveKernelFunction(Omega_sq_matrix=this%OmegaSqMatrix, &
                                                              dt=dt, f_c=cutoff_frequency, v_n=RHS/this%Mmatrix_diag, &
                                             DampingRatio=this%DampingRatio, itrmax=this%itrmax, tol=this%tol, weights=this%weights)
         else
            v = v + LPF_damped_t_sinc_sqrt_WaveKernelFunction(Omega_sq_matrix=this%OmegaSqMatrix, &
                                                              dt=dt, f_c=cutoff_frequency, v_n=RHS/this%Mmatrix_diag, &
                                                              DampingRatio=this%DampingRatio, itrmax=this%itrmax, tol=this%tol)
         end if
      end if

      v = gain_value*v
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

   pure function LPF_cos_sqrt_taylor_coefficient(k, t, f_c) result(c_k)
      integer(int32), intent(in) :: k
      real(real64), intent(in) :: t
      real(real64), intent(in) :: f_c
      real(real64) :: c_k
      real(real64) :: t_hat, dt
      type(Math_) :: math

      if (k == 0) then
         c_k = 1.0d0
         return
      end if

      !dt = 1.0d0/(f_c*4.0d0)
      dt = 1.0d0/f_c/2.0d0/math%pi*acos(sqrt(2.0d0) - 1.0d0)

      t_hat = 0.250d0*((t - dt)**(2*k)) + 0.50d0*((t)**(2*k)) + 0.250d0*((t + dt)**(2*k))

      if (mod(k, 2) == 0) then
         ! k=even
         c_k = t_hat/Gamma(2.0d0*k + 1.0d0)
      else
         ! k=odd
         c_k = -1.0d0*t_hat/Gamma(2.0d0*k + 1.0d0)
      end if

   end function

   function LPF_damped_cos_sqrt_taylor_coefficient(k, t, f_c, DampingRatio, weights) result(c_k)
      integer(int32), intent(in) :: k
      real(real64), intent(in) :: t
      real(real64), intent(in) :: f_c
      real(real64), intent(in) :: DampingRatio(:)
      real(real64), optional, intent(in) :: weights(:)
      real(real64) :: dt
      real(real64), allocatable:: c_k(:)
      real(real64), allocatable:: t_tilde(:)
      type(Math_) :: math
      integer(int32) :: n

      integer(int32) :: lbd, ubd
      real(real64), allocatable :: w(:)

      if (present(weights)) then
         ubd = size(weights) - (size(weights) + 1)/2
         lbd = -(size(weights) - (size(weights) + 1)/2)
         allocate (w(lbd:ubd))
         w(lbd:ubd) = weights(1:size(weights))
      end if

      if (k == 0) then
         c_k = ones(size(DampingRatio))
         return
      end if
      t_tilde = zeros(size(DampingRatio))

      !dt = 1.0d0/(f_c*4.0d0)
      dt = 1.0d0/f_c/2.0d0/math%pi*acos(sqrt(2.0d0) - 1.0d0)

      if (present(weights)) then
         t_tilde = 0.0d0
         do n = lbound(w, 1), ubound(w, 1)
            t_tilde = t_tilde + w(n)*((t + n*dt)**(2*k))*exp(-DampingRatio*(t + n*dt))
         end do
      else
         t_tilde = 0.250d0*((t - dt)**(2*k))*exp(-DampingRatio*(t - dt)) &
                   + 0.500d0*((t)**(2*k))*exp(-DampingRatio*(t)) &
                   + 0.250d0*((t + dt)**(2*k))*exp(-DampingRatio*(t + dt))
      end if

      if (mod(k, 2) == 0) then
         ! k=even
         c_k = t_tilde/Gamma(2.0d0*k + 1.0d0)
      else
         ! k=odd
         c_k = -1.0d0*t_tilde/Gamma(2.0d0*k + 1.0d0)
      end if

   end function

   pure function LPF_t_sinc_sqrt_taylor_coefficient(k, t, f_c) result(s_k)
      integer(int32), intent(in) :: k
      real(real64), intent(in) :: t
      real(real64), intent(in) :: f_c
      real(real64) :: s_k
      real(real64) :: t_hat, dt
      type(Math_) :: math

      if (k == 0) then
         s_k = t
         return
      end if

      !dt = 1.0d0/(f_c*4.0d0)
      dt = 1.0d0/f_c/2.0d0/math%pi*acos(sqrt(2.0d0) - 1.0d0)

      t_hat = 0.250d0*((t - dt)**(2*k + 1)) + 0.50d0*((t)**(2*k + 1)) &
              + 0.250d0*((t + dt)**(2*k + 1))

      if (mod(k, 2) == 0) then
         ! k=even
         s_k = (2.0d0**(-1 - 2*k))*sqrt(math%pi)/Gamma(1.0d0 + dble(k))/Gamma(1.5d0 + dble(k)) &
               *t_hat
      else
         ! k=odd
         s_k = -1.0d0* &
               (2.0d0**(-1 - 2*k))*sqrt(math%pi)/Gamma(1.0d0 + dble(k))/Gamma(1.5d0 + dble(k)) &
               *t_hat
      end if

   end function

   function LPF_damped_t_sinc_sqrt_taylor_coefficient(k, t, f_c, DampingRatio, weights) result(s_k)
      integer(int32), intent(in) :: k
      real(real64), intent(in) :: t
      real(real64), intent(in) :: f_c
      real(real64), allocatable :: s_k(:)
      real(real64), allocatable :: t_tilde(:)
      real(real64), optional, intent(in) :: weights(:)
      real(real64) :: dt, buf
      real(real64), intent(in) :: DampingRatio(:)
      type(Math_) :: math
      integer(int32) :: n

      integer(int32) :: lbd, ubd
      real(real64), allocatable :: w(:)

      if (present(weights)) then
         ubd = size(weights) - (size(weights) + 1)/2
         lbd = -(size(weights) - (size(weights) + 1)/2)
         allocate (w(lbd:ubd))
         w(lbd:ubd) = weights(1:)
      end if

      if (k == 0) then
         s_k = t*ones(size(DampingRatio))
         return
      end if
      t_tilde = zeros(size(DampingRatio))

      !dt = 1.0d0/(f_c*4.0d0)
      dt = 1.0d0/f_c/2.0d0/math%pi*acos(sqrt(2.0d0) - 1.0d0)

      if (present(weights)) then
         t_tilde = 0.0d0
         do n = lbound(w, 1), ubound(w, 1)
            t_tilde = t_tilde + w(n)*((t + n*dt)**(2*k + 1))*exp(-DampingRatio*(t + n*dt))
         end do

         ! check solution
         !if(lbound(weights,1)==-1 .and. ubound(weights,1)==1)then
         !    if(weights(-1)==0.250d0 .or. weights(0)==0.50d0)then
         !        if(weights(0)==0.250d0)then
         !            buf =  0.250d0*((t-dt)**(2*k+1) )*exp(-DampingRatio*(t-dt) ) &
         !            + 0.500d0*((t   )**(2*k+1) )*exp(-DampingRatio*(t   ) ) &
         !            + 0.250d0*((t+dt)**(2*k+1) )*exp(-DampingRatio*(t+dt) )
         !            if(buf/=t_tilde)then
         !                open(10,file="debug.txt")
         !                write(10,*) buf,t_tilde
         !                close(10)
         !            endif
         !        endif
         !    endif
         !endif
         ! check solution

      else
         t_tilde = 0.250d0*((t - dt)**(2*k + 1))*exp(-DampingRatio*(t - dt)) &
                   + 0.500d0*((t)**(2*k + 1))*exp(-DampingRatio*(t)) &
                   + 0.250d0*((t + dt)**(2*k + 1))*exp(-DampingRatio*(t + dt))
      end if

      if (mod(k, 2) == 0) then
         ! k=even
         s_k = (2.0d0**(-1 - 2*k))*sqrt(math%pi)/Gamma(1.0d0 + dble(k))/Gamma(1.5d0 + dble(k)) &
               *t_tilde
      else
         ! k=odd
         s_k = -1.0d0* &
               (2.0d0**(-1 - 2*k))*sqrt(math%pi)/Gamma(1.0d0 + dble(k))/Gamma(1.5d0 + dble(k)) &
               *t_tilde
      end if

   end function

! #########################################################
   function LPF_cos_sqrt_WaveKernelFunction(Omega_sq_matrix, dt, f_c, u_n, itrmax, tol, fix_idx) result(ret)
      type(CRS_), intent(inout) :: Omega_sq_matrix
      real(real64), intent(in) :: dt, f_c, u_n(:), tol
      real(real64), allocatable :: ret(:), du(:)

      integer(int32), intent(in) :: itrmax, fix_idx(:)
      integer(int32) :: k

      k = 0
      du = u_n
      du(fix_idx) = 0.0d0
      ret = LPF_cos_sqrt_taylor_coefficient(k=k, t=dt, f_c=f_c)*du

      do k = 1, itrmax
         du = Omega_sq_matrix%matmul(du)
         if (norm(LPF_cos_sqrt_taylor_coefficient(k=k, t=dt, f_c=f_c)*du) < tol) exit
         ret = ret + LPF_cos_sqrt_taylor_coefficient(k=k, t=dt, f_c=f_c)*du
      end do

   end function
! #########################################################

! #########################################################
   function LPF_damped_cos_sqrt_WaveKernelFunction(Omega_sq_matrix,dt,f_c,u_n,DampingRatio, itrmax,tol,fix_idx,weights) result(ret)
      type(CRS_), intent(inout) :: Omega_sq_matrix
      real(real64), intent(in) :: dt, f_c, u_n(:), tol, DampingRatio(:)

      real(real64), allocatable :: ret(:), du(:)

      integer(int32), intent(in) :: itrmax
      integer(int32), optional, intent(in) :: fix_idx(:)
      real(real64), optional, intent(in) :: weights(:)

      integer(int32) :: k

      k = 0
      du = u_n
      if (present(fix_idx)) then
         du(fix_idx) = 0.0d0
      end if
      ret = LPF_damped_cos_sqrt_taylor_coefficient(k=k, t=dt, f_c=f_c, DampingRatio=DampingRatio, weights=weights)*du

      do k = 1, itrmax
         du = Omega_sq_matrix%matmul(du)
     if (norm(LPF_damped_cos_sqrt_taylor_coefficient(k=k, t=dt, f_c=f_c, DampingRatio=DampingRatio, weights=weights)*du) < tol) exit
         ret = ret + LPF_damped_cos_sqrt_taylor_coefficient(k=k, t=dt, f_c=f_c, DampingRatio=DampingRatio, weights=weights)*du
      end do

   end function
! #########################################################

! #########################################################
  function LPF_damped_cos_sqrt_complex64_WaveKernelFunction(Omega_sq_matrix,dt,f_c,u_n,DampingRatio, itrmax,tol,fix_idx) result(ret)
      type(CRS_), intent(inout) :: Omega_sq_matrix
      real(real64), intent(in) :: dt, f_c, tol, DampingRatio(:)
      complex(real64), intent(in) :: u_n(:)
      complex(real64), allocatable :: ret(:), du(:)

      integer(int32), intent(in) :: itrmax
      integer(int32), optional, intent(in) :: fix_idx(:)
      integer(int32) :: k

      k = 0
      du = u_n
      if (present(fix_idx)) then
         du(fix_idx) = 0.0d0
      end if
      ret = LPF_damped_cos_sqrt_taylor_coefficient(k=k, t=dt, f_c=f_c, DampingRatio=DampingRatio)*du

      do k = 1, itrmax
         du = Omega_sq_matrix%matmul(du)
         if (norm(LPF_damped_cos_sqrt_taylor_coefficient(k=k, t=dt, f_c=f_c, DampingRatio=DampingRatio)*du) < tol) exit
         ret = ret + LPF_damped_cos_sqrt_taylor_coefficient(k=k, t=dt, f_c=f_c, DampingRatio=DampingRatio)*du
      end do

   end function
! #########################################################

! #########################################################
   function LPF_t_sinc_sqrt_WaveKernelFunction(Omega_sq_matrix, dt, f_c, v_n, itrmax, tol, fix_idx) result(ret)
      type(CRS_), intent(inout) :: Omega_sq_matrix
      real(real64), intent(in) :: dt, f_c, v_n(:), tol
      real(real64), allocatable :: ret(:), dv(:)
      integer(int32), intent(in) :: itrmax, fix_idx(:)
      integer(int32) :: k

      k = 0
      dv = v_n
      dv(fix_idx) = 0.0d0
      ret = LPF_t_sinc_sqrt_taylor_coefficient(k=k, t=dt, f_c=f_c)*dv

      do k = 1, itrmax
         dv = Omega_sq_matrix%matmul(dv)
         if (norm(LPF_t_sinc_sqrt_taylor_coefficient(k=k, t=dt, f_c=f_c)*dv) < tol) exit
         ret = ret + LPF_t_sinc_sqrt_taylor_coefficient(k=k, t=dt, f_c=f_c)*dv
      end do

   end function
! #########################################################

! #########################################################
  function LPF_damped_t_sinc_sqrt_WaveKernelFunction(Omega_sq_matrix,dt,f_c,v_n,itrmax,tol,fix_idx,DampingRatio,weights) result(ret)
      type(CRS_), intent(inout) :: Omega_sq_matrix
      real(real64), intent(in) :: dt, f_c, v_n(:), tol, DampingRatio(:)
      real(real64), allocatable :: ret(:), dv(:)
      integer(int32), intent(in) :: itrmax
      integer(int32), optional, intent(in) :: fix_idx(:)
      real(real64), optional, intent(in) :: weights(:)
      integer(int32) :: k

      k = 0
      dv = v_n
      if (present(fix_idx)) then
         dv(fix_idx) = 0.0d0
      end if
      ret = LPF_damped_t_sinc_sqrt_taylor_coefficient(k=k, t=dt, f_c=f_c, DampingRatio=DampingRatio, weights=weights)*dv

      do k = 1, itrmax
         dv = Omega_sq_matrix%matmul(dv)
  if (norm(LPF_damped_t_sinc_sqrt_taylor_coefficient(k=k, t=dt, f_c=f_c, DampingRatio=DampingRatio, weights=weights)*dv) < tol) exit
         ret = ret + LPF_damped_t_sinc_sqrt_taylor_coefficient(k=k, t=dt, f_c=f_c, DampingRatio=DampingRatio, weights=weights)*dv
      end do

   end function
! #########################################################

! #########################################################
   function LPF_damped_t_sinc_sqrt_MPI_WaveKernelFunction(Omega_sq_matrix, &
                                             dt, f_c, v_n, itrmax, tol, fix_idx, DampingRatio, MPID, FEMDomain, weights) result(ret)
      type(CRS_), intent(inout) :: Omega_sq_matrix
      real(real64), intent(in) :: dt, f_c, v_n(:), tol, DampingRatio(:)
      type(MPI_), intent(inout) :: MPID
      type(FEMDomain_), intent(inout) :: FEMDomain
      real(real64), allocatable :: ret(:), dv(:)
      integer(int32), intent(in) :: itrmax
      integer(int32), optional, intent(in) :: fix_idx(:)
      real(real64), optional, intent(in) :: weights(:)
      integer(int32) :: k

      k = 0
      dv = v_n
      if (present(fix_idx)) then
         dv(fix_idx) = 0.0d0
      end if
      ret = LPF_damped_t_sinc_sqrt_taylor_coefficient(k=k, t=dt, f_c=f_c, DampingRatio=DampingRatio, weights=weights)*dv

      do k = 1, itrmax
         call MPID%barrier()
         !dv = Omega_sq_matrix%matmul(dv)
         dv = FEMDomain%mpi_matmul(Omega_Sq_Matrix, dv, MPID)

  if (norm(LPF_damped_t_sinc_sqrt_taylor_coefficient(k=k, t=dt, f_c=f_c, DampingRatio=DampingRatio, weights=weights)*dv) < tol) then
            call MPID%barrier()
            exit
         end if
         ret = ret + LPF_damped_t_sinc_sqrt_taylor_coefficient(k=k, t=dt, f_c=f_c, DampingRatio=DampingRatio, weights=weights)*dv
      end do
      call MPID%barrier()

   end function
! #########################################################

! #########################################################
   function LPF_damped_t_sinc_sqrt_complex64_WaveKernelFunction( &
      Omega_sq_matrix, dt, f_c, v_n, itrmax, tol, fix_idx, DampingRatio, weights) result(ret)
      type(CRS_), intent(inout) :: Omega_sq_matrix
      real(real64), intent(in) :: dt, f_c, tol, DampingRatio(:)

      complex(real64), intent(in) :: v_n(:)
      complex(real64), allocatable :: ret(:), dv(:)

      integer(int32), intent(in) :: itrmax
      integer(int32), optional, intent(in) :: fix_idx(:)
      real(real64), optional, intent(in) :: weights(:)
      integer(int32) :: k

      k = 0
      dv = v_n
      if (present(fix_idx)) then
         dv(fix_idx) = 0.0d0
      end if
      ret = LPF_damped_t_sinc_sqrt_taylor_coefficient(k=k, t=dt, f_c=f_c, DampingRatio=DampingRatio, weights=weights)*dv

      do k = 1, itrmax
         dv = Omega_sq_matrix%matmul(dv)
  if (norm(LPF_damped_t_sinc_sqrt_taylor_coefficient(k=k, t=dt, f_c=f_c, DampingRatio=DampingRatio, weights=weights)*dv) < tol) exit
         ret = ret + LPF_damped_t_sinc_sqrt_taylor_coefficient(k=k, t=dt, f_c=f_c, DampingRatio=DampingRatio, weights=weights)*dv
      end do

   end function
! #########################################################

   subroutine setLPF_WaveKernel(this, weights)
      class(WaveKernel_), intent(inout) :: this
      real(real64), intent(in) :: weights(:)

      if (mod(size(weights), 2) == 0) then
         print *, "[ERROR] >> setLPF_WaveKernel >> size(weights) should be odd."
      else
         this%weights = weights
      end if
   end subroutine

end module

