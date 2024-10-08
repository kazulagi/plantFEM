module SeismicAnalysisClass
   use fem
   use ModalAnalysisClass
   implicit none

   integer(int32) :: WAVE_DISP = 1
   integer(int32) :: WAVE_VELOCITY = 2
   integer(int32) :: WAVE_ACCEL = 3

   type::SeismicAnalysis_
      ! only for single-domain >>
      type(FEMDomain_), pointer :: femdomain
      type(FEMSolver_) :: femsolver
      type(CRS_) :: M_matrix, K_matrix
      ! <<<

      ! for multi-domain, use >>>
      type(ModalAnalysis_) :: modal
      ! <<<

      ! common for single/multi domain >>>
      real(real64), allocatable :: da(:) ! increment of accel.

      real(real64), allocatable :: a_ext(:) ! External accel.
      real(real64), allocatable :: a_ext_n(:) ! External accel.

      real(real64), allocatable :: a(:) ! accel.
      real(real64), allocatable :: a_half(:) ! for RK4
      real(real64), allocatable :: a_n(:) ! for RK4

      real(real64), allocatable :: v(:) ! velocity
      real(real64), allocatable :: v_half(:) ! for RK4
      real(real64), allocatable :: v_n(:)

      real(real64), allocatable :: u(:) ! disp.
      real(real64), allocatable :: u_n(:) ! disp.

      complex(real64), allocatable :: u_p(:)
      complex(real64), allocatable :: u_m(:)

      real(real64), allocatable :: du(:) ! increment of disp.
      real(real64), allocatable :: wave(:, :)
      real(real64), allocatable :: dwave(:, :)
      ! <<<

      ! >>> only for single-domain
      real(real64), allocatable :: Density(:)
      real(real64), allocatable :: YoungModulus(:)
      real(real64), allocatable :: PoissonRatio(:)

      real(real64) :: MaxA(3) = 0.0d0
      real(real64) :: MaxV(3) = 0.0d0
      real(real64) :: MaxU(3) = 0.0d0

      integer(int32), allocatable :: WaveNodeList(:)
      ! <<<

      ! displacement boundary

      ! only for single-domain >>
      integer(int32), allocatable :: FixNodeList_x(:)
      integer(int32), allocatable :: FixNodeList_y(:)
      integer(int32), allocatable :: FixNodeList_z(:)
      ! in case of multi-domain, use this%modal
      ! <<<

      ! only for single-domain >>
      real(real64), allocatable :: FixNodeList_Disp_x(:)
      real(real64), allocatable :: FixNodeList_Disp_y(:)
      real(real64), allocatable :: FixNodeList_Disp_z(:)
      ! in case of multi-domain, use
      real(real64), allocatable :: FixNodeList_Disp(:)
      ! <<<

      integer(int32), allocatable :: num_nodes_in_domains(:)

      real(real64), allocatable :: Traction(:)

      ! only for single-domain >>>
      integer(int32), allocatable :: absorbingBoundary_x(:)
      integer(int32), allocatable :: absorbingBoundary_y(:)
      integer(int32), allocatable :: absorbingBoundary_z(:)
      ! <<<

      integer(int32), allocatable :: absorbingBoundary_xyz(:)
      real(real64) :: absorbingBoundary_elasticity = 0.0d0
      real(real64) :: absorbingBoundary_viscosity = 0.0d0

      ! modal analysis
      real(real64), allocatable :: Frequency(:)
      real(real64), allocatable :: ModeVectors(:, :)
      integer(int32), allocatable :: NodeID_range(:, :)
      integer(int32), allocatable :: ElementID_range(:, :)
      ! <<<

      character(1) :: wavedirection = "z"
      integer(int32) :: wavetype = 0
      real(real64) :: dt = 1.0d0
      real(real64) :: error = dble(1.0e-14)
      real(real64) :: tolerance = dble(1.0e-14)
      real(real64) :: t = 0.0d0
      integer(int32) :: step = 0
      real(real64) :: alpha = 0.52400d0
      real(real64) :: beta = 0.00129d0 ! Rayleigh dumping parameters, h=1%

      real(real64) :: damping_ratio_h = 0.010d0! damping ratio regarding ma+2hwv+w^2 u = f

      real(real64) :: Newmark_beta = 0.250d0 ! Nemark-beta method parameters
      real(real64) :: Newmark_gamma = 0.50d0 ! Nemark-beta method parameters
      real(real64) :: boundary_dumping_ratio = 1.0d0

      logical :: restart = .False.
      logical :: debug = .False.

      logical :: multi_domain_mode = .false.
      real(real64) :: overset_penalty = 1000.0d0*1000.0d0 ! default
   contains
      ! multi-domain mode
      procedure, public :: init => initSeismicAnalysis
      procedure, public :: setMaterial => setMaterialSeismicAnalysis
      procedure, public :: setBoundary => setBoundarySeismicAnalysis
      procedure, public :: setSolverParameters => setSolverParametersSeismicAnalysis

      procedure, pass :: runSeismicAnalysis_user_function
      procedure, pass :: runSeismicAnalysis

      generic :: solve => runSeismicAnalysis, runSeismicAnalysis_user_function

      ! single-doamin mode
      procedure, public :: loadWave => loadWaveSeismicAnalysis
      procedure, public :: fixDisplacement => fixDisplacementSeismicAnalysis
      procedure, public :: updateWave => updateWaveSeismicAnalysis

      generic :: run => runSeismicAnalysis, runSeismicAnalysis_user_function

      procedure, public :: LinearReyleighNewmark => LinearReyleighNewmarkSeismicAnalysis
      procedure, public :: recordMaxValues => recordMaxValuesSeismicAnalysis
      procedure, public :: save => saveSeismicAnalysis
      procedure, public :: getNewmarkBetaMatrix => getNewmarkBetaMatrixSeismicAnalysis
      procedure, public :: getNewmarkBetaVector => getNewmarkBetaVectorSeismicAnalysis
      procedure, public :: updateVelocityNewmarkBeta => updateVelocityNewmarkBetaSeismicAnalysis
      procedure, public :: updateAccelNewmarkBeta => updateAccelNewmarkBetaSeismicAnalysis

      procedure, public :: remove => removeSeismicAnalysis

      procedure, public :: absorbingBoundary => absorbingBoundarySeismicAnalysis
      procedure, public :: getAbsorbingBoundaryForce => getAbsorbingBoundaryForceSeismicAnalysis
      procedure, public :: velocity => velocitySeismicAnalysis
      procedure, pass :: modalAnalysisSeismicAnalysis_single_domain
      procedure, pass :: modalAnalysisSeismicAnalysis_multi_domain

      generic :: modalAnalysis => modalAnalysisSeismicAnalysis_single_domain, &
         modalAnalysisSeismicAnalysis_multi_domain

      procedure, public :: getModeVector => getModeVectorSeismicAnalysis
      procedure, public :: exportModeShape => exportModeShapeSeismicAnalysis

      procedure, public :: vtk => vtkSeismicAnalysis
   end type

contains

! ##############################################
   subroutine initSeismicAnalysis(obj, femdomains)
      class(SeismicAnalysis_), intent(inout) :: obj
      type(FEMDomain_), optional, intent(in) :: femdomains(:)
      integer(int32) :: i, n

      n = size(femdomains)
      allocate (obj%num_nodes_in_domains(n))
      do i = 1, n
         obj%num_nodes_in_domains(i) = femdomains(i)%nn()
      end do
      obj%femsolver%num_nodes_in_domains = obj%num_nodes_in_domains

      if (present(femdomains)) then
         call obj%modal%init(domains=femdomains)
         obj%multi_domain_mode = .true.
         n = 0
         do i = 1, size(femdomains)
            n = n + femdomains(i)%nn()*femdomains(i)%nd()
         end do
         obj%U = zeros(n)
         obj%V = zeros(n)
         obj%A = zeros(n)
      else
         ! Regacy mode
         obj%U = zeros(obj%femdomain%nn()*obj%femdomain%nd())
         obj%V = zeros(obj%femdomain%nn()*obj%femdomain%nd())
         obj%A = zeros(obj%femdomain%nn()*obj%femdomain%nd())

         if (obj%femdomain%mesh%empty()) then
            print *, "[ERROR] Seismic % init >> obj%femdomain is empty"
            stop
         end if
         obj%Density = zeros(obj%femdomain%ne())
         obj%YoungModulus = zeros(obj%femdomain%ne())
         obj%PoissonRatio = zeros(obj%femdomain%ne())
         obj%A_ext = zeros(obj%femdomain%nn()*obj%femdomain%nd())
         obj%A_ext_n = zeros(obj%femdomain%nn()*obj%femdomain%nd())
         obj%multi_domain_mode = .false.
      end if
   end subroutine
! ##############################################
   subroutine setMaterialSeismicAnalysis(this, DomainID, Density, YoungModulus, PoissonRatio)
      class(SeismicAnalysis_), intent(inout) :: this
      integer(int32), intent(in) :: DomainID
      real(real64), intent(in)   :: YoungModulus(:), PoissonRatio(:), Density(:)

      if (.not. this%multi_domain_mode) then
         print *, "ERROR :: setMaterialSeismicAnalysis >> this%multi_domain_mode should be .true."
         print *, "please redo %init(femdomain=your_fem_domains)"
         stop
      end if
      call this%modal%setMaterial(DomainID=DomainID, &
                                  density=Density, YoungModulus=YoungModulus, PoissonRatio=PoissonRatio)

   end subroutine
! ##############################################

! ##############################################
   subroutine setBoundarySeismicAnalysis(this, DomainID, NodeList, condition, boundaryValue, overwrite)
      class(SeismicAnalysis_), intent(inout) :: this
      integer(int32), intent(in) :: DomainID, NodeList(:)
      character(*), intent(in) :: condition
      real(real64), intent(in) :: boundaryValue(:)
      logical, optional, intent(in) :: overwrite

      integer(int32) :: i, j, domain_node_offset, nd, n, m

      if (.not. this%multi_domain_mode) then
         print *, "ERROR :: setBoundarySeismicAnalysis >> this%multi_domain_mode should be .true."
         print *, "please redo %init(femdomain=your_fem_domains)"
         stop
      end if

      select case (condition)

      case ("fix", "FIX", "Fix", "Dirichlet", "First", "U", "Displacement", "u")
         call this%modal%setBoundary(DomainID=DomainID, NodeList=NodeList)

         nd = this%modal%solver%femdomains(1)%femdomainp%nd()
         if (.not. allocated(this%FixNodeList_Disp)) then
            this%FixNodeList_Disp = zeros(maxval(this%modal%DomainNodeID(:, 2)) &
                                          *nd)
         end if
         domain_node_offset = this%modal%DomainNodeID(DomainID, 1) - 1
         do i = 1, size(NodeList)
            if (nd /= 3) then
               print *, "setBoundarySeismicAnalysis >> only for 3D"
               print *, "Please revise this part for 1/2 D"
               stop
            end if
            n = size(boundaryValue)
            m = size(NodeList)

            if (n == 1) then
               this%FixNodeList_Disp((domain_node_offset + NodeList(i))*nd - 2) = boundaryValue(1)
               this%FixNodeList_Disp((domain_node_offset + NodeList(i))*nd - 1) = boundaryValue(1)
               this%FixNodeList_Disp((domain_node_offset + NodeList(i))*nd - 0) = boundaryValue(1)
            elseif (n == 3) then
               this%FixNodeList_Disp((domain_node_offset + NodeList(i))*nd - 2) = boundaryValue(1)
               this%FixNodeList_Disp((domain_node_offset + NodeList(i))*nd - 1) = boundaryValue(2)
               this%FixNodeList_Disp((domain_node_offset + NodeList(i))*nd - 0) = boundaryValue(3)
            elseif (n == m) then
               this%FixNodeList_Disp((domain_node_offset + NodeList(i))*nd - 2) = boundaryValue(i)
               this%FixNodeList_Disp((domain_node_offset + NodeList(i))*nd - 1) = boundaryValue(i)
               this%FixNodeList_Disp((domain_node_offset + NodeList(i))*nd - 0) = boundaryValue(i)
            elseif (n == m*nd) then
               this%FixNodeList_Disp((domain_node_offset + NodeList(i))*nd - 2) = boundaryValue(nd*i - 2)
               this%FixNodeList_Disp((domain_node_offset + NodeList(i))*nd - 1) = boundaryValue(nd*i - 1)
               this%FixNodeList_Disp((domain_node_offset + NodeList(i))*nd - 0) = boundaryValue(nd*i - 0)
            else
               print *, "setBoundarySeismicAnalysis >> invalid size(boundaryValue) "
               print *, "it should be 1, 3, size(NodeList) or size(NodeList)*num_dimension"
               stop
            end if
         end do

      case ("traction", "TRACTION", "Neumann", "t", "TractionForce", "Second")
         nd = this%modal%solver%femdomains(1)%femdomainp%nd()
         if (.not. allocated(this%Traction)) then
            this%Traction = zeros(maxval(this%modal%DomainNodeID(:, 2)) &
                                  *nd)
         end if
         domain_node_offset = this%modal%DomainNodeID(DomainID, 1) - 1
         do i = 1, size(NodeList)
            if (nd /= 3) then
               print *, "setBoundarySeismicAnalysis >> only for 3D"
               print *, "Please revise this part for 1/2 D"
               stop
            end if
            n = size(boundaryValue)
            m = size(NodeList)

            if (n == 1) then
               this%Traction((domain_node_offset + NodeList(i))*nd - 2) = boundaryValue(1)
               this%Traction((domain_node_offset + NodeList(i))*nd - 1) = boundaryValue(1)
               this%Traction((domain_node_offset + NodeList(i))*nd - 0) = boundaryValue(1)
            elseif (n == 3) then
               this%Traction((domain_node_offset + NodeList(i))*nd - 2) = boundaryValue(1)
               this%Traction((domain_node_offset + NodeList(i))*nd - 1) = boundaryValue(2)
               this%Traction((domain_node_offset + NodeList(i))*nd - 0) = boundaryValue(3)
            elseif (n == m) then
               this%Traction((domain_node_offset + NodeList(i))*nd - 2) = boundaryValue(i)
               this%Traction((domain_node_offset + NodeList(i))*nd - 1) = boundaryValue(i)
               this%Traction((domain_node_offset + NodeList(i))*nd - 0) = boundaryValue(i)
            elseif (n == m*nd) then
               this%Traction((domain_node_offset + NodeList(i))*nd - 2) = boundaryValue(nd*i - 2)
               this%Traction((domain_node_offset + NodeList(i))*nd - 1) = boundaryValue(nd*i - 1)
               this%Traction((domain_node_offset + NodeList(i))*nd - 0) = boundaryValue(nd*i - 0)
            else
               print *, "setBoundarySeismicAnalysis >> invalid size(boundaryValue) "
               print *, "it should be 1, 3, size(NodeList) or size(NodeList)*num_dimension"
               stop
            end if
         end do

      case ("Absorb", "Absorbing Boundary")
         nd = this%modal%solver%femdomains(1)%femdomainp%nd()
         if (.not. allocated(this%Traction)) then
            this%Traction = zeros(maxval(this%modal%DomainNodeID(:, 2)) &
                                  *nd)
         end if
         domain_node_offset = this%modal%DomainNodeID(DomainID, 1) - 1

         n = size(boundaryValue)
         m = size(NodeList)

         if (n == 2) then
            if (.not. allocated(this%absorbingBoundary_xyz)) then
               this%absorbingBoundary_xyz = NodeList(:)
            else
               if (present(overwrite)) then
                  if (overwrite) then
                     this%absorbingBoundary_xyz = NodeList(:)
                  else
                     this%absorbingBoundary_xyz = this%absorbingBoundary_xyz//NodeList(:)
                  end if
               else
                  this%absorbingBoundary_xyz = this%absorbingBoundary_xyz//NodeList(:)
               end if
               this%absorbingBoundary_elasticity = boundaryValue(1)
               this%absorbingBoundary_viscosity = boundaryValue(2)
            end if

         else
            print *, "setBoundarySeismicAnalysis >> invalid size(boundaryValue) "
            print *, "it should be 2 (viscosity and elasticity)"
            stop
         end if

      case ("A", "Acceleration", "a", "dv/dt", "d^2 u/dt^2")

         nd = this%modal%solver%femdomains(1)%femdomainp%nd()

         if (.not. allocated(this%a_ext)) then
            this%a_ext = zeros(maxval(this%modal%DomainNodeID(:, 2)) &
                               *nd)
         end if
         domain_node_offset = this%modal%DomainNodeID(DomainID, 1) - 1
         do i = 1, size(NodeList)
            if (nd /= 3) then
               print *, "setBoundarySeismicAnalysis >> only for 3D"
               print *, "Please revise this part for 1/2 D"
               stop
            end if
            n = size(boundaryValue)
            m = size(NodeList)

            if (n == 1) then
               this%a_ext((domain_node_offset + NodeList(i))*nd - 2) = boundaryValue(1)
               this%a_ext((domain_node_offset + NodeList(i))*nd - 1) = boundaryValue(1)
               this%a_ext((domain_node_offset + NodeList(i))*nd - 0) = boundaryValue(1)
            elseif (n == 3) then
               this%a_ext((domain_node_offset + NodeList(i))*nd - 2) = boundaryValue(1)
               this%a_ext((domain_node_offset + NodeList(i))*nd - 1) = boundaryValue(2)
               this%a_ext((domain_node_offset + NodeList(i))*nd - 0) = boundaryValue(3)
            elseif (n == m) then
               this%a_ext((domain_node_offset + NodeList(i))*nd - 2) = boundaryValue(i)
               this%a_ext((domain_node_offset + NodeList(i))*nd - 1) = boundaryValue(i)
               this%a_ext((domain_node_offset + NodeList(i))*nd - 0) = boundaryValue(i)
            elseif (n == m*nd) then
               this%a_ext((domain_node_offset + NodeList(i))*nd - 2) = boundaryValue(nd*i - 2)
               this%a_ext((domain_node_offset + NodeList(i))*nd - 1) = boundaryValue(nd*i - 1)
               this%a_ext((domain_node_offset + NodeList(i))*nd - 0) = boundaryValue(nd*i - 0)
            else
               print *, "setBoundarySeismicAnalysis >> invalid size(boundaryValue) "
               print *, "it should be 1, 3, size(NodeList) or size(NodeList)*num_dimension"
               stop
            end if
         end do

         !case("V","Velocity","v","du/dt")

      case default
         print *, "setBoundarySeismicAnalysis >> invalid boundary condition", condition
      end select

   end subroutine
! ##############################################

! ##############################################
   subroutine saveSeismicAnalysis(obj, name, ratio)
      class(SeismicAnalysis_), intent(inout) :: obj
      character(*), intent(in) :: name
      real(real64), optional, intent(in) :: ratio
      real(real64) :: rat
      integer(int32) :: i, j

      rat = input(default=1.0d0, option=ratio)

      do i = 1, obj%femdomain%nn()
         do j = 1, obj%femdomain%nd()
            obj%femdomain%mesh%nodcoord(i, j) = obj%femdomain%mesh%nodcoord(i, j) &
                                                + rat*obj%U(obj%femdomain%nd()*(i - 1) + j)
         end do
      end do

      call obj%femdomain%msh(name=name)

      do i = 1, obj%femdomain%nn()
         do j = 1, obj%femdomain%nd()
            obj%femdomain%mesh%nodcoord(i, j) = obj%femdomain%mesh%nodcoord(i, j) &
                                                - rat*obj%U(obj%femdomain%nd()*(i - 1) + j)
         end do
      end do

      !obj%femdomain%mesh%nodcoord(:,:) = obj%femdomain%mesh%nodcoord(:,:) &
      !    - rat*reshape(obj%U,obj%femdomain%nn(),obj%femdomain%nd() )

   end subroutine
! ##############################################

! ##############################################
   subroutine loadWaveSeismicAnalysis(obj, x_min, x_max, y_min, y_max, z_min, z_max, direction, wavetype)
      class(SeismicAnalysis_), intent(inout) :: obj
      real(real64), optional, intent(in) :: x_min, x_max, y_min, y_max, z_min, z_max
      integer(int32), intent(in) :: wavetype !
      character(1), optional, intent(in) :: direction ! x, y or z
      obj%wavetype = wavetype

      if (obj%wavetype < 0 .or. obj%wavetype > 3) then
         print *, "Invalid loadAs :: WAVE_DISP,WAVE_VELOCITY or WAVE_ACCEL"
         stop
      end if

      if (present(direction)) then
         obj%wavedirection = direction
      end if

      obj%WaveNodeList = obj%femdomain%select( &
                         x_min=x_min, x_max=x_max, y_min=y_min, y_max=y_max, z_min=z_min, z_max=z_max)

   end subroutine
! ##############################################

! ##############################################
   subroutine fixDisplacementSeismicAnalysis(obj, x_min, x_max, y_min, y_max, z_min, z_max, displacement, direction, release)
      class(SeismicAnalysis_), intent(inout) :: obj
      real(real64), optional, intent(in) :: x_min, x_max, y_min, y_max, z_min, z_max, displacement
      character(*), optional, intent(in) :: direction ! x, y or z
      character(*), optional, intent(in) :: release
      real(real64) :: disp
      real(real64), allocatable :: buf(:)

      disp = input(default=0.0d0, option=displacement)

      if (present(release)) then
         if (index(release, "x") /= 0) then
            deallocate (obj%FixNodeList_x)
            deallocate (obj%FixNodeList_Disp_x)
         end if
         if (index(release, "X") /= 0) then
            deallocate (obj%FixNodeList_x)
            deallocate (obj%FixNodeList_Disp_x)
         end if
         if (index(release, "y") /= 0) then
            deallocate (obj%FixNodeList_y)
            deallocate (obj%FixNodeList_Disp_y)
         end if
         if (index(release, "Y") /= 0) then
            deallocate (obj%FixNodeList_y)
            deallocate (obj%FixNodeList_Disp_y)
         end if
         if (index(release, "z") /= 0) then
            deallocate (obj%FixNodeList_z)
            deallocate (obj%FixNodeList_Disp_z)
         end if
         if (index(release, "Z") /= 0) then
            deallocate (obj%FixNodeList_z)
            deallocate (obj%FixNodeList_Disp_z)
         end if
         if (index(release, "all") /= 0) then
            deallocate (obj%FixNodeList_x)
            deallocate (obj%FixNodeList_Disp_x)
            deallocate (obj%FixNodeList_y)
            deallocate (obj%FixNodeList_Disp_y)
            deallocate (obj%FixNodeList_z)
            deallocate (obj%FixNodeList_Disp_z)
         end if
         return
      end if

      if (present(direction)) then
         if (direction == "x" .or. direction == "X") then
            if (allocated(obj%FixNodeList_x)) then
               obj%FixNodeList_x = obj%FixNodeList_x// &
                                   obj%femdomain%select( &
                                   x_min=x_min, x_max=x_max, y_min=y_min, y_max=y_max, z_min=z_min, z_max=z_max)
            else

               obj%FixNodeList_x = &
                  obj%femdomain%select( &
                  x_min=x_min, x_max=x_max, y_min=y_min, y_max=y_max, z_min=z_min, z_max=z_max)
            end if
            buf = zeros(size(obj%FixNodeList_x))
            buf(:) = disp
            if (.not. allocated(obj%FixNodeList_Disp_x)) then
               obj%FixNodeList_Disp_x = buf
            else
               buf(1:size(obj%FixNodeList_Disp_x)) = obj%FixNodeList_Disp_x(:)
            end if
            obj%FixNodeList_Disp_x = buf

         elseif (direction == "y" .or. direction == "Y") then
            if (allocated(obj%FixNodeList_y)) then
               obj%FixNodeList_y = obj%FixNodeList_y// &
                                   obj%femdomain%select( &
                                   x_min=x_min, x_max=x_max, y_min=y_min, y_max=y_max, z_min=z_min, z_max=z_max)
            else

               obj%FixNodeList_y = &
                  obj%femdomain%select( &
                  x_min=x_min, x_max=x_max, y_min=y_min, y_max=y_max, z_min=z_min, z_max=z_max)
            end if
            buf = zeros(size(obj%FixNodeList_y))
            buf(:) = disp
            if (.not. allocated(obj%FixNodeList_Disp_y)) then
            else
               buf(1:size(obj%FixNodeList_Disp_y)) = obj%FixNodeList_Disp_y(:)
            end if
            obj%FixNodeList_Disp_y = buf

         elseif (direction == "z" .or. direction == "Z") then
            if (allocated(obj%FixNodeList_z)) then
               obj%FixNodeList_z = obj%FixNodeList_z// &
                                   obj%femdomain%select( &
                                   x_min=x_min, x_max=x_max, y_min=y_min, y_max=y_max, z_min=z_min, z_max=z_max)
            else

               obj%FixNodeList_z = &
                  obj%femdomain%select( &
                  x_min=x_min, x_max=x_max, y_min=y_min, y_max=y_max, z_min=z_min, z_max=z_max)
            end if
            buf = zeros(size(obj%FixNodeList_z))
            buf(:) = disp

            if (.not. allocated(obj%FixNodeList_Disp_z)) then
            else
               buf(1:size(obj%FixNodeList_Disp_z)) = obj%FixNodeList_Disp_z(:)
            end if
            obj%FixNodeList_Disp_z = buf

         elseif (direction == "all" .or. direction == "ALL") then

            if (allocated(obj%FixNodeList_x)) then
               obj%FixNodeList_x = obj%FixNodeList_x// &
                                   obj%femdomain%select( &
                                   x_min=x_min, x_max=x_max, y_min=y_min, y_max=y_max, z_min=z_min, z_max=z_max)
            else

               obj%FixNodeList_x = &
                  obj%femdomain%select( &
                  x_min=x_min, x_max=x_max, y_min=y_min, y_max=y_max, z_min=z_min, z_max=z_max)
            end if
            buf = zeros(size(obj%FixNodeList_x))
            buf(:) = disp

            if (.not. allocated(obj%FixNodeList_Disp_x)) then
            else
               buf(1:size(obj%FixNodeList_Disp_x)) = obj%FixNodeList_Disp_x(:)
            end if
            obj%FixNodeList_Disp_x = buf

            if (allocated(obj%FixNodeList_y)) then
               obj%FixNodeList_y = obj%FixNodeList_y// &
                                   obj%femdomain%select( &
                                   x_min=x_min, x_max=x_max, y_min=y_min, y_max=y_max, z_min=z_min, z_max=z_max)
            else

               obj%FixNodeList_y = &
                  obj%femdomain%select( &
                  x_min=x_min, x_max=x_max, y_min=y_min, y_max=y_max, z_min=z_min, z_max=z_max)
            end if
            buf = size(zeros(size(obj%FixNodeList_y)))
            buf(:) = disp
            if (.not. allocated(obj%FixNodeList_Disp_y)) then
            else
               buf(1:size(obj%FixNodeList_Disp_y)) = obj%FixNodeList_Disp_y(:)
            end if
            obj%FixNodeList_Disp_y = buf

            if (allocated(obj%FixNodeList_z)) then
               obj%FixNodeList_z = obj%FixNodeList_z// &
                                   obj%femdomain%select( &
                                   x_min=x_min, x_max=x_max, y_min=y_min, y_max=y_max, z_min=z_min, z_max=z_max)
            else

               obj%FixNodeList_z = &
                  obj%femdomain%select( &
                  x_min=x_min, x_max=x_max, y_min=y_min, y_max=y_max, z_min=z_min, z_max=z_max)
            end if
            buf = zeros(size(obj%FixNodeList_z))
            buf(:) = disp

            if (.not. allocated(obj%FixNodeList_Disp_z)) then
            else
               buf(1:size(obj%FixNodeList_Disp_z)) = obj%FixNodeList_Disp_z(:)
            end if
            obj%FixNodeList_Disp_z = buf
         else
            print *, "ERROR :: loadWaveSeismicAnalysis >> direction should be x, y or z"
            stop
         end if
      else
         if (allocated(obj%FixNodeList_x)) then
            obj%FixNodeList_x = obj%FixNodeList_x// &
                                obj%femdomain%select( &
                                x_min=x_min, x_max=x_max, y_min=y_min, y_max=y_max, z_min=z_min, z_max=z_max)
         else

            obj%FixNodeList_x = &
               obj%femdomain%select( &
               x_min=x_min, x_max=x_max, y_min=y_min, y_max=y_max, z_min=z_min, z_max=z_max)
         end if
         buf = zeros(size(obj%FixNodeList_x))
         buf(:) = disp

         if (.not. allocated(obj%FixNodeList_Disp_x)) then
         else
            buf(1:size(obj%FixNodeList_Disp_x)) = obj%FixNodeList_Disp_x(:)
         end if
         obj%FixNodeList_Disp_x = buf

         if (allocated(obj%FixNodeList_y)) then
            obj%FixNodeList_y = obj%FixNodeList_y// &
                                obj%femdomain%select( &
                                x_min=x_min, x_max=x_max, y_min=y_min, y_max=y_max, z_min=z_min, z_max=z_max)
         else

            obj%FixNodeList_y = &
               obj%femdomain%select( &
               x_min=x_min, x_max=x_max, y_min=y_min, y_max=y_max, z_min=z_min, z_max=z_max)
         end if
         buf = size(zeros(size(obj%FixNodeList_y)))
         buf(:) = disp
         if (.not. allocated(obj%FixNodeList_Disp_y)) then
         else
            buf(1:size(obj%FixNodeList_Disp_y)) = obj%FixNodeList_Disp_y(:)
         end if
         obj%FixNodeList_Disp_y = buf

         if (allocated(obj%FixNodeList_z)) then
            obj%FixNodeList_z = obj%FixNodeList_z// &
                                obj%femdomain%select( &
                                x_min=x_min, x_max=x_max, y_min=y_min, y_max=y_max, z_min=z_min, z_max=z_max)
         else

            obj%FixNodeList_z = &
               obj%femdomain%select( &
               x_min=x_min, x_max=x_max, y_min=y_min, y_max=y_max, z_min=z_min, z_max=z_max)
         end if
         buf = zeros(size(obj%FixNodeList_z))
         buf(:) = disp

         if (.not. allocated(obj%FixNodeList_Disp_z)) then
         else
            buf(1:size(obj%FixNodeList_Disp_z)) = obj%FixNodeList_Disp_z(:)
         end if
         obj%FixNodeList_Disp_z = buf

      end if
   end subroutine
! ##############################################

   subroutine updateWaveSeismicAnalysis(obj, timestep, direction)
      class(SeismicAnalysis_), intent(inout) :: obj
      integer(int32), intent(in) :: timestep
      character(1), optional, intent(in) :: direction ! x, y or z
      integer(int32) :: node_id, i, dir, dim_num

      if (present(direction)) then
         obj%wavedirection = direction
      end if
      if (obj%wavedirection == "x" .or. obj%wavedirection == "X") then
         dir = 1
      end if
      if (obj%wavedirection == "y" .or. obj%wavedirection == "Y") then
         dir = 2
      end if
      if (obj%wavedirection == "z" .or. obj%wavedirection == "Z") then
         dir = 3
      end if

      if (.not. allocated(obj%WaveNodeList)) then
         print *, "Caution >> updateWaveSeismicAnalysis >> no wave"
      end if

      dim_num = obj%femdomain%nd()
      if (obj%wavetype == WAVE_ACCEL) then
         obj%A_ext_n = obj%A_ext
         do i = 1, size(obj%WaveNodeList)
            node_id = obj%WaveNodeList(i)
            obj%A(dim_num*(node_id - 1) + dir) = obj%wave(timestep, 2)
            ! update accel
            obj%A_ext(dim_num*(node_id - 1) + dir) = obj%wave(timestep, 2)
         end do

      elseif (obj%wavetype == WAVE_VELOCITY) then
         do i = 1, size(obj%WaveNodeList)
            node_id = obj%WaveNodeList(i)
            obj%V(dim_num*(node_id - 1) + dir) = obj%wave(timestep, 2)
         end do
      elseif (obj%wavetype == WAVE_DISP) then
         do i = 1, size(obj%WaveNodeList)
            node_id = obj%WaveNodeList(i)
            obj%U(dim_num*(node_id - 1) + dir) = obj%wave(timestep, 2)
         end do
      end if

   end subroutine

! ##############################################
   subroutine runSeismicAnalysis(obj, t0, timestep, wave, AccelLimit, disp_magnify_ratio, use_same_stiffness, &
                                 dt, timeIntegral, use_same_matrix, preconditioning)
      class(SeismicAnalysis_), intent(inout) :: obj
      ! >> for multi-domain
      real(real64), optional, intent(in) :: dt
      logical, optional, intent(in) :: use_same_matrix

      character(*), intent(in) :: timeIntegral
      character(*), optional, intent(in) :: preconditioning ! preconditioning algorithm for linear solver
      ! <<

      ! >> for single-domain
      integer(int32), optional, intent(in) :: timestep(2)
      logical, optional, intent(in) :: use_same_stiffness! Use A' for all t_n, A'x=b
      real(real64), optional, intent(in) :: t0, disp_magnify_ratio
      real(real64), optional, intent(in) :: wave(:, :), AccelLimit
      real(real64), allocatable :: mass_diag(:), R(:), boundary_force(:), F_vec(:), new_U(:), new_V(:), new_A(:)
      real(real64), allocatable :: diag(:), bar_A(:), bar_V(:), K_inv_F(:)

      integer(int32), allocatable :: fix_idx(:)
      real(real64), allocatable :: fix_val(:)
      ! <<<

      type(LinearSolver_) :: solver
      type(IO_) :: U, V, A
      type(CRS_) :: M_matrix, K_matrix, A_matrix, crs
      type(Math_) :: math
      complex(real64) :: alpha

      integer(int32) :: i, j
      real(real64) :: ratio, h

      if (obj%multi_domain_mode) then
         if (.not. present(dt)) then
            print *, "Need real(real64) :: dt for multi-domain"
            stop
         end if

         select case (timeIntegral)
         case ("Exponential-Integrator", "AEI", "EI", "WKF")
            ! Use augmented exponential integrator (Tomobe, in prep.)

            ! load last values
            if (.not. allocated(obj%a_n)) then
               obj%a_n = obj%a
            end if
            if (.not. allocated(obj%v_n)) then
               obj%v_n = obj%v
            end if
            if (.not. allocated(obj%u_n)) then
               obj%u_n = obj%u
            end if

            obj%modal%solver%CRS_val = 0.0d0

            ! multiplication/summersion of CRS matrices

            if (present(use_same_matrix)) then
               if (use_same_matrix) then
                  if (.not. allocated(obj%M_matrix%val)) then
                     print *, "[ok] use_same_matrix >> enabled "
                     call obj%modal%solve(only_matrix=.true., penalty=obj%overset_penalty)
                     obj%M_matrix = obj%modal%solver%getCRS("B")
                     obj%K_matrix = obj%modal%solver%getCRS("A")
                     M_matrix = obj%M_matrix
                     K_matrix = obj%K_matrix
                  else
                     print *, "[ok] use_same_matrix >> active "
                     M_matrix = obj%M_matrix
                     K_matrix = obj%K_matrix
                  end if

               else
                  call obj%modal%solve(only_matrix=.true., penalty=obj%overset_penalty)
                  M_matrix = obj%modal%solver%getCRS("B")
                  K_matrix = obj%modal%solver%getCRS("A")
               end if
            else
               call obj%modal%solve(only_matrix=.true., penalty=obj%overset_penalty)
               M_matrix = obj%modal%solver%getCRS("B")
               K_matrix = obj%modal%solver%getCRS("A")
            end if

            crs = K_matrix%divide_by(diag_vector=M_matrix%diag(cell_centered=.true.))
            F_vec = obj%Traction + M_matrix%matmul(obj%A_ext) &
                    + obj%getAbsorbingBoundaryForce()

            h = obj%damping_ratio_h

            fix_idx = obj%modal%solver%get_fix_idx()
            fix_val = obj%modal%solver%get_fix_value()

            ! under revision
            ! no boundary conditions are considered.

            obj%U = crs%tensor_wave_kernel(u0=obj%u_n, v0=obj%v_n, &
                                           h=h, t=dt, itrmax=100)

            ! how to update velocity and acceleration
            obj%V = crs%tensor_wave_kernel( &
                    u0=-h*dt*obj%u_n + obj%v_n, &
                    v0=-crs%matmul(obj%u_n) - h*dt*obj%v_n, &
                    h=h, t=dt, itrmax=100)

            obj%A = crs%tensor_wave_kernel( &
                    u0=-h*dt*(-h*dt*obj%u_n + obj%v_n) + (-crs%matmul(obj%u_n) - h*dt*obj%v_n), &
                    v0=-crs%matmul(-h*dt*obj%u_n + obj%v_n) - h*dt*(-crs%matmul(obj%u_n) - h*dt*obj%v_n), &
                    h=h, t=dt, itrmax=100)

            obj%V_n = obj%V
            obj%U_n = obj%U
            obj%A_n = obj%A

         case ("Nemwark-beta", "Nemwark-Beta")

            if (.not. allocated(obj%a_n)) then
               obj%a_n = obj%a
            end if
            if (.not. allocated(obj%v_n)) then
               obj%v_n = obj%v
            end if
            if (.not. allocated(obj%u_n)) then
               obj%u_n = obj%u
            end if

            obj%modal%solver%CRS_val = 0.0d0

            ! multiplication/summersion of CRS matrices

            if (present(use_same_matrix)) then
               if (use_same_matrix) then
                  if (.not. allocated(obj%M_matrix%val)) then
                     print *, "[ok] use_same_matrix >> enabled "
                     call obj%modal%solve(only_matrix=.true., penalty=obj%overset_penalty)
                     obj%M_matrix = obj%modal%solver%getCRS("B")
                     obj%K_matrix = obj%modal%solver%getCRS("A")
                     M_matrix = obj%M_matrix
                     K_matrix = obj%K_matrix
                  else
                     print *, "[ok] use_same_matrix >> active "
                     M_matrix = obj%M_matrix
                     K_matrix = obj%K_matrix
                  end if

               else
                  call obj%modal%solve(only_matrix=.true., penalty=obj%overset_penalty)
                  M_matrix = obj%modal%solver%getCRS("B")
                  K_matrix = obj%modal%solver%getCRS("A")
               end if
            else
               call obj%modal%solve(only_matrix=.true., penalty=obj%overset_penalty)
               M_matrix = obj%modal%solver%getCRS("B")
               K_matrix = obj%modal%solver%getCRS("A")
            end if

            A_matrix = (1.0d0/dt/dt/obj%Newmark_beta + &
                        obj%Newmark_gamma/dt/obj%Newmark_beta*obj%alpha)*M_matrix &
                       + (obj%Newmark_gamma/dt/obj%Newmark_beta*obj%beta + 1.0d0)*K_matrix

            bar_A = -1.0d0/dt/obj%Newmark_beta*obj%V_n(:) - 1.0d0/2.0d0/obj%Newmark_beta &
                    *(1.0d0 - 2.0d0*obj%Newmark_beta)*obj%A_n(:)

            bar_V = obj%V_n + dt*(1.0d0 - obj%Newmark_gamma)*obj%A_n &
                    - obj%Newmark_gamma/obj%Newmark_beta*obj%V_n - dt*obj%Newmark_gamma/2.0d0 &
                    /obj%Newmark_beta*(1.0d0 - 2.0d0*obj%Newmark_beta)*obj%A_n

            F_vec = obj%Traction + M_matrix%matmul(obj%A_ext) &
                    + obj%getAbsorbingBoundaryForce() &
                    - K_matrix%matmul(obj%U_n) &
                    - M_matrix%matmul(bar_A) &
                    - obj%alpha*M_matrix%matmul(bar_V) &
                    - obj%beta*K_matrix%matmul(bar_V)
!debug
            call obj%modal%solver%setCRS(A_matrix)
            call obj%modal%solver%setRHS(F_vec)

            new_U = obj%modal%solver%solve(x0=obj%U, preconditioning=preconditioning)

            New_A = 1.0d0/dt/dt/obj%Newmark_beta*new_U + bar_A

            New_V = obj%newmark_gamma/dt/obj%newmark_beta*new_U + bar_V

            new_U = new_U + obj%U_n

            obj%U = new_U
            obj%U_n = obj%U

            obj%V = new_V
            obj%V_n = obj%V

            obj%A = new_A
            obj%A_n = obj%A

         case ("RK4")
            print *, "[STOP] Forward Euler >> Buggy"
            stop
            if (.not. allocated(obj%a_n)) then
               obj%a_n = obj%a
            end if
            if (.not. allocated(obj%v_n)) then
               obj%v_n = obj%v
            end if

            if (.not. allocated(obj%a_half)) then
               obj%a_half = obj%a_n
            end if
            if (.not. allocated(obj%v_half)) then
               obj%v_half = obj%v_n
            end if

            if (.not. allocated(obj%U_n)) then
               obj%U_n = obj%U
            end if

            call obj%modal%solve(only_matrix=.true.)
            ! multiplication/summersion of CRS matrices
            M_matrix = obj%modal%solver%getCRS("B")
            K_matrix = obj%modal%solver%getCRS("A")
            A_matrix = (obj%beta*6.0d0/dt + 1.0d0)*K_matrix + &
                       (36.0d0/dt/dt + obj%alpha*6.0d0/dt)*M_matrix

            F_vec = obj%Traction + M_matrix%matmul(obj%A_ext) &
                    + obj%getAbsorbingBoundaryForce() &
                    + (36.0d0/dt/dt + obj%alpha*6.0d0/dt)*M_matrix%matmul(obj%u_n) &
                    + obj%beta*6.0d0/dt*K_matrix%matmul(obj%u_n) &
                    + (12.0d0/dt + obj%alpha)*M_matrix%matmul(obj%v_n) &
                    + obj%beta*K_matrix%matmul(obj%v_n) &
                    + (12.0d0/dt + 2.0d0*obj%alpha)*M_matrix%matmul(obj%v_half) &
                    + (2.0d0*obj%beta)*K_matrix%matmul(obj%v_half) &
                    + 2.0d0*M_matrix%matmul(obj%a_n) &
                    + 2.0d0*M_matrix%matmul(obj%a_half)
            ! diag

            call obj%modal%solver%setCRS(A_matrix)
            call obj%modal%solver%setRHS(F_vec)

            new_U = obj%modal%solver%solve(x0=obj%U_n, preconditioning=preconditioning)

            new_V = 6.0d0/dt*new_U - 6.0d0/dt*obj%U_n - (obj%v_n + 2.0d0*obj%v_half)

            new_A = 6.0d0/dt*new_V - 6.0d0/dt*obj%V_n - (obj%a_n + 2.0d0*obj%a_half)

            ! update valiables
            obj%U_n = new_U
            obj%U = new_U

            obj%V_n = obj%V_half
            obj%V_half = obj%V
            obj%V = new_V

            obj%A_n = obj%A_half
            obj%A_half = obj%A
            obj%A = new_A

         end select
      else
         ratio = input(default=1.0d0, option=disp_magnify_ratio)
         if (present(wave)) then
            obj%wave = wave
         end if

         do i = timestep(1), timestep(2) - 1
            ! update dt
            obj%dt = abs(obj%wave(i + 1, 1) - obj%wave(i, 1))

            ! update time
            obj%step = i
            obj%t = obj%dt*obj%Step
            call obj%updateWave(timestep=obj%step + 1)
            ! show info.
            call print("SeismicAnalysis >> "//str(obj%t - obj%dt)//"< t <"//str(obj%t)//" sec.")

            ! solve Linear-ElastoDynamic problem with Reyleigh dumping and Newmark Beta
            if (present(AccelLimit)) then
               if (maxval(obj%A) >= AccelLimit) then
                  print *, "[Caution] :: runSeismicAnalysis >> exceeds AccelLimit!"
                  return
               end if
            end if

            call obj%LinearReyleighNewmark()

            call obj%recordMaxValues()
            ! Export results
            call obj%save("step_"//str(obj%step), ratio=ratio)

         end do
      end if

   end subroutine
! ##############################################

! ##############################################
   subroutine runSeismicAnalysis_user_function(obj, t0, timestep, wave, AccelLimit, disp_magnify_ratio, use_same_stiffness, &
                                               dt, timeIntegral, use_same_matrix, LinearSolver)
      class(SeismicAnalysis_), intent(inout) :: obj
      ! >> for multi-domain
      real(real64), optional, intent(in) :: dt
      logical, optional, intent(in) :: use_same_matrix

      character(*), intent(in) :: timeIntegral

      ! CRS formatted Linear solver
      interface
         subroutine LinearSolver(row_ptr, col_idx, val, rhs, x)
            use iso_fortran_env
            implicit none
            real(real64), intent(in) :: val(:), rhs(:)
            real(real64), intent(inout) :: x(:)
            integer(int32), intent(in) :: col_idx(:)
            integer(int64), intent(in) :: row_ptr(:)

         end subroutine
      end interface

      ! <<

      ! >> for single-domain
      integer(int32), optional, intent(in) :: timestep(2)
      logical, optional, intent(in) :: use_same_stiffness! Use A' for all t_n, A'x=b
      real(real64), optional, intent(in) :: t0, disp_magnify_ratio
      real(real64), optional, intent(in) :: wave(:, :), AccelLimit
      real(real64), allocatable :: mass_diag(:), R(:), boundary_force(:), F_vec(:), new_U(:), new_V(:), new_A(:)
      real(real64), allocatable :: diag(:), bar_A(:), bar_V(:)
      ! <<<

      type(LinearSolver_) :: solver
      type(IO_) :: U, V, A
      type(CRS_) :: M_matrix, K_matrix, A_matrix

      integer(int32) :: i, j
      real(real64) :: ratio

      if (obj%multi_domain_mode) then
         if (.not. present(dt)) then
            print *, "Need real(real64) :: dt for multi-domain"
            stop
         end if

         select case (timeIntegral)
         case ("Nemwark-beta", "Nemwark-Beta")

            if (.not. allocated(obj%a_n)) then
               obj%a_n = obj%a
            end if
            if (.not. allocated(obj%v_n)) then
               obj%v_n = obj%v
            end if
            if (.not. allocated(obj%u_n)) then
               obj%u_n = obj%u
            end if

            obj%modal%solver%CRS_val = 0.0d0

            ! multiplication/summersion of CRS matrices

            if (present(use_same_matrix)) then
               if (use_same_matrix) then
                  if (.not. allocated(obj%M_matrix%val)) then
                     print *, "[ok] use_same_matrix >> enabled "
                     call obj%modal%solve(only_matrix=.true., penalty=obj%overset_penalty)
                     obj%M_matrix = obj%modal%solver%getCRS("B")
                     obj%K_matrix = obj%modal%solver%getCRS("A")
                     M_matrix = obj%M_matrix
                     K_matrix = obj%K_matrix
                  else
                     print *, "[ok] use_same_matrix >> active "
                     M_matrix = obj%M_matrix
                     K_matrix = obj%K_matrix
                  end if

               else
                  call obj%modal%solve(only_matrix=.true., penalty=obj%overset_penalty)
                  M_matrix = obj%modal%solver%getCRS("B")
                  K_matrix = obj%modal%solver%getCRS("A")
               end if
            else
               call obj%modal%solve(only_matrix=.true., penalty=obj%overset_penalty)
               M_matrix = obj%modal%solver%getCRS("B")
               K_matrix = obj%modal%solver%getCRS("A")
            end if

            A_matrix = (1.0d0/dt/dt/obj%Newmark_beta + &
                        obj%Newmark_gamma/dt/obj%Newmark_beta*obj%alpha)*M_matrix &
                       + (obj%Newmark_gamma/dt/obj%Newmark_beta*obj%beta + 1.0d0)*K_matrix

            bar_A = -1.0d0/dt/obj%Newmark_beta*obj%V_n(:) - 1.0d0/2.0d0/obj%Newmark_beta &
                    *(1.0d0 - 2.0d0*obj%Newmark_beta)*obj%A_n(:)

            bar_V = obj%V_n + dt*(1.0d0 - obj%Newmark_gamma)*obj%A_n &
                    - obj%Newmark_gamma/obj%Newmark_beta*obj%V_n - dt*obj%Newmark_gamma/2.0d0 &
                    /obj%Newmark_beta*(1.0d0 - 2.0d0*obj%Newmark_beta)*obj%A_n

            F_vec = obj%Traction + M_matrix%matmul(obj%A_ext) &
                    + obj%getAbsorbingBoundaryForce() &
                    - K_matrix%matmul(obj%U_n) &
                    - M_matrix%matmul(bar_A) &
                    - obj%alpha*M_matrix%matmul(bar_V) &
                    - obj%beta*K_matrix%matmul(bar_V)
!debug
            call obj%modal%solver%setCRS(A_matrix)
            call obj%modal%solver%setRHS(F_vec)

            new_U = obj%modal%solver%solve(x0=obj%U, LinearSolver=LinearSolver)

            New_A = 1.0d0/dt/dt/obj%Newmark_beta*new_U + bar_A

            New_V = obj%newmark_gamma/dt/obj%newmark_beta*new_U + bar_V

            new_U = new_U + obj%U_n

            obj%U = new_U
            obj%U_n = obj%U

            obj%V = new_V
            obj%V_n = obj%V

            obj%A = new_A
            obj%A_n = obj%A

         case ("RK4")
            print *, "[STOP] Forward Euler >> Buggy"
            stop
            if (.not. allocated(obj%a_n)) then
               obj%a_n = obj%a
            end if
            if (.not. allocated(obj%v_n)) then
               obj%v_n = obj%v
            end if

            if (.not. allocated(obj%a_half)) then
               obj%a_half = obj%a_n
            end if
            if (.not. allocated(obj%v_half)) then
               obj%v_half = obj%v_n
            end if

            if (.not. allocated(obj%U_n)) then
               obj%U_n = obj%U
            end if

            call obj%modal%solve(only_matrix=.true.)
            ! multiplication/summersion of CRS matrices
            M_matrix = obj%modal%solver%getCRS("B")
            K_matrix = obj%modal%solver%getCRS("A")
            A_matrix = (obj%beta*6.0d0/dt + 1.0d0)*K_matrix + &
                       (36.0d0/dt/dt + obj%alpha*6.0d0/dt)*M_matrix

            F_vec = obj%Traction + M_matrix%matmul(obj%A_ext) &
                    + obj%getAbsorbingBoundaryForce() &
                    + (36.0d0/dt/dt + obj%alpha*6.0d0/dt)*M_matrix%matmul(obj%u_n) &
                    + obj%beta*6.0d0/dt*K_matrix%matmul(obj%u_n) &
                    + (12.0d0/dt + obj%alpha)*M_matrix%matmul(obj%v_n) &
                    + obj%beta*K_matrix%matmul(obj%v_n) &
                    + (12.0d0/dt + 2.0d0*obj%alpha)*M_matrix%matmul(obj%v_half) &
                    + (2.0d0*obj%beta)*K_matrix%matmul(obj%v_half) &
                    + 2.0d0*M_matrix%matmul(obj%a_n) &
                    + 2.0d0*M_matrix%matmul(obj%a_half)
            ! diag

            call obj%modal%solver%setCRS(A_matrix)
            call obj%modal%solver%setRHS(F_vec)

            new_U = obj%modal%solver%solve(x0=obj%U_n, LinearSolver=LinearSolver)

            new_V = 6.0d0/dt*new_U - 6.0d0/dt*obj%U_n - (obj%v_n + 2.0d0*obj%v_half)

            new_A = 6.0d0/dt*new_V - 6.0d0/dt*obj%V_n - (obj%a_n + 2.0d0*obj%a_half)

            ! update valiables
            obj%U_n = new_U
            obj%U = new_U

            obj%V_n = obj%V_half
            obj%V_half = obj%V
            obj%V = new_V

            obj%A_n = obj%A_half
            obj%A_half = obj%A
            obj%A = new_A

         end select
      else
         ratio = input(default=1.0d0, option=disp_magnify_ratio)
         if (present(wave)) then
            obj%wave = wave
         end if

         do i = timestep(1), timestep(2) - 1
            ! update dt
            obj%dt = abs(obj%wave(i + 1, 1) - obj%wave(i, 1))

            ! update time
            obj%step = i
            obj%t = obj%dt*obj%Step
            call obj%updateWave(timestep=obj%step + 1)
            ! show info.
            call print("SeismicAnalysis >> "//str(obj%t - obj%dt)//"< t <"//str(obj%t)//" sec.")

            ! solve Linear-ElastoDynamic problem with Reyleigh dumping and Newmark Beta
            if (present(AccelLimit)) then
               if (maxval(obj%A) >= AccelLimit) then
                  print *, "[Caution] :: runSeismicAnalysis >> exceeds AccelLimit!"
                  return
               end if
            end if

            call obj%LinearReyleighNewmark()

            call obj%recordMaxValues()
            ! Export results
            call obj%save("step_"//str(obj%step), ratio=ratio)

         end do
      end if

   end subroutine
! ##############################################

! ##############################################
   subroutine LinearReyleighNewmarkSeismicAnalysis(obj, TOL)
      class(SeismicAnalysis_), intent(inout) :: obj
      type(LinearSolver_) :: solver
      type(IO_) :: f
      real(real64), allocatable :: M_ij(:, :)
      real(real64), allocatable :: C_ij(:, :)
      real(real64), allocatable :: K_ij(:, :)
      real(real64), allocatable :: F_i(:)
      real(real64), allocatable :: dF_i(:)
      real(real64), allocatable :: R_i(:)
      real(real64), allocatable :: U_i(:)
      real(real64), allocatable ::dU_i(:)
      real(real64), allocatable :: V_i(:)
      real(real64), allocatable ::dV_i(:)
      real(real64), allocatable :: A_i(:)
      real(real64), allocatable :: A_ext_i(:)
      real(real64), allocatable :: A_ext_i_n(:)
      real(real64), allocatable ::dA(:)
      real(real64), allocatable ::dV(:)
      real(real64), allocatable ::dU(:)

      real(real64), allocatable ::u_upd(:)
      real(real64), allocatable ::v_upd(:)
      real(real64), allocatable ::a_upd(:)

      real(real64), allocatable ::U_n(:)
      real(real64), allocatable ::V_n(:)
      real(real64), allocatable ::A_n(:)

      real(real64), allocatable :: A_ij(:, :)
      integer(int32) :: i, j, k, l, m, dim_num, n
      integer(int32), allocatable :: FixNodeList(:), DomainIDs(:)
      real(real64), allocatable   :: Coordinate(:, :)
      real(real64), optional, intent(in) :: TOL
      real(real64) :: TOL_seismic, center_accel(3), rho, gravity(3), a(0:7)

      gravity(:) = 0.0d0
      gravity(3) = -9.81d0

      TOL_seismic = input(default=dble(1.0e-14), option=TOL)

      dim_num = size(obj%femdomain%mesh%nodcoord, 2)
      n = dim_num*size(obj%femdomain%mesh%nodcoord, 1)
      if (.not. allocated(obj%a)) then
         allocate (obj%a(n))
      end if
      if (.not. allocated(obj%v)) then
         allocate (obj%v(n))
      end if
      if (.not. allocated(obj%u)) then
         allocate (obj%u(n))
      end if

      ! Element matrix
      call solver%init(NumberOfNode=[obj%femdomain%nn()], DOF=3)
      obj%dt = abs(obj%dt)

      if (obj%debug) then
         print *, '[Seismic] Creating Element Matrices...'
      end if
      do i = 1, obj%femdomain%ne()
         ! For each element
         ! Ax=b will be installed into solv -obj%A_ext_n)
         rho = obj%Density(i)
         M_ij = obj%femdomain%MassMatrix(ElementID=i, DOF=obj%femdomain%nd(), density=rho)
         K_ij = obj%femdomain%StiffnessMatrix(ElementID=i, E=obj%YoungModulus(i), v=obj%PoissonRatio(i))
         C_ij = obj%alpha*M_ij + obj%beta*K_ij
         U_i = obj%femdomain%ElementVector(ElementID=i, GlobalVector=obj%U, DOF=obj%femdomain%nd())
         V_i = obj%femdomain%ElementVector(ElementID=i, GlobalVector=obj%V, DOF=obj%femdomain%nd())
         A_i = obj%femdomain%ElementVector(ElementID=i, GlobalVector=obj%A, DOF=obj%femdomain%nd())
         A_ext_i = obj%femdomain%ElementVector(ElementID=i, GlobalVector=obj%A_ext, DOF=obj%femdomain%nd())
         A_ext_i_n = obj%femdomain%ElementVector(ElementID=i, GlobalVector=obj%A_ext_n, DOF=obj%femdomain%nd())

         dim_num = obj%femdomain%nd()

         F_i = matmul(M_ij, A_ext_i)

         A_ij = obj%getNewmarkBetaMatrix(M=M_ij, C=C_ij, K=K_ij, &
                                         beta=obj%Newmark_beta, gamma=obj%Newmark_gamma, dt=obj%dt)
         R_i = obj%getNewmarkBetaVector(M=M_ij, C=C_ij, u_n=U_i, v_n=V_i, a_n=A_i, &
                                        force=F_i, beta=obj%Newmark_beta, gamma=obj%Newmark_gamma, dt=obj%dt)
         !print *, maxval(A_ij),minval(A_ij)
         !print *, maxval(R_i),minval(R_i)
         !stop
         ! Assemble stiffness matrix
         DomainIDs = zeros(obj%femdomain%nne())
         DomainIDs(:) = 1

         call solver%assemble( &
            connectivity=obj%femdomain%connectivity(ElementID=i), &
            DOF=obj%femdomain%nd(), &
            DomainIDs=DomainIDs, &
            eMatrix=A_ij)

         call solver%assemble( &
            connectivity=obj%femdomain%connectivity(ElementID=i), &
            DOF=obj%femdomain%nd(), &
            DomainIDs=DomainIDs, &
            eVector=R_i)
      end do

      ! absorbing boundary
      if (obj%debug) then
         print *, '[Seismic] Setting absorbing boundaries...'
         print *, size(obj%absorbingBoundary_x)
         print *, size(obj%absorbingBoundary_y)
         print *, size(obj%absorbingBoundary_z)
      end if
      ! setup absorbing boundary

      ! as dumper
      !    ----
      ! --- |  |----
      !    ----
      ! with dumping ratio = obj%boundary_dumping_ratio: C
      ! F_i  = C v_i

      solver%b(:) = solver%b(:) + obj%getAbsorbingBoundaryForce()

      if (obj%debug) then
         print *, '[Seismic] Fixing Boundaries...'
         print *, size(obj%FixNodeList_x)
         print *, size(obj%FixNodeList_y)
         print *, size(obj%FixNodeList_z)

      end if
      solver%debug = obj%debug
      if (allocated(obj%FixNodeList_x)) then
         do i = 1, size(obj%FixNodeList_x)
            call solver%fix(NodeID=obj%FixNodeList_x(i)*3 - 2, &
                            entryvalue=obj%FixNodeList_Disp_x(i), &
                            row_DomainID=1, &
                            debug=solver%debug)
         end do
      end if
      if (allocated(obj%FixNodeList_y)) then
         do i = 1, size(obj%FixNodeList_y)
            call solver%fix(NodeID=obj%FixNodeList_y(i)*3 - 1, &
                            entryvalue=obj%FixNodeList_Disp_y(i), &
                            row_DomainID=1, &
                            debug=solver%debug)
         end do
      end if
      if (allocated(obj%FixNodeList_z)) then
         do i = 1, size(obj%FixNodeList_z)
            call solver%fix(NodeID=obj%FixNodeList_z(i)*3, &
                            entryvalue=obj%FixNodeList_Disp_z(i), &
                            row_DomainID=1, &
                            debug=solver%debug)
         end do
      end if
      ! Now [A] {du} = {R} is ready
      ! Solve

      if (obj%debug) then
         print *, '[Seismic] Solving...'
      end if
      call solver%solve("BiCGSTAB")

!    print *, maxval(solver%val),minval(solver%val)
!    print *, maxval(solver%x),minval(solver%x)

      u_upd = solver%x
      v_upd = obj%updateVelocityNewmarkBeta(u=u_upd, u_n=obj%U, v_n=obj%V, a_n=obj%A, &
                                            gamma=obj%newmark_gamma, beta=obj%newmark_beta, dt=obj%dt)
      a_upd = obj%updateAccelNewmarkBeta(u=u_upd, u_n=obj%U, v_n=obj%V, a_n=obj%A, &
                                         gamma=obj%newmark_gamma, beta=obj%newmark_beta, dt=obj%dt)

      obj%U = u_upd
      obj%V = v_upd
      obj%A = a_upd

      print *, "U"
      print *, minval(obj%U), maxval(obj%U)

      print *, "V"
      print *, minval(obj%V), maxval(obj%V)

      print *, "A"
      print *, minval(obj%A), maxval(obj%A)

   end subroutine
! ##############################################

   subroutine recordMaxValuesSeismicAnalysis(obj)
      class(SeismicAnalysis_), intent(inout) :: Obj
      real(real64), allocatable :: array(:, :)
      integer(int32) :: i

      array = reshape(obj%U, obj%femdomain%nn(), obj%femdomain%nd())
      array = abs(array)
      do i = 1, 3
         obj%maxU(i) = maxval([abs(obj%maxU(i)), maxval(array(:, i))])
      end do

      array = reshape(obj%V, obj%femdomain%nn(), obj%femdomain%nd())
      array = abs(array)
      do i = 1, 3
         obj%maxV(i) = maxval([abs(obj%maxV(i)), maxval(array(:, i))])
      end do

      array = reshape(obj%A, obj%femdomain%nn(), obj%femdomain%nd())
      array = abs(array)
      do i = 1, 3
         obj%maxA(i) = maxval([abs(obj%maxA(i)), maxval(array(:, i))])
      end do

   end subroutine

   function getNewmarkBetaMatrixSeismicAnalysis(obj, M, C, K, beta, gamma, dt) result(ret)
      class(SeismicAnalysis_), intent(in) :: obj
      real(real64), intent(in) :: M(:, :), C(:, :), K(:, :), beta, gamma, dt
      real(real64), allocatable :: ret(:, :)
      integer(int32) :: n

      n = size(M, 1)
      ret = zeros(n, n)
      ret(:, :) = 1.0d0/(beta*dt*dt)*M(:, :) &
                  + gamma/(beta*dt)*C(:, :) &
                  + K(:, :)

   end function

   function getNewmarkBetaVectorSeismicAnalysis(obj, u_n, v_n, a_n, force, beta, gamma, M, C, dt) result(ret)
      class(SeismicAnalysis_), intent(in) :: obj
      real(real64), intent(in) :: u_n(:), v_n(:), a_n(:), force(:), beta, gamma, dt
      real(real64), intent(in) :: M(:, :), C(:, :)
      real(real64), allocatable :: ret(:)
      real(real64) :: a(8)
      integer(int32) :: n

      n = size(U_n, 1)
      ret = zeros(n)
      a(1) = gamma/(beta*dt)
      a(2) = -gamma/(beta*dt)
      a(3) = 1.0d0 - gamma/beta
      a(4) = dt*(1.0d0 - gamma/(2.0d0*beta))
      a(5) = 1.0d0/(beta*dt*dt)
      a(6) = -1.0d0/(beta*dt*dt)
      a(7) = -1.0d0/(beta*dt)
      a(8) = 1.0d0 - 1.0d0/(2.0d0*beta)
      ret(:) = force(:) &
               - a(6)*matmul(M, u_n) - a(7)*matmul(M, v_n) - a(8)*matmul(M, a_n) &
               - a(2)*matmul(C, u_n) - a(3)*matmul(C, v_n) - a(4)*matmul(C, a_n)
   end function
! ##########################################################################################
   function updateVelocityNewmarkBetaSeismicAnalysis(obj, u, u_n, v_n, a_n, gamma, beta, dt) result(ret)
      class(SeismicAnalysis_), intent(in) :: obj
      real(real64), intent(in) :: u(:), u_n(:), v_n(:), a_n(:), beta, gamma, dt
      real(real64), allocatable :: ret(:)
      integer(int32) :: n

      ret = zeros(size(u))
      ! usually, gamma=0.5, beta=0.25
      !ret(:) = gamma/(beta*dt)*u(:) &
      !    - gamma/(beta*dt)*u_n(:) &
      !    + (1.0d0 - gamma/beta )*v_n(:) &
      !    + dt*(1.0d0 - gamma/(2.0d0*beta) )*a_n(:)

      ret(:) = gamma/(beta*dt)*u(:) & ! modified
               - gamma/(beta*dt)*u_n(:) &  ! modified
               + (1.0d0 - gamma/beta)*v_n(:) &
               + dt*(1.0d0 - gamma/(2.0d0*beta))*a_n(:)
      ! confirmed 2021/10/13
   end function
! ##########################################################################################

! ##########################################################################################
   function updateAccelNewmarkBetaSeismicAnalysis(obj, u, u_n, v_n, a_n, gamma, beta, dt) result(ret)
      class(SeismicAnalysis_), intent(in) :: obj
      real(real64), intent(in) :: u(:), u_n(:), v_n(:), a_n(:), beta, gamma, dt
      real(real64), allocatable :: ret(:)
      integer(int32) :: n

      ret = zeros(size(u))

      !ret(:) = 1.0d0/(beta*dt*dt)*u(:) &
      !    - 1.0d0/(beta*dt*dt)*u_n(:) &
      !    - 1.0d0/(beta*dt)*v_n(:) &
      !    + (1.0d0 - 1.0d0/(2.0d0*beta) )*a_n(:)
      ! confirmed 2021/10/13
      ret(:) = 1.0d0/(beta*dt*dt)*u(:) &
               - 1.0d0/(beta*dt*dt)*u_n(:) &
               - 1.0d0/(beta*dt)*v_n(:) &
               + (1.0d0 - 1.0d0/(2.0d0*beta))*a_n(:)
   end function
! ##########################################################################################

   subroutine removeSeismicAnalysis(obj)
      class(SeismicAnalysis_), intent(inout) :: obj

      if (associated(obj%femdomain)) then
         nullify (obj%femdomain)
      end if

      if (allocated(obj%da)) then !(:) ! increment of accel.
         deallocate (obj%da)
      end if
      if (allocated(obj%a)) then !(:) ! accel.
         deallocate (obj%a)
      end if
      if (allocated(obj%a_ext)) then !(:) ! External accel.
         deallocate (obj%a_ext)
      end if
      if (allocated(obj%a_ext_n)) then !(:) ! External accel.
         deallocate (obj%a_ext_n)
      end if
      if (allocated(obj%v)) then !(:) ! velocity
         deallocate (obj%v)
      end if
      if (allocated(obj%u)) then !(:) ! disp.
         deallocate (obj%u)
      end if
      if (allocated(obj%du)) then !(:) ! increment of disp.
         deallocate (obj%du)
      end if
      if (allocated(obj%wave)) then !(:,:)
         deallocate (obj%wave)
      end if
      if (allocated(obj%dwave)) then !(:,:)
         deallocate (obj%dwave)
      end if

      if (allocated(obj%Density)) then !(:)
         deallocate (obj%Density)
      end if
      if (allocated(obj%YoungModulus)) then !(:)
         deallocate (obj%YoungModulus)
      end if
      if (allocated(obj%PoissonRatio)) then !(:)
         deallocate (obj%PoissonRatio)
      end if

      obj%MaxA(1:3) = 0.0d0
      obj%MaxV(1:3) = 0.0d0
      obj%MaxU(1:3) = 0.0d0

      if (allocated(obj%WaveNodeList)) then!(:)
         deallocate (obj%WaveNodeList)
      end if
      if (allocated(obj%FixNodeList_x)) then!(:)
         deallocate (obj%FixNodeList_x)
      end if
      if (allocated(obj%FixNodeList_y)) then!(:)
         deallocate (obj%FixNodeList_y)
      end if
      if (allocated(obj%FixNodeList_z)) then!(:)
         deallocate (obj%FixNodeList_z)
      end if

      if (allocated(obj%FixNodeList_Disp_x)) then !(:)
         deallocate (obj%FixNodeList_Disp_x)
      end if
      if (allocated(obj%FixNodeList_Disp_y)) then !(:)
         deallocate (obj%FixNodeList_Disp_y)
      end if
      if (allocated(obj%FixNodeList_Disp_z)) then !(:)
         deallocate (obj%FixNodeList_Disp_z)
      end if

      obj%wavedirection = "z"
      obj%wavetype = 0
      obj%dt = 1.0d0
      obj%error = dble(1.0e-14)
      obj%t = 0.0d0
      obj%step = 0
      obj%alpha = 0.52400d0
      obj%beta = 0.00129d0 ! Rayleigh dumping parameters, h=1%
      obj%Newmark_beta = 0.250d0 ! Nemark-beta method parameters
      obj%Newmark_gamma = 0.50d0 ! Nemark-beta method parameters
      obj%restart = .False.
      call obj%femsolver%remove()
      if (associated(obj%femdomain)) nullify (obj%femdomain)

   end subroutine

! #############################################################
   subroutine absorbingBoundarySeismicAnalysis(obj, x_min, x_max, y_min, y_max, z_min, z_max, &
                                               direction, dumping_ratio)
      class(SeismicAnalysis_), intent(inout) :: obj
      real(real64), optional, intent(in) :: x_min, x_max, y_min, y_max, z_min, z_max, &
                                            dumping_ratio
      character(1) :: direction
      integer(int32), allocatable :: selected_nodes(:)

      if (present(dumping_ratio)) then
         obj%boundary_dumping_ratio = dumping_ratio
      end if

      selected_nodes = obj%femdomain%select( &
                       x_min=x_min, &
                       x_max=x_max, &
                       y_min=y_min, &
                       y_max=y_max, &
                       z_min=z_min, &
                       z_max=z_max)

      if (direction == "x" .or. direction == "X") then
         obj%absorbingBoundary_x = obj%absorbingBoundary_x//selected_nodes
      elseif (direction == "y" .or. direction == "Y") then
         obj%absorbingBoundary_y = obj%absorbingBoundary_y//selected_nodes
      elseif (direction == "z" .or. direction == "Z") then
         obj%absorbingBoundary_z = obj%absorbingBoundary_z//selected_nodes
      else
         print *, "ERROR ::absorbingBoundarySeismicAnalysis >> invalid direction: ", direction
         print *, "direction = {x, X, y, Y, z, Z}"
      end if

   end subroutine
! #############################################################

! #############################################################
   pure function getAbsorbingBoundaryForceSeismicAnalysis(obj) result(force)
      class(SeismicAnalysis_), intent(in) :: obj
      real(real64), allocatable :: force(:)
      integer(int32) :: i, node_id

      if (allocated(obj%absorbingBoundary_xyz)) then
         ! for multi-domain
         force = zeros(size(obj%U))
         do i = 1, size(obj%absorbingBoundary_xyz)
            node_id = obj%absorbingBoundary_xyz(i)
            force(3*node_id - 2) = -obj%absorbingBoundary_elasticity*obj%U(3*node_id - 2) &
                                   - obj%absorbingBoundary_viscosity*obj%V(3*node_id - 2)
            force(3*node_id - 1) = -obj%absorbingBoundary_elasticity*obj%U(3*node_id - 1) &
                                   - obj%absorbingBoundary_viscosity*obj%V(3*node_id - 1)
            force(3*node_id - 0) = -obj%absorbingBoundary_elasticity*obj%U(3*node_id - 0) &
                                   - obj%absorbingBoundary_viscosity*obj%V(3*node_id - 0)
         end do
      else
         ! for single-domain
         force = zeros(obj%femdomain%nn()*obj%femdomain%nd())
         if (allocated(obj%absorbingBoundary_x)) then
            do i = 1, size(obj%absorbingBoundary_x)
               force((obj%absorbingBoundary_x(i) - 1)*obj%femdomain%nd() + 1) &
                  = -obj%boundary_dumping_ratio* &
                    obj%V((obj%absorbingBoundary_x(i) - 1)*obj%femdomain%nd() + 1)
            end do
         end if

         if (allocated(obj%absorbingBoundary_y)) then
            do i = 1, size(obj%absorbingBoundary_y)
               force((obj%absorbingBoundary_y(i) - 1)*obj%femdomain%nd() + 2) &
                  = -obj%boundary_dumping_ratio* &
                    obj%V((obj%absorbingBoundary_y(i) - 1)*obj%femdomain%nd() + 2)
            end do
         end if

         if (allocated(obj%absorbingBoundary_z)) then
            do i = 1, size(obj%absorbingBoundary_z)
               force((obj%absorbingBoundary_z(i) - 1)*obj%femdomain%nd() + 3) &
                  = -obj%boundary_dumping_ratio* &
                    obj%V((obj%absorbingBoundary_z(i) - 1)*obj%femdomain%nd() + 3)
            end do
         end if
      end if

   end function
! #############################################################

   subroutine modalAnalysisSeismicAnalysis_single_domain(this, femdomain, YoungModulus, PoissonRatio, Density, &
                                                         fix_node_list_x, fix_node_list_y, fix_node_list_z)
      class(SeismicAnalysis_), intent(inout) :: this
      type(FEMDomain_), intent(inout), target :: femdomain
      real(real64), intent(in) :: YoungModulus(:), PoissonRatio(:), Density(:)

      type(IO_) :: f

      integer(int32) :: i
      real(real64) :: Vs, t, dt, E_Al
      real(real64), allocatable :: Mode_U(:), mode_Ut(:), freq(:), eigen_value(:), eigen_vectors(:, :)
      integer(int32), allocatable :: node_list(:)
      integer(int32), optional, allocatable, intent(in) :: fix_node_list_x(:)
      integer(int32), optional, allocatable, intent(in) :: fix_node_list_y(:)
      integer(int32), optional, allocatable, intent(in) :: fix_node_list_z(:)

      this%YoungModulus = YoungModulus
      this%PoissonRatio = PoissonRatio
      this%Density = Density
      ! Modal analysis

      if (associated(this%femdomain)) then
         nullify (this%femdomain)
      end if
      this%femdomain => femdomain

      !read file
      call this%femsolver%init(NumDomain=1)
      call this%femsolver%setDomain(FEMDomain=femdomain, DomainID=1)
      call this%femsolver%setCRS(DOF=femdomain%nd())

      !$OMP parallel do
      do i = 1, femdomain%ne()
         call this%femsolver%setMatrix( &
            DomainID=1, &
            ElementID=i, &
            DOF=3, &
            Matrix=femdomain%MassMatrix(ElementID=i, Density=Density(i), DOF=3) &
            )
      end do
      !$OMP end parallel do

      call this%femsolver%keepThisMatrixAs("B")
      call this%femsolver%zeros()

      print *, "Save Stiffness Matrix"

      !$OMP parallel do
      do i = 1, femdomain%ne()
         call this%femsolver%setMatrix( &
            DomainID=1, &
            ElementID=i, &
            DOF=3, &
            Matrix=femdomain%StiffnessMatrix(ElementID=i, E=YoungModulus(i), v=PoissonRatio(i)) &
            )
      end do
      !$OMP end parallel do

      call this%femsolver%keepThisMatrixAs("A")

      ! Eigen value problem solver by scipy

!    print *, "solver%eig"
      if (present(fix_node_list_x)) then
         if (allocated(fix_node_list_x)) then
            node_list = fix_node_list_x
            node_list = (node_list(:) - 1)*3 + 1
            call this%femsolver%fix_eig(IDs=node_list)
         end if
      end if

      if (present(fix_node_list_y)) then
         if (allocated(fix_node_list_y)) then
            node_list = fix_node_list_y
            node_list = (node_list(:) - 1)*3 + 2
            call this%femsolver%fix_eig(IDs=node_list)
         end if
      end if

      if (present(fix_node_list_z)) then
         if (allocated(fix_node_list_z)) then
            node_list = fix_node_list_z
            node_list = (node_list(:) - 1)*3 + 3
            call this%femsolver%fix_eig(IDs=node_list)
         end if
      end if

      call this%femsolver%eig(eigen_value=eigen_value, eigen_vectors=eigen_vectors)

      ! read results
      freq = sqrt(abs(eigen_value))/2.0d0/3.141590d0
      !$OMP parallel do
      do i = 1, size(freq)
         if (freq(i) == 0.0d0) cycle
         eigen_vectors(:, i) = eigen_vectors(:, i)/norm(eigen_vectors(:, i))
      end do
      !$OMP end parallel do

      this%frequency = freq
      this%ModeVectors = eigen_vectors

!    ! 20 modes
!    do i_i=1,20
!        mode_U = zeros(size(eigen_vectors,1))
!        mode_U = eigen_vectors(:,i_i)
!        dt = 1.0d0/freq(i_i)/100.0d0
!        do j_j=1,100
!            t = dt * dble(j_j-1)
!            mode_Ut = mode_U*cos( 2.0d0*3.140d0*freq(i_i)*t )
!
!            domains(1)%mesh%nodcoord = domains(1)%mesh%nodcoord &
!            +0.00010d0*reshape(mode_Ut,domains(1)%nn(),3 )
!
!            call domains(1)%vtk("Mode_Fortran_"+str(i_i)+"_t_"+str(j_j))
!            domains(1)%mesh%nodcoord = domains(1)%mesh%nodcoord &
!            -0.00010d0*reshape(mode_Ut,domains(1)%nn(),3 )
!        enddo
!    enddo

   end subroutine

   subroutine modalAnalysisSeismicAnalysis_multi_domain(this, femdomains, connectivity, &
                                                        YoungModulus, PoissonRatio, Density, &
                                                        fix_node_list_x, fix_node_list_y, fix_node_list_z, &
                                                        overset_algorithm, penalty)
      class(SeismicAnalysis_), intent(inout) :: this

      type(FEMDomain_), intent(inout) :: femdomains(:)
      integer(int32), intent(in) :: connectivity(:, :), overset_algorithm
      real(real64), intent(in) :: YoungModulus(:), PoissonRatio(:), Density(:)
      real(real64), optional, intent(in) ::penalty

      type(IO_) :: f

      integer(int32) :: i, DomainID
      real(real64) :: Vs, t, dt, E_Al
      real(real64), allocatable :: Mode_U(:), mode_Ut(:), freq(:), eigen_value(:), eigen_vectors(:, :)
      integer(int32), allocatable :: node_list(:)
      integer(int32), optional, intent(in) :: fix_node_list_x(:)
      integer(int32), optional, intent(in) :: fix_node_list_y(:)
      integer(int32), optional, intent(in) :: fix_node_list_z(:)

      integer(int32), allocatable :: DomainIDs(:)
      !read file

      DomainIDs = arange(start_val=1, stop_val=size(femdomains), step=1, dtype=int32)

      ! overset
      do i = 1, size(connectivity, 1)
         call femdomains(connectivity(i, 1))%overset( &
            FEMDomain=femdomains(connectivity(i, 2)), &
            DomainID=connectivity(i, 2), &
            MyDomainID=connectivity(i, 1), &
            algorithm=overset_algorithm)
      end do

      call this%femsolver%init(NumDomain=size(femdomains))
      call this%femsolver%setDomain(FEMDomains=femdomains, DomainIDs=DomainIDs)
      call this%femsolver%setCRS(DOF=femdomains(1)%nd())

      if (allocated(this%NodeID_range)) deallocate (this%NodeID_range)
      allocate (this%NodeID_range(0:size(femdomains), 1:2))
      this%NodeID_range(0, 1) = 0
      this%NodeID_range(0, 2) = 0
      this%NodeID_range(1, 1) = 1
      this%NodeID_range(1, 2) = femdomains(1)%nn()
      do domainID = 2, size(femdomains)
         this%NodeID_range(domainID, 1) = this%NodeID_range(DomainID - 1, 2) + 1
         this%NodeID_range(domainID, 2) = this%NodeID_range(DomainID - 1, 2) &
                                          + femdomains(DomainID)%nn()
      end do

      if (allocated(this%ElementID_range)) deallocate (this%ElementID_range)
      allocate (this%ElementID_range(0:size(femdomains), 1:2))
      this%ElementID_range(0, 1) = 0
      this%ElementID_range(0, 2) = 0
      this%ElementID_range(1, 1) = 1
      this%ElementID_range(1, 2) = femdomains(1)%ne()
      do domainID = 2, size(femdomains)
         this%ElementID_range(domainID, 1) = this%ElementID_range(DomainID - 1, 2) + 1
         this%ElementID_range(domainID, 2) = this%ElementID_range(DomainID - 1, 2) &
                                             + femdomains(DomainID)%ne()
      end do

      do domainID = 1, size(femdomains)
         do i = 1, femdomains(domainID)%ne()
            call this%femsolver%setMatrix( &
               DomainID=DomainID, &
               ElementID=i, &
               DOF=3, &
               Matrix=femdomains(domainID)%MassMatrix( &
               ElementID=i, &
               Density=Density(this%ElementID_range(DomainID - 1, 2) + i), DOF=3) &
               )
         end do
      end do

      call this%femsolver%keepThisMatrixAs("B")
      call this%femsolver%zeros()

      print *, "Save Stiffness Matrix"
      do domainID = 1, size(femdomains)
         do i = 1, femdomains(domainID)%ne()
            call this%femsolver%setMatrix( &
               DomainID=DomainID, &
               ElementID=i, &
               DOF=3, &
               Matrix=femdomains(domainID)%StiffnessMatrix( &
               ElementID=i, &
               E=YoungModulus(this%ElementID_range(DomainID - 1, 2) + i), &
               v=PoissonRatio(this%ElementID_range(DomainID - 1, 2) + i)) &
               )
         end do
      end do
      call this%femsolver%setEbOM(penalty=input(default=10000000.0d0, option=penalty), DOF=3)

      call this%femsolver%keepThisMatrixAs("A")

      ! Eigen value problem solver by scipy

!    print *, "solver%eig"
      if (present(fix_node_list_x)) then
         node_list = fix_node_list_x
         node_list = (node_list(:) - 1)*3 + 1
         call this%femsolver%fix_eig(IDs=node_list)
      end if

      if (present(fix_node_list_y)) then
         node_list = fix_node_list_y
         node_list = (node_list(:) - 1)*3 + 2
         call this%femsolver%fix_eig(IDs=node_list)
      end if

      if (present(fix_node_list_z)) then
         node_list = fix_node_list_z
         node_list = (node_list(:) - 1)*3 + 3
         call this%femsolver%fix_eig(IDs=node_list)
      end if

      call this%femsolver%eig(eigen_value=eigen_value, eigen_vectors=eigen_vectors)

      ! read results
      freq = sqrt(abs(eigen_value))/2.0d0/3.141590d0

      this%frequency = freq
      this%ModeVectors = eigen_vectors

!    ! 20 modes
!    do i_i=1,20
!        mode_U = zeros(size(eigen_vectors,1))
!        mode_U = eigen_vectors(:,i_i)
!        dt = 1.0d0/freq(i_i)/100.0d0
!        do j_j=1,100
!            t = dt * dble(j_j-1)
!            mode_Ut = mode_U*cos( 2.0d0*3.140d0*freq(i_i)*t )
!
!            domains(1)%mesh%nodcoord = domains(1)%mesh%nodcoord &
!            +0.00010d0*reshape(mode_Ut,domains(1)%nn(),3 )
!
!            call domains(1)%vtk("Mode_Fortran_"+str(i_i)+"_t_"+str(j_j))
!            domains(1)%mesh%nodcoord = domains(1)%mesh%nodcoord &
!            -0.00010d0*reshape(mode_Ut,domains(1)%nn(),3 )
!        enddo
!    enddo

   end subroutine

   subroutine vtkSeismicAnalysis(this, name, num_mode, amp, scalar_field, with_stress)
      class(SeismicAnalysis_), intent(in) :: this
      character(*), intent(in) :: name
      integer(int32), intent(in) :: num_mode
      real(real64), intent(in) :: amp
      real(real64), optional, intent(in) :: scalar_field(:)
      integer(int32), optional, intent(in) :: with_stress(1:2) ! i and j for sigma_{i,j}
      integer(int32) :: i, j, n
      real(real64), allocatable :: Mode_U(:), mode_Ut(:), freq(:)
      real(real64) :: t, dt
      ! 20 modes
      t = 0.0d0
      dt = 0.0d0

      do i = 1, num_mode
         mode_U = zeros(size(this%modevectors, 1))
         mode_U = this%modevectors(:, i)
         dt = 1.0d0/this%frequency(i)/100.0d0
         do j = 1, 100
            t = dt*dble(j - 1)
            mode_Ut = mode_U*cos(2.0d0*3.140d0*this%frequency(i)*t)

            this%femdomain%mesh%nodcoord = this%femdomain%mesh%nodcoord &
                                           + amp*reshape(mode_Ut, this%femdomain%nn(), 3)
            if (present(scalar_field)) then
               call this%femdomain%vtk(name + str(i) + "_t_"+str(j), scalar=scalar_field)
            elseif (present(with_stress)) then
               call this%femdomain%vtk(name + str(i) + "_t_"+str(j), &
                                       scalar=this%femdomain%getElementCauchyStress( &
                                       displacement=mode_Ut, &
                                       E=this%YoungModulus, &
                                       v=this%PoissonRatio, &
                                       i=with_stress(1), j=with_stress(2)))
            else
               call this%femdomain%vtk(name + str(i) + "_t_"+str(j))
            end if

            this%femdomain%mesh%nodcoord = this%femdomain%mesh%nodcoord &
                                           - amp*reshape(mode_Ut, this%femdomain%nn(), 3)
         end do
      end do

   end subroutine
! ######################################################
   function velocitySeismicAnalysis(this, domainID, Direction) result(v)
      class(SeismicAnalysis_), intent(in) :: this
      integer(int32), intent(in) :: DomainID, Direction
      real(real64), allocatable :: v(:), v_xyz(:, :)
      integer(int32) :: from, to, i
      integer(int32) :: DOF = 3

      from = 1
      do i = 1, domainID - 1
         from = from + this%femsolver%Num_nodes_in_Domains(i)*DOF
      end do
      to = from + this%femsolver%Num_nodes_in_Domains(DomainID)*DOF - 1
      v_xyz = reshape(this%v(from:to), this%femsolver%Num_nodes_in_Domains(i), DOF)
      v = v_xyz(:, direction)

   end function
! ######################################################

   subroutine setSolverParametersSeismicAnalysis(this, error, debug)
      class(SeismicAnalysis_), intent(inout) :: this
      real(real64), optional, intent(in) :: error
      logical, optional, intent(in) :: debug

      if (present(error)) then
         this%modal%solver%er0 = error
         this%femsolver%er0 = error
         this%modal%solver%relative_er = error
         this%femsolver%relative_er = error
         this%error = error
      else
         this%modal%solver%er0 = this%error
         this%femsolver%er0 = this%error
         this%modal%solver%relative_er = this%error
         this%femsolver%relative_er = this%error
      end if

      if (present(debug)) then
         this%modal%solver%debug = debug
         this%femsolver%debug = debug
         this%debug = debug
      else
         this%modal%solver%debug = this%debug
         this%femsolver%debug = this%debug
      end if

   end subroutine

   function getModeVectorSeismicAnalysis(this, domainID, ModeID) result(mode_vector)
      class(SeismicAnalysis_), intent(in) :: this
      integer(int32), intent(in) :: domainID, ModeID
      real(real64), allocatable :: mode_vector(:)
      integer(int32) :: i, n, nn

      if (.not. allocated(this%ModeVectors)) then
         print *, "ERROR >> getModeVectorSeismicAnalysis >> Please call %modalAnalysis"
         return
      end if

      if (allocated(this%NodeID_range)) then
         n = this%NodeID_range(domainID - 1, 2)
         nn = this%NodeID_range(domainID, 2)! - this%NodeID_range(domainID-1,2)
         mode_vector = this%ModeVectors(3*n + 1:nn*3, ModeID)
      else
         mode_vector = this%ModeVectors(:, ModeID)
      end if

   end function

   subroutine exportModeShapeSeismicAnalysis(this, domainID, femdomain, name, MAX_MODE_NUM, amp)
      class(SeismicAnalysis_), intent(in) :: this
      integer(int32), intent(in) :: domainID
      type(FEMDomain_), intent(inout) :: femdomain
      character(*), intent(in) :: name
      integer(int32), optional, intent(in) :: MAX_MODE_NUM
      real(real64), optional, intent(in) :: amp

      real(real64), allocatable :: mode_vector(:), disp(:)
      integer(int32) :: i, n, nn
      integer(int32) :: MAX_MODE_ID = 10
      real(real64) :: amp_val

      amp_val = input(default=1.0d0, option=amp)
      MAX_MODE_ID = minval([size(this%ModeVectors, 2), MAX_MODE_ID])
      do i = 1, MAX_MODE_ID
         disp = this%getModeVector(domainID=domainID, ModeID=i)
         call femdomain%deform(disp=disp*amp_val)
         call femdomain%vtk(name + "_mode_"+zfill(i, 4) + ".vtk")
         call femdomain%deform(disp=-disp*amp_val)
      end do

   end subroutine

end module SeismicAnalysisClass
