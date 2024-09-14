module SeepageFlowClass
   use FEMDomainClass
   use FEMSolverClass
   implicit none

   type :: SeepageFlow_
      type(FEMDomain_), pointer :: FEMDomain => null()
      type(FEMSolver_) :: solver
      character(:), allocatable :: model
      real(real64) :: rho = 1.0d0 ! 1t/m^3
      real(real64) :: g = 9.80d0! m/s/s

      real(real64), allocatable :: Permiability(:)
      real(real64), allocatable :: WaterHead(:)
      real(real64), allocatable :: Velocity(:, :)

   contains
      procedure, public :: init => initSeepageFlow
      procedure, public :: fixPressureBoundary => fixPressureBoundarySeepageFlow
      procedure, public :: getPressure => getPressureSeepageFlow
      procedure, public :: getVelocity => getVelocitySeepageFlow
      !procedure,public :: SeepageFlowAnalysis => SeepageFlowAnalysis_single_domain_SeepageFlow
   end type

   interface SWCC
      module procedure SWCC_scalar, SWCC_vector
   end interface

contains

   subroutine initSeepageFlow(this, femdomain, model, Permiability)
      class(SeepageFlow_), intent(inout) :: this
      type(FEMDomain_), target, intent(in) :: femdomain
      real(real64), intent(in) :: Permiability(:)
      character(*), intent(in) :: model
      real(real64) :: rho, gravity
      real(real64), allocatable :: hvec(:)
      integer(int32) :: ElementID

      if (associated(this%femdomain)) then
         this%femdomain => null()
      end if

      this%femdomain => femdomain
      this%model = model

      this%Permiability = Permiability

      this%WaterHead = zeros(femdomain%nn())
      this%Velocity = zeros(femdomain%nn(), femdomain%nd())
      select case (this%model)
      case ("Darcy", "darcy")
         ! activate following element-wise values
         call this%solver%init(NumDomain=1)
         call this%solver%setDomain(FEMDomain=femdomain, domainID=1)
         call this%solver%setCRS(DOF=1)

         rho = this%rho ! 1.0d0  1t/m^3
         gravity = this%g   ! 9.80d0 m/s/s
         !$OMP parallel default(shared) private(hvec)
         !$OMP do
         do ElementID = 1, this%femdomain%ne()

            hvec = -this%femdomain%ElementVector( &
                   ElementID=ElementID, &
                   GlobalVector=this%femdomain%z(), &
                   DOF=1)
            call this%solver%setMatrix(DomainID=1, ElementID=ElementID, DOF=1, &
                                       Matrix=this%femdomain%DiffusionMatrix(ElementID=ElementID, &
                                                                             D=Permiability(ElementID)))

            call this%solver%setVector(DomainID=1, ElementID=ElementID, DOF=1, &
                                       Vector=matmul(this%femdomain%DiffusionMatrix(ElementID=ElementID, &
                                                                                    D=rho*gravity), &
                                                     hvec))

         end do
         !$OMP end do
         !$OMP end parallel
         call this%solver%keepThisMatrixAs("A")

      case default
         print *, "initSeepageFlow >> unknown model name ", model
      end select

   end subroutine

! ##########################################################################
   subroutine fixPressureBoundarySeepageFlow(this, NodeList, Pressure, Pressures, DomainID)
      class(SeepageFlow_), intent(inout) :: This
      integeR(int32), intent(in) :: NodeList(:)
      integeR(int32), optional, intent(in) :: DomainID
      real(real64), optional, intent(in) :: Pressure
      real(real64), optional, intent(in) :: Pressures(:)
      integeR(int32) :: DomainID_

      DomainID_ = input(default=1, option=DomainID)

      if (present(Pressure)) then
         call this%solver%fix(DomainID=DomainID_, IDs=NodeList, FixValue=Pressure)
         return
      end if

      if (present(Pressure)) then
         call this%solver%fix(DomainID=DomainID_, IDs=NodeList, FixValues=Pressures)
         return
      end if
   end subroutine
! ##########################################################################

   function getPressureSeepageFlow(this, debug, dt, timeIntegral) result(WaterHead)
      class(SeepageFlow_), intent(inout) :: This
      logical, optional, intent(in) :: debug
      real(real64), allocatable :: WaterHead(:)
      real(real64), optional, intent(in) :: dt
      character(*), optional, intent(in) :: timeIntegral
      type(CRS_) :: Dmat, Mmat
      real(real64) :: rho, gravity
      real(real64), allocatable :: hvec(:)
      integer(int32) :: ElementID

      if (present(debug)) then
         this%solver%debug = debug
      end if

      if (present(dt)) then
         ! unsteady state
         select case (timeIntegral)
         case default
            print *, "getPressureSeepageFlow >>  ", timeIntegral, " is &
&                 not implemented for time-integral"
         case ("BackwardEuler", "Backward Euler", "BE")
            ![M]{dh/dt} = -[D]{h}
            !{h}_{n+1} = {dh/dt}_{n+1} * dt + {h}_n
            !({h}_{n+1}-{h}_n)/dt = {dh/dt}_{n+1}
            ![M]({h}_{n+1}-{h}_n)/dt = -[D]{h}_{n+1}
            !1/dt[M]{h}_{n+1}  + [D]{h}_{n+1}  = 1/dt[M]{h}_n

            rho = this%rho ! 1.0d0  1t/m^3
            gravity = this%g   ! 9.80d0 m/s/s

            call this%solver%zeros() ! 一旦クリア

            ! Create M-matrix
            !$OMP parallel default(shared) private(hvec)
            !$OMP do
            do ElementID = 1, this%femdomain%ne()
               call this%solver%setMatrix(DomainID=1, ElementID=ElementID, DOF=1, &
                                          Matrix=this%femdomain%MassMatrix(ElementID=ElementID, &
                                                                           Density=1.0d0/dt, DOF=1))

               hvec = this%femdomain%ElementVector( &
                      ElementID=ElementID, &
                      GlobalVector=this%femdomain%z(), &
                      DOF=1)
               call this%solver%setVector(DomainID=1, ElementID=ElementID, DOF=1, &
                                          Vector=matmul(this%femdomain%DiffusionMatrix(ElementID=ElementID, &
                                                                                       D=this%Permiability(ElementID)), &
                                                        hvec))
            end do
            !$OMP end do
            !$OMP end parallel

            call this%solver%keepThisMatrixAs("A")! [M] matrix
            ! add D-matrix
            !$OMP parallel default(shared) private(hvec)
            !$OMP do
            do ElementID = 1, this%femdomain%ne()

               call this%solver%setMatrix(DomainID=1, ElementID=ElementID, DOF=1, &
                                          Matrix=this%femdomain%DiffusionMatrix(ElementID=ElementID, &
                                                                                D=this%Permiability(ElementID)))

            end do
            !$OMP end do
            !$OMP end parallel

            Mmat = this%solver%getCRS("A")

            !1/dt[M]{h}_{n+1}  + [D]{h}_{n+1}  = 1/dt[M]{h}_n

            this%solver%CRS_RHS = Mmat%matmul(this%WaterHead)
            WaterHead = this%solver%solve()

         end select

      else
         ! steady state
         WaterHead = this%solver%solve()
      end if

   end function

! ##########################################################################
   function getVelocitySeepageFlow(this, Pressure, Permiability, Permiability_field) result(Velocity)
      class(SeepageFlow_), intent(inout) :: This
      real(real64), intent(in) :: Pressure(:)
      real(real64), optional, intent(in) :: Permiability, Permiability_field(:)
      real(real64), allocatable :: Velocity(:, :)
      integer(int32) :: ElementID

      Velocity = zeros(this%femdomain%ne(), this%femdomain%nd())
      do ElementID = 1, this%femdomain%ne()
         if (present(Permiability)) then
            Velocity(ElementID, :) = this%femdomain%FlowVector( &
                                     ElementID=ElementID, Pressure=Pressure, Permiability=Permiability)
         elseif (present(Permiability_field)) then
            Velocity(ElementID, :) = this%femdomain%FlowVector( &
                                     ElementID=ElementID, Pressure=Pressure, Permiability=Permiability_field(ElementID))
         end if
      end do

   end function

! ##########################################################################

   function SWCC_scalar(model, params, theta, psi) result(ret)
      character(*), intent(in) :: model
      real(real64), intent(in) :: params(:)
      real(real64) :: theta_res, theta_sat, alpha, lambda, mu
      real(real64), optional, intent(in) :: theta, psi
      real(real64) :: ret ! psi or theta

      ret = 0.0d0
      select case (model)
      case ("van Genuchten", "vanGenuchten", "Van Genuchten", "VanGenuchten", "VG")
         theta_res = params(1)
         theta_sat = params(2)
         alpha = params(3)
         lambda = params(4)
         mu = 1.0d0 - 1.0d0/lambda
         if (present(theta)) then
            ret = (((theta_sat - theta_res)/(theta - theta_res)**(1.0d0/mu) - 1.0d0)**(1.0d0/lambda))/alpha

            return
         end if
         if (present(psi)) then
            if (psi > 0.0d0) then
               ret = theta_res + (theta_sat - theta_res)/(1.0d0 + (alpha*psi)**lambda)**mu
            else
               ret = theta_sat
            end if
            return
         end if

      end select

   end function

! functions
   function SWCC_vector(model, params, theta, psi, n) result(ret)
      character(*), intent(in) :: model
      real(real64), intent(in) :: params(:)
      real(real64) :: theta_res, theta_sat, alpha, lambda, mu
      real(real64), optional, intent(in) :: theta(:), psi(:)
      real(real64), allocatable :: ret(:) ! psi or theta
      integer(int32), intent(in) :: n
      integer(int32) :: i

      ret = zeros(n)
      if (present(theta)) then
         do i = 1, n
            ret(i) = SWCC_scalar(model=model, params=params, theta=theta(i))
         end do
         return
      end if

      if (present(psi)) then
         do i = 1, n
            ret(i) = SWCC_scalar(model=model, params=params, psi=psi(i))
         end do
         return
      end if
   end function
! ####################################################

!subroutine SeepageFlowAnalysis_single_domain_SeepageFlow(this,femdomain,WaterHead &
!    ,Permiability,fix_node_list,fix_node_value, debug&
!    )
!    class(SeepageFlow_),intent(inout) :: this
!    type(FEMDomain_),intent(in) :: femdomain
!    real(real64),intent(in) :: WaterHead(:),Permiability(:),fix_node_value(:) ! mH2O, m/day, mH2O
!    integer(int32),intent(in) :: fix_node_list(:)
!    logical,optional,intent(in) :: debug
!    integer(int32) :: i
!
!    ! Darcy flow
!    call this%init(femdomain,model="Darcy",Permiability=Permiability)
!
!    ! Boundary condition
!    call this%fixPressureBoundary(&
!            NodeList = fix_node_list , &
!            pressures = fix_node_value &
!        )
!
!    this%WaterHead = this%getPressure(debug=debug)
!    this%Velocity     = this%getVelocity(Pressure=WaterHead,Permiability_field=Permiability)
!
!end subroutine
! ####################################################

end module

