module SeismicAnalysisClass
    use fem
    implicit none

    type::SeismicAnalysis_
        type(FEMDomain_),pointer :: femdomain
        real(real64),allocatable :: a(:) ! accel.
        real(real64),allocatable :: v(:) ! velocity
        real(real64),allocatable :: u(:) ! disp.
        real(real64),allocatable :: du(:) ! increment of disp.
        real(real64),allocatable :: wave(:,:)
        real(real64) :: dt=1.0d0
        real(real64) :: t=0.0d0
        real(real64) :: alpha = 0.50d0
        real(real64) :: beta  = 0.50d0 ! Rayleigh damping parameters
        real(real64) :: Newmark_beta  = 0.250d0 ! Nemark-beta method parameters
        real(real64) :: Newmark_delta  = 0.50d0 ! Nemark-beta method parameters
        integer(int32) :: timestep=1
    contains
        procedure, public :: run => runSeismicAnalysis
        procedure, public :: CreateMatrixAndVector => CreateMatrixAndVectorSeismicAnalysis
    end type

contains

! ##############################################
subroutine runSeismicAnalysis(obj,dt,timestep,wave)
    class(SeismicAnalysis_),intent(inout) :: obj
    type(LinearSolver_) :: solver
    real(real64),optional,intent(in) :: dt
    integer(int32),optional,intent(in) :: timestep
    real(real64),optional,intent(in) :: wave(:,:)
    integer(int32) :: i

    obj%dt = input(default=0.0d0,option=dt)
    obj%timestep = input(default=1,option=timestep)
    if(present(wave) )then
        obj%wave = wave
    endif
    do i=1, obj%timestep
        obj%t = obj%t + dt
        call print("SeismicAnalysis >> "//str(obj%t-obj%dt)//"< t <"//str(obj%t)//" sec.")
        call obj%CreateMatrixAndVector(solver)
        call solver%solve(Solver="BiCGSTAB")
    enddo 
end subroutine
! ##############################################


! ##############################################
subroutine CreateMatrixAndVectorSeismicAnalysis(obj, solver)
    class(SeismicAnalysis_),intent(inout) :: obj
    type(LinearSolver_),intent(inout) :: solver
    real(real64),allocatable :: M_ij(:,:)
    real(real64),allocatable :: C_ij(:,:)
    real(real64),allocatable :: K_ij(:,:)
    real(real64),allocatable :: dF_i(:)
    real(real64),allocatable :: R_i(:)
    real(real64),allocatable ::dU_i(:)
    real(real64),allocatable :: V_i(:)
    real(real64),allocatable ::dV_i(:)
    real(real64),allocatable :: A_i(:)
    real(real64),allocatable ::dA_i(:)
    real(real64),allocatable :: A_ij(:,:)
    integer(int32) :: i,dim_num,n


    dim_num = size(obj%femdomain%mesh%nodcoord,2)
    n = dim_num * size(obj%femdomain%mesh%nodcoord,1)
    if(.not. allocated(obj%a) )then
        allocate(obj%a(n) )
    endif
    if(.not. allocated(obj%v) )then
        allocate(obj%v(n) )
    endif
    if(.not. allocated(obj%u) )then
        allocate(obj%u(n) )
    endif

!    do  ! Newton's Loop
!        ! Element matrix
!        do i=1,obj%femdomain%ne()
!            ! For each element
!            ! Ax=b will be installed into solver
!            M_ij = obj%femdomain%MassMatrix(ElementID=i)
!            K_ij = obj%femdomain%StiffnessMatrix(ElementID=i,E=,v=)
!            C_ij = obj%alpha * M_ij + obj%beta * K_ij
!            U_i  = obj%femdomain%ElementVector(ElementID=i, GlobalVector=obj%U, DOF=obj%femdomain%nd() )
!            V_i  = obj%femdomain%ElementVector(ElementID=i, GlobalVector=obj%V, DOF=obj%femdomain%nd() )
!            A_i  = obj%femdomain%ElementVector(ElementID=i, GlobalVector=obj%A, DOF=obj%femdomain%nd() )
!            
!            dF_i = obj%femdomain%MassVector(ElementID=i,Density=)
!
!            ! A_ij dU_j = R_i 
!            A_ij = K_ij &
!                + 1.0d0/(obj%Newmark_beta * obj%dt * obj%dt)*M_ij &
!                + obj%Newmark_delta/(obj%Newmark_beta*obj%dt)*C_ij
!            R_i  = dF_i &
!                + 1.0d0/(obj%Newmark_beta*obj%dt)*matmul(M_ij, V_i)&
!                + 1.0d0/(obj%Newmark_beta*2.0d0)*matmul(M_ij, A_i)&
!                + obj%Newmark_delta/obj%Newmark_beta*matmul(C_ij, V_i)&
!                + (obj%Newmark_delta/2.0d0*obj%Newmark_beta-1.0d0)*obj%dt*matmul(C_ij, A_i)&
!
!            ! import linear equations in the solver
!            
!            ! Stiffness matrix
!            call solver%set(node_id1=, node_id2=, val=)
!            call solver%set(node_id1=, node_id2=, val=)
!            call solver%set(node_id1=, node_id2=, val=)
!            call solver%set(node_id1=, node_id2=, val=)
!
!            ! Residual vector
!            call solver%set(node_id1=, val=)
!            call solver%set(node_id1=, val=)
!            call solver%set(node_id1=, val=)
!            call solver%set(node_id1=, val=)
!        enddo
!
!        ! Introduce Boundary Condition
!        call solver%fix(node_id1=, val=)
!
!        ! Now [A] {du} = {R} is ready
!        ! Solve
!        call solver%solve()
!
!        do i=1,obj%femdomain%ne()
!            ! For each element
!            U_i  = obj%femdomain%DisplacementVector(ElementID=i)
!            dU_i = obj%femdomain%ElementVector(ElementID=i,GlobalVector=obj%dU, DOF=obj%femdomain%nd())
!            V_i  = obj%femdomain%ElementVector(ElementID=i,GlobalVector=obj% V, DOF=obj%femdomain%nd())
!            A_i  = obj%femdomain%ElementVector(ElementID=i,GlobalVector=obj% A, DOF=obj%femdomain%nd())
!            
!            dA_i = 1.0d0/(obj%Newmark_beta*obj%dt*obj%dt)*(&
!                dU_i - obj%dt*V_i - obj%dt*obj%dt/2.0d0*dA_i &
!                )
!            
!            dV_i = obj%dt * (A_i + obj%Newmark*dA_i )
!
!            U_i = U_i + dU_i
!            V_i = V_i + dV_i
!            A_i = A_i + dA_i
!
!            ! one for all
!
!        enddo
!
!        ! obj%error == 0 only if nonlinear elasticity.
!        ! if Error <= TOL
!        if(obj%error <= TOL)then
!            ! Converged.
!            exit
!        endif
!
!    enddo

end subroutine
! ##############################################



end module SeismicAnalysisClass