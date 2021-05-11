module SeismicAnalysisClass
    use fem
    implicit none

    type::SeismicAnalysis_
        type(FEMDomain_),pointer :: femdomain
        real(real64),allocatable :: da(:) ! increment of accel.
        real(real64),allocatable :: a(:) ! accel.
        real(real64),allocatable :: v(:) ! velocity
        real(real64),allocatable :: u(:) ! disp.
        real(real64),allocatable :: du(:) ! increment of disp.
        real(real64),allocatable :: wave(:,:)
        real(real64),allocatable :: dwave(:,:)

        integer(int32),allocatable :: WaveNodeList(:)

        real(real64) :: dt=1.0d0
        real(real64) :: error=0.0d0
        real(real64) :: t=0.0d0
        real(real64) :: alpha = 0.50d0
        real(real64) :: beta  = 0.50d0 ! Rayleigh damping parameters
        real(real64) :: Newmark_beta  = 0.250d0 ! Nemark-beta method parameters
        real(real64) :: Newmark_delta  = 0.50d0 ! Nemark-beta method parameters
    contains
        procedure, public :: init => initSeismicAnalysis
        procedure, public :: loadWave => loadWaveSeismicAnalysis
        procedure, public :: run => runSeismicAnalysis
        procedure, public :: LinearReyleighNewmark => LinearReyleighNewmarkSeismicAnalysis
        procedure, public :: save => saveSeismicAnalysis
    end type

contains

! ##############################################
subroutine initSeismicAnalysis(obj)
    class(SeismicAnalysis_),intent(inout) :: obj

    obj%U = zeros(obj%femdomain%nn()*obj%femdomain%nd() )
    obj%V = zeros(obj%femdomain%nn()*obj%femdomain%nd() )
    obj%A = zeros(obj%femdomain%nn()*obj%femdomain%nd() )
    obj%dA = zeros(obj%femdomain%nn()*obj%femdomain%nd() )

end subroutine
! ##############################################


! ##############################################
subroutine saveSeismicAnalysis(obj,name,ratio)
    class(SeismicAnalysis_),intent(inout) :: obj
    character(*),intent(in) :: name
    real(real64),optional,intent(in) :: ratio
    real(real64) :: rat
    
    rat = input(default=1.0d0,option=ratio)
    obj%femdomain%mesh%nodcoord(:,:) = obj%femdomain%mesh%nodcoord(:,:) &
        + rat*reshape(obj%U,obj%femdomain%nn(),obj%femdomain%nd() )
    call obj%femdomain%msh(name)
    obj%femdomain%mesh%nodcoord(:,:) = obj%femdomain%mesh%nodcoord(:,:) &
        - rat*reshape(obj%U,obj%femdomain%nn(),obj%femdomain%nd() )
    
end subroutine
! ##############################################


! ##############################################
subroutine loadWaveSeismicAnalysis(obj,x_min,x_max,y_min,y_max,z_min,z_max)
    class(SeismicAnalysis_),intent(inout) :: obj
    real(real64),optional,intent(in) :: x_min,x_max,y_min,y_max,z_min,z_max

    obj%WaveNodeList = obj%femdomain%select(x_min=x_min,x_max=x_max,y_min=y_min,y_max=y_max,z_min=z_min,z_max=z_max)
end subroutine
! ##############################################

subroutine updateWaveSeismicAnalysis(obj)
    class(SeismicAnalysis_),intent(inout) :: obj
    integer(int32) :: node_id,i

    if(.not. allocated(obj%WaveNodeList) )then
        print *, "Caution >> updateWaveSeismicAnalysis >> no wave"
    endif

    do i=1,size(obj%WaveNodeList)
        node_id = obj%WaveNodeList(i)
        obj%dA(node_id) = obj%dwave(node_id,2)
    enddo

end subroutine

! ##############################################
subroutine runSeismicAnalysis(obj,t0,timestep,wave)
    class(SeismicAnalysis_),intent(inout) :: obj
    type(LinearSolver_) :: solver
    real(real64),optional,intent(in) :: t0
    integer(int32),optional,intent(in) :: timestep
    real(real64),optional,intent(in) :: wave(:,:)
    integer(int32) :: i,step

    if(present(wave) )then
        obj%wave = wave
    endif

    ! set wave
    ! wave = a(t)
    ! dwave = da(t)
    obj%dwave = increment(obj%wave,2 )
    step = input(default=size(obj%wave,1)-1,option=timestep )
    obj%t = input(default=0.0d0,option=t0 )

    do i=1, step
        ! update dt
        obj%dt = obj%wave(i+1,1) - obj%wave(i,1)
        
        ! update time
        obj%t = obj%t + obj%dt

        ! show info.
        call print("SeismicAnalysis >> "//str(obj%t-obj%dt)//"< t <"//str(obj%t)//" sec.")
        
        ! solve Linear-ElastoDynamic problem with Reyleigh dumping and Newmark Beta
        call obj%LinearReyleighNewmark()
        
    enddo 
end subroutine
! ##############################################


! ##############################################
subroutine LinearReyleighNewmarkSeismicAnalysis(obj,TOL)
    class(SeismicAnalysis_),intent(inout) :: obj
    type(LinearSolver_) :: solver
    type(IO_) :: f
    real(real64),allocatable :: M_ij(:,:)
    real(real64),allocatable :: C_ij(:,:)
    real(real64),allocatable :: K_ij(:,:)
    real(real64),allocatable :: dF_i(:)
    real(real64),allocatable :: R_i(:)
    real(real64),allocatable :: U_i(:)
    real(real64),allocatable ::dU_i(:)
    real(real64),allocatable :: V_i(:)
    real(real64),allocatable ::dV_i(:)
    real(real64),allocatable :: A_i(:)
    real(real64),allocatable ::dA_i(:)
    real(real64),allocatable :: A_ij(:,:)
    integer(int32) :: i,j,k,l,m,dim_num,n
    integer(int32),allocatable :: FixNodeList(:) 
    real(real64),allocatable   :: Coordinate(:,:)
    real(real64),optional,intent(in) :: TOL
    real(real64) :: TOL_seismic

    TOL_seismic = input(default=dble(1.0e-14),option=TOL )

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

    do  ! Newton's Loop
        ! Element matrix
        call solver%init()

        do i=1,obj%femdomain%ne()
            ! For each element
            ! Ax=b will be installed into solver
            M_ij = obj%femdomain%MassMatrix(ElementID=i,DOF=obj%femdomain%nd() )
            K_ij = obj%femdomain%StiffnessMatrix(ElementID=i,E=1000.0d0,v=0.30d0)
            C_ij = obj%alpha * M_ij + obj%beta * K_ij
            U_i  = obj%femdomain%ElementVector(ElementID=i, GlobalVector=obj%U, DOF=obj%femdomain%nd() )
            V_i  = obj%femdomain%ElementVector(ElementID=i, GlobalVector=obj%V, DOF=obj%femdomain%nd() )
            A_i  = obj%femdomain%ElementVector(ElementID=i, GlobalVector=obj%A, DOF=obj%femdomain%nd() )
            dA_i = obj%femdomain%ElementVector(ElementID=i, GlobalVector=obj%dA, DOF=obj%femdomain%nd() )


            dF_i = obj%femdomain%MassVector(&
                ElementID=i, &
                DOF=obj%femdomain%nd(), &
                Density=0.10d0,&
                Accel=(/0.0d0, 0.0d0, 9.80d0/) &
                )
            R_i = zeros(size(dF_i) )

            ! A_ij dU_j = R_i 
            A_ij = K_ij &
                + 1.0d0/(obj%Newmark_beta * obj%dt * obj%dt)*M_ij &
                + obj%Newmark_delta/(obj%Newmark_beta*obj%dt)*C_ij

            R_i(:) = dF_i(:) + 1.0d0/(obj%Newmark_beta*obj%dt)*matmul(M_ij, V_i)&
                + 1.0d0/(obj%Newmark_beta*2.0d0)*matmul(M_ij, A_i)&
                + obj%Newmark_delta/obj%Newmark_beta*matmul(C_ij, V_i)&
                + (obj%Newmark_delta/2.0d0*obj%Newmark_beta-1.0d0)*obj%dt*matmul(C_ij, A_i)

            ! Assemble stiffness matrix
            call solver%assemble(&
                connectivity=obj%femdomain%connectivity(ElementID=i), &
                DOF = obj%femdomain%nd(), &
                eMatrix = A_ij)
            call solver%assemble(&
                connectivity=obj%femdomain%connectivity(ElementID=i), &
                DOF = obj%femdomain%nd(), &
                eVector = R_i)
            
        enddo


        ! Now [A] {du} = {R} is ready
        ! Solve
        call solver%solve("BiCGSTAB",CRS=.true.)

        obj%dU = solver%x
        
        

        do i=1,obj%femdomain%ne()
            ! For each element
            U_i  = obj%femdomain%ElementVector(ElementID=i,GlobalVector=obj%U, DOF=obj%femdomain%nd())
            dU_i = obj%femdomain%ElementVector(ElementID=i,GlobalVector=obj%dU, DOF=obj%femdomain%nd())
            V_i  = obj%femdomain%ElementVector(ElementID=i,GlobalVector=obj% V, DOF=obj%femdomain%nd())
            A_i  = obj%femdomain%ElementVector(ElementID=i,GlobalVector=obj% A, DOF=obj%femdomain%nd())
            
            dA_i = 1.0d0/(obj%Newmark_beta*obj%dt*obj%dt)*(&
                dU_i - obj%dt*V_i - obj%dt*obj%dt/2.0d0*dA_i &
                )
            
            dV_i = obj%dt * (A_i + obj%Newmark_beta*dA_i )

            U_i = U_i + dU_i
            V_i = V_i + dV_i
            A_i = A_i + dA_i

            ! one for all
            call obj%femdomain%GlobalVector(&
                ElementID=i, &
                ElementVector=U_i, &
                GlobalVector=obj%U, &
                DOF=obj%femdomain%nd() )
            ! one for all
            call obj%femdomain%GlobalVector(&
                ElementID=i, &
                ElementVector=V_i, &
                GlobalVector=obj%V, &
                DOF=obj%femdomain%nd() )

            ! one for all
            call obj%femdomain%GlobalVector(&
                ElementID=i, &
                ElementVector=A_i, &
                GlobalVector=obj%A, &
                DOF=obj%femdomain%nd() )

        enddo
return
        ! obj%error == 0 only if nonlinear elasticity.
        ! if Error <= TOL
        if(obj%error <= TOL_seismic)then
            ! Converged.
            exit
        endif

    enddo

end subroutine
! ##############################################



end module SeismicAnalysisClass