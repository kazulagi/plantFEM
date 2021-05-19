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
        integer(int32),allocatable :: FixNodeList_x(:)
        integer(int32),allocatable :: FixNodeList_y(:)
        integer(int32),allocatable :: FixNodeList_z(:)
        character(1) :: wavetype="z"
        real(real64) :: dt=1.0d0
        real(real64) :: error=0.0d0
        real(real64) :: t=0.0d0
        real(real64) :: alpha = 0.057100d0
        integer(int32) :: step=0
        real(real64) :: beta  = 0.000578d0 ! Rayleigh damping parameters, h=1%
        real(real64) :: Newmark_beta  = 0.250d0 ! Nemark-beta method parameters
        real(real64) :: Newmark_delta  = 0.50d0 ! Nemark-beta method parameters
        logical :: restart=.False.
    contains
        procedure, public :: init => initSeismicAnalysis
        procedure, public :: loadWave => loadWaveSeismicAnalysis
        procedure, public :: fixDisplacement => fixDisplacementSeismicAnalysis 
        procedure, public :: updateWave => updateWaveSeismicAnalysis
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

end subroutine
! ##############################################


! ##############################################
subroutine saveSeismicAnalysis(obj,name,ratio)
    class(SeismicAnalysis_),intent(inout) :: obj
    character(*),intent(in) :: name
    real(real64),optional,intent(in) :: ratio
    real(real64) :: rat
    integer(int32) :: i,j

    rat = input(default=1.0d0,option=ratio)

    do i=1,obj%femdomain%nn()
        do j=1,obj%femdomain%nd()
            obj%femdomain%mesh%nodcoord(i,j) = obj%femdomain%mesh%nodcoord(i,j)&
                + rat*obj%U( obj%femdomain%nd()*(i-1) + j )
        enddo
    enddo

    call obj%femdomain%msh(name=name)

    do i=1,obj%femdomain%nn()
        do j=1,obj%femdomain%nd()
            obj%femdomain%mesh%nodcoord(i,j) = obj%femdomain%mesh%nodcoord(i,j)&
                - rat*obj%U( obj%femdomain%nd()*(i-1) + j )
        enddo
    enddo
    

    !obj%femdomain%mesh%nodcoord(:,:) = obj%femdomain%mesh%nodcoord(:,:) &
    !    - rat*reshape(obj%U,obj%femdomain%nn(),obj%femdomain%nd() )
    
end subroutine
! ##############################################


! ##############################################
subroutine loadWaveSeismicAnalysis(obj,x_min,x_max,y_min,y_max,z_min,z_max,direction)
    class(SeismicAnalysis_),intent(inout) :: obj
    real(real64),optional,intent(in) :: x_min,x_max,y_min,y_max,z_min,z_max
    character(1),optional,intent(in) :: direction ! x, y or z

    if(present(direction) )then
        obj%wavetype = direction
    endif

    obj%WaveNodeList = obj%femdomain%select(&
        x_min=x_min,x_max=x_max,y_min=y_min,y_max=y_max,z_min=z_min,z_max=z_max)
end subroutine
! ##############################################


! ##############################################
subroutine fixDisplacementSeismicAnalysis(obj,x_min,x_max,y_min,y_max,z_min,z_max,direction)
    class(SeismicAnalysis_),intent(inout) :: obj
    real(real64),optional,intent(in) :: x_min,x_max,y_min,y_max,z_min,z_max
    character(*),optional,intent(in) :: direction ! x, y or z


    if(present(direction) )then
        if( trim(direction) == "x" .or.  trim(direction) == "X")then
            obj%FixNodeList_x = obj%femdomain%select(&
                x_min=x_min,x_max=x_max,y_min=y_min,y_max=y_max,z_min=z_min,z_max=z_max)
        elseif( trim(direction) == "y" .or.  trim(direction) == "Y")then
            obj%FixNodeList_y = obj%femdomain%select(&
                x_min=x_min,x_max=x_max,y_min=y_min,y_max=y_max,z_min=z_min,z_max=z_max)
        elseif( trim(direction) == "z" .or.  trim(direction) == "Z")then
            obj%FixNodeList_z = obj%femdomain%select(&
                x_min=x_min,x_max=x_max,y_min=y_min,y_max=y_max,z_min=z_min,z_max=z_max)
        elseif( trim(direction) == "all" .or.  trim(direction) == "ALL")then
        
            obj%FixNodeList_x = obj%femdomain%select(&
            x_min=x_min,x_max=x_max,y_min=y_min,y_max=y_max,z_min=z_min,z_max=z_max)
        
            obj%FixNodeList_y = obj%femdomain%select(&
                x_min=x_min,x_max=x_max,y_min=y_min,y_max=y_max,z_min=z_min,z_max=z_max)

            obj%FixNodeList_z = obj%femdomain%select(&
                x_min=x_min,x_max=x_max,y_min=y_min,y_max=y_max,z_min=z_min,z_max=z_max)
        else
            print *, "ERROR :: loadWaveSeismicAnalysis >> direction should be x, y or z"
            stop 
        endif
    else
        obj%FixNodeList_x = obj%femdomain%select(&
            x_min=x_min,x_max=x_max,y_min=y_min,y_max=y_max,z_min=z_min,z_max=z_max)
        
        obj%FixNodeList_y = obj%femdomain%select(&
            x_min=x_min,x_max=x_max,y_min=y_min,y_max=y_max,z_min=z_min,z_max=z_max)

        obj%FixNodeList_z = obj%femdomain%select(&
            x_min=x_min,x_max=x_max,y_min=y_min,y_max=y_max,z_min=z_min,z_max=z_max)
    endif
end subroutine
! ##############################################



subroutine updateWaveSeismicAnalysis(obj,timestep,direction)
    class(SeismicAnalysis_),intent(inout) :: obj
    integer(int32),intent(in) :: timestep
    character(1),optional,intent(in) :: direction ! x, y or z
    integer(int32) :: node_id,i,dir,dim_num

    if(present(direction) )then
        obj%wavetype = direction
    endif
    if(obj%wavetype=="x" .or.obj%wavetype=="X" )then
        dir=1
    endif
    if(obj%wavetype=="y" .or.obj%wavetype=="Y" )then
        dir=2
    endif
    if(obj%wavetype=="z" .or.obj%wavetype=="Z" )then
        dir=3
    endif

    if(.not. allocated(obj%WaveNodeList) )then
        print *, "Caution >> updateWaveSeismicAnalysis >> no wave"
    endif
    dim_num = obj%femdomain%nd()
    do i=1,size(obj%WaveNodeList)
        node_id = obj%WaveNodeList(i)
        obj%A( dim_num*(node_id-1)+dir ) = obj%wave(timestep ,2)
    enddo

end subroutine

! ##############################################
subroutine runSeismicAnalysis(obj,t0,timestep,wave,restart)
    class(SeismicAnalysis_),intent(inout) :: obj
    type(LinearSolver_) :: solver
    real(real64),optional,intent(in) :: t0
    integer(int32),optional,intent(in) :: timestep
    real(real64),optional,intent(in) :: wave(:,:)
    logical,optional,intent(in) :: restart
    integer(int32) :: i,step

    if(present(restart) )then
        obj%restart = restart
    endif

    if(present(wave) )then
        obj%wave = wave
    endif

    ! set wave
    ! wave = a(t)
    ! dwave = da(t)
    step = input(default=size(obj%wave,1)-1,option=timestep )
    if(.not.obj%restart)then
        obj%dwave = increment(obj%wave,2 )
        obj%t = input(default=0.0d0,option=t0 )
        obj%restart = .True. 
    endif

    do i=1, step
        ! update dt
        obj%dt = obj%wave(i+1,1) - obj%wave(i,1)
        
        ! update time
        obj%t = obj%t + obj%dt
        obj%step = obj%step+1

        call obj%updateWave(timestep=obj%step)

        ! show info.
        call print("SeismicAnalysis >> "//str(obj%t-obj%dt)//"< t <"//str(obj%t)//" sec.")
        
        ! solve Linear-ElastoDynamic problem with Reyleigh dumping and Newmark Beta
        call obj%LinearReyleighNewmark()
        
        call obj%save("step_"//str(obj%step),ratio=1.0d0)

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
    real(real64) :: TOL_seismic,center_accel(3),rho

    
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

    !do  ! Newton's Loop
        ! Element matrix
        call solver%init()

        do i=1,obj%femdomain%ne()
            ! For each element
            ! Ax=b will be installed into solver
            rho = 17000.0d0
            M_ij = rho*obj%femdomain%MassMatrix(ElementID=i,DOF=obj%femdomain%nd() )
            K_ij = obj%femdomain%StiffnessMatrix(ElementID=i,E=10000000.0d0,v=0.00d0)
            C_ij = obj%alpha * M_ij + obj%beta * K_ij
            U_i  = obj%femdomain%ElementVector(ElementID=i, GlobalVector=obj%U, DOF=obj%femdomain%nd() )
            V_i  = obj%femdomain%ElementVector(ElementID=i, GlobalVector=obj%V, DOF=obj%femdomain%nd() )
            A_i  = obj%femdomain%ElementVector(ElementID=i, GlobalVector=obj%A, DOF=obj%femdomain%nd() )
            
            dim_num = obj%femdomain%nd()
            center_accel = 0.0d0
            do j=1,obj%femdomain%nne() 
                do k=1,obj%femdomain%nd() 
                    center_accel(k) = center_accel(k) + A_i(dim_num*(j-1)+k ) 
                enddo
            enddo
            center_accel = center_accel/dble(obj%femdomain%nne())

            dF_i = obj%femdomain%MassVector(&
                ElementID=i, &
                DOF=obj%femdomain%nd(), &
                Density=rho,&
                Accel=center_accel &
                )    
            R_i = zeros(size(dF_i))

            ! A_ij dU_j = R_i 
            A_ij = K_ij &
                + 1.0d0/(obj%Newmark_beta * obj%dt * obj%dt)*M_ij &
                + obj%Newmark_delta/(obj%Newmark_beta*obj%dt)*C_ij

            R_i(:) = dF_i(:) + 1.0d0/(obj%Newmark_beta*obj%dt)*matmul(M_ij, V_i)&
                + 1.0d0/(obj%Newmark_beta*2.0d0)*matmul(M_ij, A_i)&
                + obj%Newmark_delta/obj%Newmark_beta*matmul(C_ij, V_i)&
                + (obj%Newmark_delta/(2.0d0*obj%Newmark_beta)-1.0d0)*obj%dt*matmul(C_ij, A_i)

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

        ! introduce boundary conditions
        if(allocated(obj%FixNodeList_x) )then
            do i=1,size(obj%FixNodeList_x)
                call solver%fix( dim_num*(obj%FixNodeList_x(i)-1)+1, 0.0d0 )
            enddo
        endif
        if(allocated(obj%FixNodeList_y) )then
            do i=1,size(obj%FixNodeList_y)
                call solver%fix( dim_num*(obj%FixNodeList_y(i)-1)+2, 0.0d0 )
            enddo
        endif
        if(allocated(obj%FixNodeList_z) )then
            do i=1,size(obj%FixNodeList_z)
                call solver%fix( dim_num*(obj%FixNodeList_z(i)-1)+3, 0.0d0 )
            enddo
        endif

        ! Now [A] {du} = {R} is ready
        ! Solve
        call solver%solve("BiCGSTAB",CRS=.true.)

        obj%dU = solver%x
        
        print *, maxval(solver%x)

        do i=1,obj%femdomain%ne()
            ! For each element
            U_i  = obj%femdomain%ElementVector(ElementID=i,GlobalVector=obj%U, DOF=obj%femdomain%nd())
            dU_i = obj%femdomain%ElementVector(ElementID=i,GlobalVector=obj%dU, DOF=obj%femdomain%nd())
            V_i  = obj%femdomain%ElementVector(ElementID=i,GlobalVector=obj% V, DOF=obj%femdomain%nd())
            A_i  = obj%femdomain%ElementVector(ElementID=i,GlobalVector=obj% A, DOF=obj%femdomain%nd())
            
            dA_i = 1.0d0/(obj%Newmark_beta*obj%dt*obj%dt)*(&
                dU_i - obj%dt*V_i - obj%dt*obj%dt/2.0d0*A_i &
                )
            
            dV_i = obj%dt * (A_i + obj%Newmark_delta*dA_i )

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
        ! obj%error == 0 only if nonlinear elasticity.
        ! if Error <= TOL
!        if(obj%error <= TOL_seismic)then
!            ! Converged.
!            exit
!        endif
!
!    enddo

end subroutine
! ##############################################



end module SeismicAnalysisClass