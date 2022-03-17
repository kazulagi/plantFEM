module SeismicAnalysisClass
    use fem
    implicit none

    integer(int32) :: WAVE_DISP = 1
    integer(int32) :: WAVE_VELOCITY = 2
    integer(int32) :: WAVE_ACCEL = 3


    type::SeismicAnalysis_
        type(FEMDomain_),pointer :: femdomain
        type(FEMSolver_) :: femsolver

        
        real(real64),allocatable :: da(:) ! increment of accel.
        real(real64),allocatable :: a(:) ! accel.
        real(real64),allocatable :: a_ext(:) ! External accel.
        real(real64),allocatable :: a_ext_n(:) ! External accel.
        real(real64),allocatable :: v(:) ! velocity
        real(real64),allocatable :: u(:) ! disp.
        real(real64),allocatable :: du(:) ! increment of disp.
        real(real64),allocatable :: wave(:,:)
        real(real64),allocatable :: dwave(:,:)

        real(real64),allocatable :: Density(:)
        real(real64),allocatable :: YoungModulus(:)
        real(real64),allocatable :: PoissonRatio(:)

        real(real64) :: MaxA(3) = 0.0d0
        real(real64) :: MaxV(3) = 0.0d0
        real(real64) :: MaxU(3) = 0.0d0

        integer(int32),allocatable :: WaveNodeList(:)
        
        ! displacement boundary
        integer(int32),allocatable :: FixNodeList_x(:)
        integer(int32),allocatable :: FixNodeList_y(:)
        integer(int32),allocatable :: FixNodeList_z(:)
        real(real64),allocatable :: FixNodeList_Disp_x(:)
        real(real64),allocatable :: FixNodeList_Disp_y(:)
        real(real64),allocatable :: FixNodeList_Disp_z(:)

        integer(int32),allocatable :: absorbingBoundary_x(:)
        integer(int32),allocatable :: absorbingBoundary_y(:)
        integer(int32),allocatable :: absorbingBoundary_z(:)

        ! modal analysis
        real(real64),allocatable :: Frequency(:)
        real(real64),allocatable :: ModeVectors(:,:)


        character(1) :: wavedirection="z"
        integer(int32) :: wavetype = 0
        real(real64) :: dt=1.0d0
        real(real64) :: error=0.0d0
        real(real64) :: t=0.0d0
        integer(int32) :: step=0
        real(real64) :: alpha = 0.52400d0
        real(real64) :: beta  = 0.00129d0 ! Rayleigh dumping parameters, h=1%
        real(real64) :: Newmark_beta  = 0.250d0 ! Nemark-beta method parameters
        real(real64) :: Newmark_gamma = 0.50d0 ! Nemark-beta method parameters
        real(real64) :: boundary_dumping_ratio = 1.0d0
        logical :: restart=.False.
        logical :: debug=.False.
    contains
        procedure, public :: init => initSeismicAnalysis
        procedure, public :: loadWave => loadWaveSeismicAnalysis
        procedure, public :: fixDisplacement => fixDisplacementSeismicAnalysis 
        procedure, public :: updateWave => updateWaveSeismicAnalysis
        procedure, public :: run => runSeismicAnalysis

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

        procedure, public :: modalAnalysis => modalAnalysisSeismicAnalysis
        procedure, public :: vtk  => vtkSeismicAnalysis
    end type

contains

! ##############################################
subroutine initSeismicAnalysis(obj)
    class(SeismicAnalysis_),intent(inout) :: obj

    obj%U = zeros(obj%femdomain%nn()*obj%femdomain%nd() )
    obj%V = zeros(obj%femdomain%nn()*obj%femdomain%nd() )
    obj%A = zeros(obj%femdomain%nn()*obj%femdomain%nd() )

    if(obj%femdomain%mesh%empty() )then
        print *, "[ERROR] Seismic % init >> obj%femdomain is empty"
        stop
    endif
    obj%Density = zeros(obj%femdomain%ne() )
    obj%YoungModulus = zeros(obj%femdomain%ne() )
    obj%PoissonRatio = zeros(obj%femdomain%ne() )
    obj%A_ext = zeros(obj%femdomain%nn()*obj%femdomain%nd() )
    obj%A_ext_n = zeros(obj%femdomain%nn()*obj%femdomain%nd() )

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
subroutine loadWaveSeismicAnalysis(obj,x_min,x_max,y_min,y_max,z_min,z_max,direction,wavetype)
    class(SeismicAnalysis_),intent(inout) :: obj
    real(real64),optional,intent(in) :: x_min,x_max,y_min,y_max,z_min,z_max
    integer(int32),intent(in) :: wavetype !
    character(1),optional,intent(in) :: direction ! x, y or z
    obj%wavetype= wavetype
    
    if(obj%wavetype < 0 .or. obj%wavetype >3)then
        print *, "Invalid loadAs :: WAVE_DISP,WAVE_VELOCITY or WAVE_ACCEL"
        stop
    endif

    if(present(direction) )then
        obj%wavedirection = direction
    endif

    obj%WaveNodeList = obj%femdomain%select(&
        x_min=x_min,x_max=x_max,y_min=y_min,y_max=y_max,z_min=z_min,z_max=z_max)
    
end subroutine
! ##############################################


! ##############################################
subroutine fixDisplacementSeismicAnalysis(obj,x_min,x_max,y_min,y_max,z_min,z_max,displacement,direction,release)
    class(SeismicAnalysis_),intent(inout) :: obj
    real(real64),optional,intent(in) :: x_min,x_max,y_min,y_max,z_min,z_max,displacement
    character(*),optional,intent(in) :: direction ! x, y or z
    character(*),optional,intent(in) :: release
    real(real64) :: disp 
    real(real64),allocatable :: buf(:)

    disp = input(default=0.0d0,option=displacement)

    if(present(release) )then
        if(index(release,"x")/=0 )then
            deallocate(obj%FixNodeList_x)
            deallocate(obj%FixNodeList_Disp_x)
        endif
        if(index(release,"X")/=0 )then
            deallocate(obj%FixNodeList_x)
            deallocate(obj%FixNodeList_Disp_x)
        endif
        if(index(release,"y")/=0 )then
            deallocate(obj%FixNodeList_y)
            deallocate(obj%FixNodeList_Disp_y)
        endif
        if(index(release,"Y")/=0 )then
            deallocate(obj%FixNodeList_y)
            deallocate(obj%FixNodeList_Disp_y)
        endif
        if(index(release,"z")/=0 )then
            deallocate(obj%FixNodeList_z)
            deallocate(obj%FixNodeList_Disp_z)
        endif
        if(index(release,"Z")/=0 )then
            deallocate(obj%FixNodeList_z)
            deallocate(obj%FixNodeList_Disp_z)
        endif
        if(index(release,"all")/=0 )then
            deallocate(obj%FixNodeList_x)
            deallocate(obj%FixNodeList_Disp_x)
            deallocate(obj%FixNodeList_y)
            deallocate(obj%FixNodeList_Disp_y)
            deallocate(obj%FixNodeList_z)
            deallocate(obj%FixNodeList_Disp_z)
        endif
        return
    endif

    if(present(direction) )then
        if( trim(direction) == "x" .or.  trim(direction) == "X")then
            obj%FixNodeList_x = hstack(obj%FixNodeList_x , &
             obj%femdomain%select(&
                x_min=x_min,x_max=x_max,y_min=y_min,y_max=y_max,z_min=z_min,z_max=z_max)&
                )
            buf = zeros(size(obj%FixNodeList_x))
            buf(:) = disp
            if(.not. allocated(obj%FixNodeList_Disp_x) )then
                obj%FixNodeList_Disp_x = buf
            else
                buf(1:size(obj%FixNodeList_Disp_x) ) = obj%FixNodeList_Disp_x(:)
            endif
            obj%FixNodeList_Disp_x = buf

        elseif( trim(direction) == "y" .or.  trim(direction) == "Y")then
            obj%FixNodeList_y = hstack(obj%FixNodeList_y , &
             obj%femdomain%select(&
                x_min=x_min,x_max=x_max,y_min=y_min,y_max=y_max,z_min=z_min,z_max=z_max)&
                )
                buf = zeros(size(obj%FixNodeList_y))
                buf(:) = disp
                if(.not. allocated(obj%FixNodeList_Disp_y) )then
                else
                    buf(1:size(obj%FixNodeList_Disp_y) ) = obj%FixNodeList_Disp_y(:)
                endif
                obj%FixNodeList_Disp_y = buf

        elseif( trim(direction) == "z" .or.  trim(direction) == "Z")then
            obj%FixNodeList_z = hstack(obj%FixNodeList_z , &
             obj%femdomain%select(&
                x_min=x_min,x_max=x_max,y_min=y_min,y_max=y_max,z_min=z_min,z_max=z_max)&
                )
                buf = zeros(size(obj%FixNodeList_z))
                buf(:) = disp

            if(.not. allocated(obj%FixNodeList_Disp_z) )then
            else
                buf(1:size(obj%FixNodeList_Disp_z) ) = obj%FixNodeList_Disp_z(:)
            endif
            obj%FixNodeList_Disp_z = buf
    
        elseif( trim(direction) == "all" .or.  trim(direction) == "ALL")then
        
            obj%FixNodeList_x = hstack(obj%FixNodeList_x , &
             obj%femdomain%select(&
            x_min=x_min,x_max=x_max,y_min=y_min,y_max=y_max,z_min=z_min,z_max=z_max)&
            )
            buf = zeros(size(obj%FixNodeList_x))
            buf(:) = disp

            if(.not. allocated(obj%FixNodeList_Disp_x) )then
            else
                buf(1:size(obj%FixNodeList_Disp_x) ) = obj%FixNodeList_Disp_x(:)
            endif
            obj%FixNodeList_Disp_x = buf
        
            obj%FixNodeList_y = hstack(obj%FixNodeList_y , &
             obj%femdomain%select(&
                x_min=x_min,x_max=x_max,y_min=y_min,y_max=y_max,z_min=z_min,z_max=z_max)&
                )
                buf = size(zeros(size(obj%FixNodeList_y)))
                buf(:) = disp

                if(.not. allocated(obj%FixNodeList_Disp_y) )then
                else
                    buf(1:size(obj%FixNodeList_Disp_y) ) = obj%FixNodeList_Disp_y(:)
                endif
                obj%FixNodeList_Disp_y = buf    

            obj%FixNodeList_z = hstack(obj%FixNodeList_z , &
             obj%femdomain%select(&
                x_min=x_min,x_max=x_max,y_min=y_min,y_max=y_max,z_min=z_min,z_max=z_max)&
                )
                buf = zeros(size(obj%FixNodeList_z))
            buf(:) = disp

            if(.not. allocated(obj%FixNodeList_Disp_z) )then
            else
                buf(1:size(obj%FixNodeList_Disp_z) ) = obj%FixNodeList_Disp_z(:)
            endif
            obj%FixNodeList_Disp_z = buf
        else
            print *, "ERROR :: loadWaveSeismicAnalysis >> direction should be x, y or z"
            stop 
        endif
    else
        obj%FixNodeList_x = hstack(obj%FixNodeList_x , &
        obj%femdomain%select(&
            x_min=x_min,x_max=x_max,y_min=y_min,y_max=y_max,z_min=z_min,z_max=z_max)&
            )
            buf = zeros(size(obj%FixNodeList_x))
            buf(:) = disp
        
            if(.not. allocated(obj%FixNodeList_Disp_x) )then
            else
                buf(1:size(obj%FixNodeList_Disp_x) ) = obj%FixNodeList_Disp_x(:)
            endif
            obj%FixNodeList_Disp_x = buf

        obj%FixNodeList_y = hstack(obj%FixNodeList_y , &
        obj%femdomain%select(&
            x_min=x_min,x_max=x_max,y_min=y_min,y_max=y_max,z_min=z_min,z_max=z_max)&
            )
            buf = size(zeros(size(obj%FixNodeList_y)))
            buf(:) = disp

            if(.not. allocated(obj%FixNodeList_Disp_y) )then
            else
                buf(1:size(obj%FixNodeList_Disp_y) ) = obj%FixNodeList_Disp_y(:)
            endif
            obj%FixNodeList_Disp_y = buf

        obj%FixNodeList_z = hstack(obj%FixNodeList_z , &
        obj%femdomain%select(&
            x_min=x_min,x_max=x_max,y_min=y_min,y_max=y_max,z_min=z_min,z_max=z_max)&
            )
            buf = zeros(size(obj%FixNodeList_z))
            buf(:) = disp
            
            if(.not. allocated(obj%FixNodeList_Disp_z) )then
            else
                buf(1:size(obj%FixNodeList_Disp_z) ) = obj%FixNodeList_Disp_z(:)
            endif
            obj%FixNodeList_Disp_z = buf

    endif
end subroutine
! ##############################################



subroutine updateWaveSeismicAnalysis(obj,timestep,direction)
    class(SeismicAnalysis_),intent(inout) :: obj
    integer(int32),intent(in) :: timestep
    character(1),optional,intent(in) :: direction ! x, y or z
    integer(int32) :: node_id,i,dir,dim_num

    if(present(direction) )then
        obj%wavedirection = direction
    endif
    if(obj%wavedirection=="x" .or.obj%wavedirection=="X" )then
        dir=1
    endif
    if(obj%wavedirection=="y" .or.obj%wavedirection=="Y" )then
        dir=2
    endif
    if(obj%wavedirection=="z" .or.obj%wavedirection=="Z" )then
        dir=3
    endif

    if(.not. allocated(obj%WaveNodeList) )then
        print *, "Caution >> updateWaveSeismicAnalysis >> no wave"
    endif
    
    dim_num = obj%femdomain%nd()
    if(obj%wavetype==WAVE_ACCEL)then
        obj%A_ext_n = obj%A_ext
        do i=1,size(obj%WaveNodeList)
            node_id = obj%WaveNodeList(i)
            obj%A( dim_num*(node_id-1)+dir ) = obj%wave(timestep ,2)
            ! update accel
            obj%A_ext( dim_num*(node_id-1)+dir ) =  obj%wave(timestep ,2)
        enddo
        
    elseif(obj%wavetype==WAVE_VELOCITY)then
        do i=1,size(obj%WaveNodeList)
            node_id = obj%WaveNodeList(i)
            obj%V( dim_num*(node_id-1)+dir ) = obj%wave(timestep ,2)
        enddo
    elseif(obj%wavetype==WAVE_DISP)then
        do i=1,size(obj%WaveNodeList)
            node_id = obj%WaveNodeList(i)
            obj%U( dim_num*(node_id-1)+dir ) = obj%wave(timestep ,2)
        enddo
    endif

end subroutine

! ##############################################
subroutine runSeismicAnalysis(obj,t0,timestep,wave,AccelLimit,disp_magnify_ratio,use_same_stiffness)
    class(SeismicAnalysis_),intent(inout) :: obj
    integer(int32),intent(in) :: timestep(2)
    logical,optional,intent(in) :: use_same_stiffness! Use A' for all t_n, A'x=b

    type(LinearSolver_) :: solver
    type(IO_) :: U, V, A
    real(real64),optional,intent(in) :: t0,disp_magnify_ratio
    real(real64),optional,intent(in) :: wave(:,:),AccelLimit
    integer(int32) :: i,j
    real(real64) :: ratio

    ratio = input(default=1.0d0,option=disp_magnify_ratio)
    if(present(wave) )then
        obj%wave = wave
    endif

    do i=timestep(1),timestep(2)-1
        ! update dt
        obj%dt = abs(obj%wave(i+1,1) - obj%wave(i,1))
        
        ! update time
        obj%step = i
        obj%t = obj%dt*obj%Step
        call obj%updateWave(timestep=obj%step+1)
        ! show info.
        call print("SeismicAnalysis >> "//str(obj%t-obj%dt)//"< t <"//str(obj%t)//" sec.")
        
        ! solve Linear-ElastoDynamic problem with Reyleigh dumping and Newmark Beta
        if(present(AccelLimit) )then
            if(maxval(obj%A)>=AccelLimit)then
                print *, "[Caution] :: runSeismicAnalysis >> exceeds AccelLimit!"
                return    
            endif
        endif

        call obj%LinearReyleighNewmark()

        call obj%recordMaxValues()
        ! Export results
        call obj%save("step_"//str(obj%step),ratio=ratio)

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
    real(real64),allocatable :: F_i(:)
    real(real64),allocatable :: dF_i(:)
    real(real64),allocatable :: R_i(:)
    real(real64),allocatable :: U_i(:)
    real(real64),allocatable ::dU_i(:)
    real(real64),allocatable :: V_i(:)
    real(real64),allocatable ::dV_i(:)
    real(real64),allocatable :: A_i(:)
    real(real64),allocatable :: A_ext_i(:)
    real(real64),allocatable :: A_ext_i_n(:)
    real(real64),allocatable ::dA(:)
    real(real64),allocatable ::dV(:)
    real(real64),allocatable ::dU(:)

    real(real64),allocatable ::u_upd(:)
    real(real64),allocatable ::v_upd(:)
    real(real64),allocatable ::a_upd(:)


    real(real64),allocatable ::U_n(:)
    real(real64),allocatable ::V_n(:)
    real(real64),allocatable ::A_n(:)

    real(real64),allocatable :: A_ij(:,:)
    integer(int32) :: i,j,k,l,m,dim_num,n
    integer(int32),allocatable :: FixNodeList(:) ,DomainIDs(:)
    real(real64),allocatable   :: Coordinate(:,:)
    real(real64),optional,intent(in) :: TOL
    real(real64) :: TOL_seismic,center_accel(3),rho,gravity(3),a(0:7)


    gravity(:)=0.0d0
    gravity(3) = -9.81d0
    
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

    ! Element matrix
    call solver%init(NumberOfNode=[obj%femdomain%nn()],DOF=3)
    obj%dt = abs(obj%dt)

    if(obj%debug) then
        print *, '[Seismic] Creating Element Matrices...'
    endif
    do i=1,obj%femdomain%ne()
        ! For each element
        ! Ax=b will be installed into solv -obj%A_ext_n)
        rho = obj%Density(i)
        M_ij = obj%femdomain%MassMatrix(ElementID=i,DOF=obj%femdomain%nd(),density=rho )
        K_ij = obj%femdomain%StiffnessMatrix(ElementID=i,E=obj%YoungModulus(i),v=obj%PoissonRatio(i) )
        C_ij = obj%alpha * M_ij + obj%beta * K_ij
        U_i  = obj%femdomain%ElementVector(ElementID=i, GlobalVector=obj%U, DOF=obj%femdomain%nd() )
        V_i  = obj%femdomain%ElementVector(ElementID=i, GlobalVector=obj%V, DOF=obj%femdomain%nd() )
        A_i  = obj%femdomain%ElementVector(ElementID=i, GlobalVector=obj%A, DOF=obj%femdomain%nd() )
        A_ext_i  = obj%femdomain%ElementVector(ElementID=i, GlobalVector=obj%A_ext, DOF=obj%femdomain%nd() )
        A_ext_i_n  = obj%femdomain%ElementVector(ElementID=i, GlobalVector=obj%A_ext_n, DOF=obj%femdomain%nd() )
        
        dim_num = obj%femdomain%nd()
        
        F_i = matmul(M_ij,A_ext_i)
        
        A_ij = obj%getNewmarkBetaMatrix(M=M_ij, C=C_ij,K=K_ij,&
            beta=obj%Newmark_beta,gamma=obj%Newmark_gamma,dt=obj%dt)
        R_i = obj%getNewmarkBetaVector(M=M_ij,C=C_ij,u_n=U_i,v_n=V_i, a_n=A_i, &
            force=F_i,beta=obj%Newmark_beta,gamma=obj%Newmark_gamma,dt=obj%dt)
        !print *, maxval(A_ij),minval(A_ij)
        !print *, maxval(R_i),minval(R_i)
        !stop
        ! Assemble stiffness matrix
        DomainIDs = zeros(obj%femdomain%nne() )
        DomainIDs(:) = 1
        
        call solver%assemble(&
            connectivity=obj%femdomain%connectivity(ElementID=i), &
            DOF = obj%femdomain%nd(), &
            DomainIDs=DomainIDs,&
            eMatrix = A_ij)

        call solver%assemble(&
            connectivity=obj%femdomain%connectivity(ElementID=i), &
            DOF = obj%femdomain%nd(), &
            DomainIDs=DomainIDs,&
            eVector = R_i)
    enddo


    ! absorbing boundary
    if(obj%debug) then
        print *, '[Seismic] Setting absorbing boundaries...'
        print *, size(obj%absorbingBoundary_x)
        print *, size(obj%absorbingBoundary_y)
        print *, size(obj%absorbingBoundary_z)
    endif
    ! setup absorbing boundary

    ! as dumper
    !    ----
    ! --- |  |----
    !    ----
    ! with dumping ratio = obj%boundary_dumping_ratio: C
    ! F_i  = C v_i

    solver % b(:) = solver % b(:) + obj%getAbsorbingBoundaryForce()

    if(obj%debug) then
        print *, '[Seismic] Fixing Boundaries...'
        print *, size(obj%FixNodeList_x)
        print *, size(obj%FixNodeList_y)
        print *, size(obj%FixNodeList_z)
        
    endif
    solver%debug = obj%debug
    if(allocated(obj%FixNodeList_x) )then
        do i=1,size(obj%FixNodeList_x)
            call solver%fix( NodeID=obj%FixNodeList_x(i)*3-2,&
            entryvalue=obj%FixNodeList_Disp_x(i) ,&
            row_DomainID=1,&
            debug=solver%debug)
        enddo
    endif
    if(allocated(obj%FixNodeList_y) )then
        do i=1,size(obj%FixNodeList_y)
            call solver%fix( NodeID=obj%FixNodeList_y(i)*3-1,&
            entryvalue=obj%FixNodeList_Disp_y(i),&
            row_DomainID=1,&
            debug=solver%debug)
        enddo
    endif
    if(allocated(obj%FixNodeList_z) )then
        do i=1,size(obj%FixNodeList_z)
            call solver%fix( NodeID=obj%FixNodeList_z(i)*3,&
            entryvalue=obj%FixNodeList_Disp_z(i),&
            row_DomainID=1,&
            debug=solver%debug)
        enddo
    endif
    ! Now [A] {du} = {R} is ready
    ! Solve
    
    if(obj%debug) then
        print *, '[Seismic] Solving...'
    endif
    call solver%solve("BiCGSTAB")

    print *, maxval(solver%val),minval(solver%val)
    print *, maxval(solver%x),minval(solver%x)
    
    u_upd = solver%x
    v_upd = obj%updateVelocityNewmarkBeta(u=u_upd,u_n=obj%U,v_n=obj%V,a_n=obj%A,&
        gamma=obj%newmark_gamma,beta=obj%newmark_beta,dt=obj%dt)
    a_upd = obj%updateAccelNewmarkBeta(u=u_upd,u_n=obj%U,v_n=obj%V,a_n=obj%A,&
        gamma=obj%newmark_gamma,beta=obj%newmark_beta,dt=obj%dt)
    
    obj%U = u_upd
    obj%V = v_upd
    obj%A = a_upd
   
    print *, "U"
    print *, minval(obj%U),maxval(obj%U)
    
    print *, "V"
    print *, minval(obj%V),maxval(obj%V)
    
    print *, "A"
    print *, minval(obj%A), maxval(obj%A)
    
end subroutine
! ##############################################


subroutine recordMaxValuesSeismicAnalysis(obj)
    class(SeismicAnalysis_),intent(inout) :: Obj
    real(real64),allocatable :: array(:,:)
    integer(int32) :: i
    
    array = reshape( obj%U,obj%femdomain%nn(),obj%femdomain%nd() )
    array = abs(array)
    do i=1,3
        obj%maxU(i) = maxval( [ abs(obj%maxU(i)) , maxval(array(:,i) ) ]  )
    enddo
    

    array = reshape( obj%V,obj%femdomain%nn(),obj%femdomain%nd() )
    array = abs(array)
    do i=1,3
        obj%maxV(i) = maxval( [ abs(obj%maxV(i)) , maxval(array(:,i) ) ]  )
    enddo


    array = reshape( obj%A,obj%femdomain%nn(),obj%femdomain%nd() )
    array = abs(array)
    do i=1,3
        obj%maxA(i) = maxval( [ abs(obj%maxA(i)) , maxval(array(:,i) ) ]  )
    enddo

end subroutine

function getNewmarkBetaMatrixSeismicAnalysis(obj,M,C,K,beta,gamma,dt) result(ret)
    class(SeismicAnalysis_),intent(in) :: obj
    real(real64),intent(in) :: M(:,:),C(:,:),K(:,:),beta,gamma,dt
    real(real64),allocatable :: ret(:,:)
    integer(int32) :: n

    n = size(M,1)
    ret = zeros(n,n)
    ret(:,:) = 1.0d0/( beta*dt*dt ) * M(:,:) &
        + gamma/( beta*dt ) * C(:,:) &
        + K(:,:)

end function

function getNewmarkBetaVectorSeismicAnalysis(obj,u_n, v_n, a_n, force,beta,gamma,M,C,dt) result(ret)
    class(SeismicAnalysis_),intent(in) :: obj
    real(real64),intent(in) :: u_n(:), v_n(:), a_n(:),force(:),beta,gamma,dt
    real(real64),intent(in) :: M(:,:),C(:,:)
    real(real64),allocatable :: ret(:)
    real(real64) :: a(8)
    integer(int32) :: n

    n = size(U_n,1)
    ret = zeros(n)
    a (1) = gamma/(beta*dt)
    a (2) = - gamma/(beta*dt)
    a (3) = 1.0d0 - gamma/beta
    a (4) = dt*(1.0d0 - gamma/(2.0d0*beta) )
    a (5) = 1.0d0/(beta*dt*dt)
    a (6) = - 1.0d0/(beta*dt*dt)
    a (7) = - 1.0d0/(beta*dt)
    a (8) = 1.0d0-1.0d0/(2.0d0*beta)
    ret(:) = force(:) &
        - a(6)*matmul(M,u_n) - a(7)*matmul(M,v_n) - a(8)*matmul(M,a_n) &
        - a(2)*matmul(C,u_n) - a(3)*matmul(C,v_n) - a(4)*matmul(C,a_n)
end function
! ##########################################################################################
function updateVelocityNewmarkBetaSeismicAnalysis(obj,u,u_n,v_n,a_n,gamma,beta,dt) result(ret)
    class(SeismicAnalysis_),intent(in) :: obj
    real(real64),intent(in) :: u(:), u_n(:), v_n(:), a_n(:),beta,gamma,dt
    real(real64),allocatable :: ret(:)
    integer(int32) :: n

    ret = zeros(size(u) )
    ! usually, gamma=0.5, beta=0.25
    !ret(:) = gamma/(beta*dt)*u(:) &
    !    - gamma/(beta*dt)*u_n(:) &
    !    + (1.0d0 - gamma/beta )*v_n(:) &
    !    + dt*(1.0d0 - gamma/(2.0d0*beta) )*a_n(:)

    ret(:) = gamma/(beta*dt)*u(:) & ! modified
        - gamma/(beta*dt)*u_n(:) &  ! modified
        + (1.0d0 - gamma/beta )*v_n(:) &
        + dt*(1.0d0 - gamma/(2.0d0*beta) )*a_n(:)
    ! confirmed 2021/10/13
end function
! ##########################################################################################


! ##########################################################################################
function updateAccelNewmarkBetaSeismicAnalysis(obj,u,u_n,v_n,a_n,gamma,beta,dt) result(ret)
    class(SeismicAnalysis_),intent(in) :: obj
    real(real64),intent(in) :: u(:), u_n(:), v_n(:), a_n(:),beta,gamma,dt
    real(real64),allocatable :: ret(:)
    integer(int32) :: n

    ret = zeros(size(u) )

    !ret(:) = 1.0d0/(beta*dt*dt)*u(:) &
    !    - 1.0d0/(beta*dt*dt)*u_n(:) &
    !    - 1.0d0/(beta*dt)*v_n(:) &
    !    + (1.0d0 - 1.0d0/(2.0d0*beta) )*a_n(:)
    ! confirmed 2021/10/13
    ret(:) = 1.0d0/(beta*dt*dt)*u(:) &
        - 1.0d0/(beta*dt*dt)*u_n(:) &
        - 1.0d0/(beta*dt)*v_n(:) &
        + (1.0d0 - 1.0d0/(2.0d0*beta) )*a_n(:)
end function
! ##########################################################################################

subroutine removeSeismicAnalysis(obj)
    class(SeismicAnalysis_),intent(inout) :: obj

    if(associated(obj%femdomain))then
        nullify(obj%femdomain)
    endif
    
    if(allocated(obj%da) )then !(:) ! increment of accel.
        deallocate(obj%da)
    endif
    if(allocated(obj%a) )then !(:) ! accel.
        deallocate(obj%a)
    endif
    if(allocated(obj%a_ext) )then !(:) ! External accel.
        deallocate(obj%a_ext)
    endif
    if(allocated(obj%a_ext_n) )then !(:) ! External accel.
        deallocate(obj%a_ext_n)
    endif
    if(allocated(obj%v) )then !(:) ! velocity
        deallocate(obj%v)
    endif
    if(allocated(obj%u) )then !(:) ! disp.
        deallocate(obj%u)
    endif
    if(allocated(obj%du) )then !(:) ! increment of disp.
        deallocate(obj%du)
    endif
    if(allocated(obj%wave) )then !(:,:)
        deallocate(obj%wave)
    endif
    if(allocated(obj%dwave) )then !(:,:)
        deallocate(obj%dwave)
    endif

    if(allocated(obj%Density) )then !(:)
        deallocate(obj%Density)
    endif
    if(allocated(obj%YoungModulus) )then !(:)
        deallocate(obj%YoungModulus)
    endif
    if(allocated(obj%PoissonRatio) )then !(:)
        deallocate(obj%PoissonRatio)
    endif

    obj%MaxA(1:3) = 0.0d0
    obj%MaxV(1:3) = 0.0d0
    obj%MaxU(1:3) = 0.0d0

    if(allocated(obj%WaveNodeList))then!(:)
        deallocate(obj%WaveNodeList)
    endif
    if(allocated(obj%FixNodeList_x))then!(:)
        deallocate(obj%FixNodeList_x)
    endif
    if(allocated(obj%FixNodeList_y))then!(:)
        deallocate(obj%FixNodeList_y)
    endif
    if(allocated(obj%FixNodeList_z))then!(:)
        deallocate(obj%FixNodeList_z)
    endif

    if(allocated(obj%FixNodeList_Disp_x) )then !(:)
        deallocate(obj%FixNodeList_Disp_x)
    endif
    if(allocated(obj%FixNodeList_Disp_y) )then !(:)
        deallocate(obj%FixNodeList_Disp_y)
    endif
    if(allocated(obj%FixNodeList_Disp_z) )then !(:)
        deallocate(obj%FixNodeList_Disp_z)
    endif
    obj%wavedirection="z"
    obj%wavetype = 0
    obj%dt=1.0d0
    obj%error=0.0d0
    obj%t=0.0d0
    obj%step=0
    obj%alpha = 0.52400d0
    obj%beta  = 0.00129d0 ! Rayleigh dumping parameters, h=1%
    obj%Newmark_beta  = 0.250d0 ! Nemark-beta method parameters
    obj%Newmark_gamma  = 0.50d0 ! Nemark-beta method parameters
    obj%restart=.False.

    
end subroutine

! #############################################################
subroutine absorbingBoundarySeismicAnalysis(obj,x_min,x_max,y_min,y_max,z_min,z_max,&
    direction,dumping_ratio)
    class(SeismicAnalysis_),intent(inout) :: obj
    real(real64),optional,intent(in) :: x_min,x_max,y_min,y_max,z_min,z_max,&
        dumping_ratio
    character(1) :: direction
    integer(int32),allocatable :: selected_nodes(:)
    
    if(present(dumping_ratio) )then
        obj%boundary_dumping_ratio = dumping_ratio
    endif

    selected_nodes = obj%femdomain%select(&
        x_min = x_min,&
        x_max = x_max,&
        y_min = y_min,&
        y_max = y_max,&
        z_min = z_min,&
        z_max = z_max)

    if   ( direction=="x" .or. direction=="X" )then
        obj%absorbingBoundary_x = obj%absorbingBoundary_x // selected_nodes
    elseif(direction=="y" .or. direction=="Y")then
        obj%absorbingBoundary_y = obj%absorbingBoundary_y // selected_nodes
    elseif(direction=="z" .or. direction=="Z")then
        obj%absorbingBoundary_z = obj%absorbingBoundary_z // selected_nodes
    else
        print *, "ERROR ::absorbingBoundarySeismicAnalysis >> invalid direction: ",direction
        print *, "direction = {x, X, y, Y, z, Z}"
    endif

    
end subroutine
! #############################################################


! #############################################################
pure function getAbsorbingBoundaryForceSeismicAnalysis(obj) result(force)
    class(SeismicAnalysis_),intent(in) :: obj
    real(real64),allocatable :: force(:)
    integer(int32) :: i

    force = zeros(obj%femdomain%nn()*obj%femdomain%nd() ) 
    if(allocated(obj%absorbingBoundary_x ) )then
        do i=1,size(obj%absorbingBoundary_x)
            force((obj%absorbingBoundary_x(i)-1)*obj%femdomain%nd() + 1 ) &
             = - obj%boundary_dumping_ratio*&
             obj%V((obj%absorbingBoundary_x(i)-1)*obj%femdomain%nd() + 1 )
        enddo
    endif

    if(allocated(obj%absorbingBoundary_y ) )then
        do i=1,size(obj%absorbingBoundary_y)
            force((obj%absorbingBoundary_y(i)-1)*obj%femdomain%nd() + 2 ) &
             = - obj%boundary_dumping_ratio*&
             obj%V((obj%absorbingBoundary_y(i)-1)*obj%femdomain%nd() + 2 )
        enddo
    endif

    if(allocated(obj%absorbingBoundary_z ) )then
        do i=1,size(obj%absorbingBoundary_z)
            force((obj%absorbingBoundary_z(i)-1)*obj%femdomain%nd() + 3 ) &
             = - obj%boundary_dumping_ratio*&
             obj%V((obj%absorbingBoundary_z(i)-1)*obj%femdomain%nd() + 3 )
        enddo
    endif

end function
! #############################################################

subroutine modalAnalysisSeismicAnalysis(this,femdomain,YoungModulus,PoissonRatio,Density,&
    fix_node_list_x,fix_node_list_y,fix_node_list_z)
    class(SeismicAnalysis_),intent(inout) :: this
    type(FEMDomain_),intent(inout),target :: femdomain
    real(real64),intent(in) :: YoungModulus(:), PoissonRatio(:),Density(:)
    
    type(IO_) :: f

    integer(int32) :: i
    real(real64) :: Vs,t,dt,E_Al
    real(real64),allocatable :: Mode_U(:),mode_Ut(:),freq(:),eigen_value(:),eigen_vectors(:,:)
    integer(int32),allocatable :: node_list(:)
    integer(int32),optional,intent(in) :: fix_node_list_x(:)
    integer(int32),optional,intent(in) :: fix_node_list_y(:)
    integer(int32),optional,intent(in) :: fix_node_list_z(:)
    
    ! Modal analysis
    
    if(associated(this%femdomain) )then
        nullify(this%femdomain)
    endif
    this%femdomain =>femdomain

    !read file
    call this%femsolver%init(NumDomain=1,NumInterfaceElement=0)
    call this%femsolver%setDomain(FEMDomain=femdomain,DomainID=1)
    call this%femsolver%setCRS(DOF=femdomain%nd() )
    
    
    !$OMP parallel do
    do i=1,femdomain%ne()
        call this%femsolver%setMatrix(&
            DomainID=1,&
            ElementID=i,&
            DOF=3,&
            Matrix=femdomain%MassMatrix(ElementID=i,Density=Density(i),DOF=3) &
            )
    enddo
    !$OMP end parallel do
    
    call this%femsolver%keepThisMatrixAs("B")
    call this%femsolver%zeros()

    print *, "Save Stiffness Matrix"
    
    !$OMP parallel do
    do i=1,femdomain%ne()
        call this%femsolver%setMatrix(&
            DomainID=1,&
            ElementID=i,&
            DOF=3,&
            Matrix=femdomain%StiffnessMatrix(ElementID=i,E=YoungModulus(i),v=PoissonRatio(i) ) &
            )
    enddo
    !$OMP end parallel do

    call this%femsolver%keepThisMatrixAs("A")
    
    ! Eigen value problem solver by scipy
    
!    print *, "solver%eig"
    if(present(fix_node_list_x) )then
        node_list = fix_node_list_x
        node_list =(node_list(:)-1)*3+1
        call this%femsolver%fix_eig(IDs=node_list)
    endif

    if(present(fix_node_list_y) )then
        node_list = fix_node_list_y
        node_list =(node_list(:)-1)*3+2
        call this%femsolver%fix_eig(IDs=node_list)
    endif

    if(present(fix_node_list_z) )then
        node_list = fix_node_list_z
        node_list =(node_list(:)-1)*3+3
        call this%femsolver%fix_eig(IDs=node_list)
    endif

    call this%femsolver%eig(eigen_value=eigen_value,eigen_vectors=eigen_vectors)
    
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

subroutine vtkSeismicAnalysis(this,name,num_mode,amp,scalar_field)
    class(SeismicAnalysis_),intent(in) :: this
    character(*),intent(in) :: name
    integer(int32),intent(in) :: num_mode
    real(real64),intent(in) :: amp
    real(real64),optional,intent(in) :: scalar_field(:)
    integer(int32) :: i,j,n
    real(real64),allocatable :: Mode_U(:),mode_Ut(:),freq(:)
    real(real64) :: t, dt
    ! 20 modes
    t = 0.0d0
    dt = 0.0d0
    
    do i=1,num_mode
        mode_U = zeros(size(this%modevectors,1))
        mode_U = this%modevectors(:,i)
        dt = 1.0d0/this%frequency(i)/100.0d0
        do j=1,100
            t = dt * dble(j-1)
            mode_Ut = mode_U*cos( 2.0d0*3.140d0*this%frequency(i)*t )

            this%femdomain%mesh%nodcoord = this%femdomain%mesh%nodcoord &
            +amp*reshape(mode_Ut,this%femdomain%nn(),3 ) 

            call this%femdomain%vtk(name+str(i)+"_t_"+str(j),scalar=scalar_field)
            
            this%femdomain%mesh%nodcoord = this%femdomain%mesh%nodcoord &
            -amp*reshape(mode_Ut,this%femdomain%nn(),3 ) 
        enddo
    enddo
    


end subroutine

end module SeismicAnalysisClass