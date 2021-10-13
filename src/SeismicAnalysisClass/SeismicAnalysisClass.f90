module SeismicAnalysisClass
    use fem
    implicit none

    integer(int32) :: WAVE_DISP = 1
    integer(int32) :: WAVE_VELOCITY = 2
    integer(int32) :: WAVE_ACCEL = 3


    type::SeismicAnalysis_
        type(FEMDomain_),pointer :: femdomain
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
        integer(int32),allocatable :: FixNodeList_x(:)
        integer(int32),allocatable :: FixNodeList_y(:)
        integer(int32),allocatable :: FixNodeList_z(:)
        real(real64),allocatable :: FixNodeList_Disp_x(:)
        real(real64),allocatable :: FixNodeList_Disp_y(:)
        real(real64),allocatable :: FixNodeList_Disp_z(:)
        character(1) :: wavedirection="z"
        integer(int32) :: wavetype = 0
        real(real64) :: dt=1.0d0
        real(real64) :: error=0.0d0
        real(real64) :: t=0.0d0
        integer(int32) :: step=0
        real(real64) :: alpha = 0.52400d0
        real(real64) :: beta  = 0.00129d0 ! Rayleigh damping parameters, h=1%
        real(real64) :: Newmark_beta  = 0.250d0 ! Nemark-beta method parameters
        real(real64) :: Newmark_gamma  = 0.50d0 ! Nemark-beta method parameters
        logical :: restart=.False.
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
            !obj%A( dim_num*(node_id-1)+dir ) = obj%wave(timestep ,2)
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
subroutine runSeismicAnalysis(obj,t0,timestep,wave,AccelLimit,disp_magnify_ratio)
    class(SeismicAnalysis_),intent(inout) :: obj
    integer(int32),intent(in) :: timestep(2)

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


        if(allocated(obj%FixNodeList_x) )then
            do i=1,size(obj%FixNodeList_x)
                call solver%fix( NodeID=obj%FixNodeList_x(i)*3-2,&
                entryvalue=obj%FixNodeList_Disp_x(i) ,&
                row_DomainID=1)
            enddo
        endif
        if(allocated(obj%FixNodeList_y) )then
            do i=1,size(obj%FixNodeList_y)
                call solver%fix( NodeID=obj%FixNodeList_y(i)*3-1,&
                entryvalue=obj%FixNodeList_Disp_y(i),&
                row_DomainID=1)
            enddo
        endif
        if(allocated(obj%FixNodeList_z) )then
            do i=1,size(obj%FixNodeList_z)
                call solver%fix( NodeID=obj%FixNodeList_z(i)*3,&
                entryvalue=obj%FixNodeList_Disp_z(i),&
                row_DomainID=1)
            enddo
        endif

        ! Now [A] {du} = {R} is ready
        ! Solve
        
        
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

end module SeismicAnalysisClass