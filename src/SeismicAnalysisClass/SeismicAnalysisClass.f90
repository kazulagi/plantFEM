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
        character(1) :: wavedirection="z"
        integer(int32) :: wavetype = 0
        real(real64) :: dt=1.0d0
        real(real64) :: error=0.0d0
        real(real64) :: t=0.0d0
        real(real64) :: alpha = 0.52400d0
        integer(int32) :: step=0
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
subroutine fixDisplacementSeismicAnalysis(obj,x_min,x_max,y_min,y_max,z_min,z_max,direction)
    class(SeismicAnalysis_),intent(inout) :: obj
    real(real64),optional,intent(in) :: x_min,x_max,y_min,y_max,z_min,z_max
    character(*),optional,intent(in) :: direction ! x, y or z


    if(present(direction) )then
        if( trim(direction) == "x" .or.  trim(direction) == "X")then
            obj%FixNodeList_x = hstack(obj%FixNodeList_x , &
             obj%femdomain%select(&
                x_min=x_min,x_max=x_max,y_min=y_min,y_max=y_max,z_min=z_min,z_max=z_max)&
                )
        elseif( trim(direction) == "y" .or.  trim(direction) == "Y")then
            obj%FixNodeList_y = hstack(obj%FixNodeList_y , &
             obj%femdomain%select(&
                x_min=x_min,x_max=x_max,y_min=y_min,y_max=y_max,z_min=z_min,z_max=z_max)&
                )
        elseif( trim(direction) == "z" .or.  trim(direction) == "Z")then
            obj%FixNodeList_z = hstack(obj%FixNodeList_z , &
             obj%femdomain%select(&
                x_min=x_min,x_max=x_max,y_min=y_min,y_max=y_max,z_min=z_min,z_max=z_max)&
                )
        elseif( trim(direction) == "all" .or.  trim(direction) == "ALL")then
        
            obj%FixNodeList_x = hstack(obj%FixNodeList_x , &
             obj%femdomain%select(&
            x_min=x_min,x_max=x_max,y_min=y_min,y_max=y_max,z_min=z_min,z_max=z_max)&
            )
        
            obj%FixNodeList_y = hstack(obj%FixNodeList_y , &
             obj%femdomain%select(&
                x_min=x_min,x_max=x_max,y_min=y_min,y_max=y_max,z_min=z_min,z_max=z_max)&
                )

            obj%FixNodeList_z = hstack(obj%FixNodeList_z , &
             obj%femdomain%select(&
                x_min=x_min,x_max=x_max,y_min=y_min,y_max=y_max,z_min=z_min,z_max=z_max)&
                )
        else
            print *, "ERROR :: loadWaveSeismicAnalysis >> direction should be x, y or z"
            stop 
        endif
    else
        obj%FixNodeList_x = hstack(obj%FixNodeList_x , &
         obj%femdomain%select(&
            x_min=x_min,x_max=x_max,y_min=y_min,y_max=y_max,z_min=z_min,z_max=z_max)&
            )
        
        obj%FixNodeList_y = hstack(obj%FixNodeList_y , &
         obj%femdomain%select(&
            x_min=x_min,x_max=x_max,y_min=y_min,y_max=y_max,z_min=z_min,z_max=z_max)&
            )

        obj%FixNodeList_z = hstack(obj%FixNodeList_z , &
         obj%femdomain%select(&
            x_min=x_min,x_max=x_max,y_min=y_min,y_max=y_max,z_min=z_min,z_max=z_max)&
            )
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
subroutine runSeismicAnalysis(obj,t0,timestep,wave,AccelLimit)
    class(SeismicAnalysis_),intent(inout) :: obj
    integer(int32),intent(in) :: timestep(2)

    type(LinearSolver_) :: solver
    type(IO_) :: U, V, A
    real(real64),optional,intent(in) :: t0
    real(real64),optional,intent(in) :: wave(:,:),AccelLimit
    integer(int32) :: i,j

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
        call obj%save("step_"//str(obj%step),ratio=100.0d0)

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
    real(real64),allocatable :: A_ext_i(:)
    real(real64),allocatable :: A_ext_i_n(:)
    real(real64),allocatable ::dA(:)
    real(real64),allocatable ::dV(:)
    real(real64),allocatable ::dU(:)


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

    !do  ! Newton's Loop
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
            !center_accel = 0.0d0
            !do j=1,obj%femdomain%nne() 
            !    do k=1,obj%femdomain%nd() 
            !        center_accel(k) = center_accel(k) + A_i(dim_num*(j-1)+k ) 
            !    enddo
            !enddo
            !center_accel = center_accel/dble(obj%femdomain%nne())

            ! If external accelaration is loaded,
            ! introduce them.
            !do j=1,size(A_ext_i)
            !    if(A_ext_i(j)/=0.0d0 )then
            !        A_i(j) = A_ext_i(j)
            !    endif
            !enddo


            !dF_i = obj%femdomain%MassVector(&
            !    ElementID=i, &
            !    DOF=obj%femdomain%nd(), &
            !    Density=rho,&
            !    Accel=gravity &
            !    )
            
            ! >> original
            ! A_ij dU_j = R_i 
            !A_ij = K_ij &
            !    + 1.0d0/(obj%Newmark_beta * obj%dt * obj%dt)*M_ij &
            !    + obj%Newmark_gamma/(obj%Newmark_beta*obj%dt)*C_ij

            !R_i(:) = dF_i(:) &
            !    + 1.0d0/(obj%Newmark_beta*obj%dt*obj%dt)*matmul(M_ij, U_i)&
            !    + 1.0d0/(obj%Newmark_beta*obj%dt)*matmul(M_ij, V_i)&
            !    + (1.0d0/(2.0d0*obj%Newmark_beta) - 1.0d0) *matmul(M_ij, A_i)&
            !    + obj%Newmark_gamma/(obj%Newmark_beta*obj%dt)*matmul(C_ij, U_i)&
            !    + (obj%Newmark_gamma/obj%Newmark_beta - 1.0d0)*matmul(C_ij, V_i)&
            !    + (obj%Newmark_gamma/(2.0d0*obj%Newmark_beta)-1.0d0)*obj%dt*matmul(C_ij, A_i)
            !<<< original
            
            ! External Force
            dF_i = matmul(M_ij,A_ext_i)
            !dF_i(:) = 0.0d0
            ! 
            R_i = zeros(size(dF_i)) 

            ! https://ocw.u-tokyo.ac.jp/lecture_files/fs_01/9/notes/ja/09.PDF
            a(0) = 1.0d0/(obj%Newmark_beta*obj%dt*obj%dt)
            a(1) = obj%Newmark_gamma/(obj%Newmark_beta*obj%dt)
            a(2) = 1.0d0/(obj%Newmark_beta*obj%dt)
            a(3) = 1.0d0/(2.0d0*obj%Newmark_beta) - 1.0d0
            a(4) = obj%Newmark_gamma/(obj%Newmark_beta) - 1.0d0
            a(5) = obj%Newmark_gamma/(2.0d0*obj%Newmark_beta) - 1.0d0
            a(6) = obj%Newmark_gamma*obj%dt
            a(7) = (1.0d0-obj%Newmark_gamma)*obj%dt

!            A_ij = K_ij &
!                + 1.0d0/(obj%Newmark_beta * obj%dt * obj%dt)*M_ij &
!                + obj%Newmark_gamma/(obj%Newmark_beta*obj%dt)*C_ij
            A_ij = M_ij
!            R_i  = dF_i - matmul(K_ij,U_i) + a(2)*matmul(M_ij,V_i) + a(3)*matmul(M_ij,A_i)&
!                + a(4)*matmul(C_ij, V_i) + a(5)*matmul(C_ij, A_i)
            R_i  = dF_i - matmul(K_ij,U_i) - matmul(C_ij,V_i) !- matmul(M_ij,A_i) 
!            R_i(:) = dF_i(:) &
!                + 1.0d0/(obj%Newmark_beta*obj%dt*obj%dt)*matmul(M_ij, U_i)&
!                + 1.0d0/(obj%Newmark_beta*obj%dt)*matmul(M_ij, V_i)&
!                + (1.0d0/(2.0d0*obj%Newmark_beta)-1.0d0) *matmul(M_ij, A_i)&
!                + (obj%Newmark_gamma/(obj%Newmark_beta*obj%dt) )*matmul(C_ij, U_i)&
!                + (obj%Newmark_gamma/obj%Newmark_beta - 1.0d0)*matmul(C_ij, V_i)&
!                + (obj%Newmark_gamma/(2.0d0*obj%Newmark_beta)-1.0d0)*obj%dt*matmul(C_ij, A_i)

!            R_i(:) = dF_i(:) &
!                + 1.0d0/(obj%Newmark_beta*obj%dt)*matmul(M_ij, V_i)&
!                + 1.0d0/(2.0d0*obj%Newmark_beta) *matmul(M_ij, A_i)&
!                + (obj%Newmark_gamma/obj%Newmark_beta)*matmul(C_ij, V_i)&
!                + (obj%Newmark_gamma/(2.0d0*obj%Newmark_beta)-1.0d0)*obj%dt*matmul(C_ij, A_i)
            ! checked!
            !call f%open("debug.txt")
            !call f%write("M_ij")
            !call f%write(M_ij)
            !call f%write("K_ij")
            !call f%write(K_ij)
            !call f%write("C_ij")
            !call f%write(C_ij)
            !call f%write("A_ij")
            !call f%write(A_ij)
            !call f%write("R_i")
            !call f%write(R_i)
            !call f%close()
            !print *, "rho",rho, "dt",obj%dt
            !stop
            ! << revision

            ! revision based on https://www.sciencedirect.com/topics/engineering/newmark-method
            
            !A_ij = K_ij &
            !    + 1.0d0/(obj%Newmark_beta * obj%dt * obj%dt)*M_ij &
            !    + obj%Newmark_gamma/(obj%Newmark_beta*obj%dt)*C_ij
            !
            
            !R_i(:) = dF_i(:) - matmul(M_ij,A_ext_i) &
            !    + 1.0d0/(obj%Newmark_beta*obj%dt*obj%dt)*matmul(M_ij, U_i)&
            !    + 1.0d0/(obj%Newmark_beta*obj%dt)*matmul(M_ij, V_i)&
            !    + (1.0d0/2.0d0/obj%Newmark_beta-1.0d0) *matmul(M_ij, A_i)&
            !    + (obj%Newmark_gamma/obj%Newmark_beta*obj%dt)*matmul(C_ij, U_i)&
            !    - (1.0d0 - obj%Newmark_gamma/obj%Newmark_beta)*matmul(C_ij, V_i)&
            !    - (1.0d0 - obj%Newmark_gamma/(2.0d0*obj%Newmark_beta))*obj%dt*matmul(C_ij, A_i)
!
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
                entryvalue=0.0d0 ,&
                row_DomainID=1)
            enddo
        endif
        if(allocated(obj%FixNodeList_y) )then
            do i=1,size(obj%FixNodeList_y)
                call solver%fix( NodeID=obj%FixNodeList_y(i)*3-1,&
                entryvalue=0.0d0,&
                row_DomainID=1)
            enddo
        endif
        if(allocated(obj%FixNodeList_z) )then
            do i=1,size(obj%FixNodeList_z)
                call solver%fix( NodeID=obj%FixNodeList_z(i)*3,&
                entryvalue=0.0d0,&
                row_DomainID=1)
            enddo
        endif

        ! Now [A] {du} = {R} is ready
        ! Solve
        
        call solver%solve("BiCGSTAB")
        print *, maxval(solver%val),minval(solver%val)
        print *, maxval(solver%x),minval(solver%x)
        
        
        A_n = obj%A
        V_n = obj%V
        U_n = obj%U

        ! revision
        ! https://ocw.u-tokyo.ac.jp/lecture_files/fs_01/9/notes/ja/09.PDF
        obj%A = solver%x
        
        obj%V = V_n + obj%dt * obj%A 
        
        obj%U = U_n + obj%V*obj%dt
            ! >>> original
        !obj%V = obj%Newmark_gamma/(obj%Newmark_beta*obj%dt)*(obj%U - U_n)&
        !        + (1.0d0 - obj%Newmark_gamma/2.0d0/obj%Newmark_beta)*V_n&
        !        + obj%dt*(1.0d0 - obj%Newmark_gamma/2.0d0/obj%Newmark_beta )*A_n
        !
        !obj%A = 1.0d0/obj%Newmark_beta/obj%dt*(obj%U - U_n)&
        !        - 1.0d0/obj%Newmark_beta/obj%dt * V_n &
        !        - (1.0d0/obj%Newmark_beta/2.0d0 - 1.0d0)*A_n
        ! <<< original 


        ! >>> revision based on IGS 2021        
        !dA = 1.0d0/obj%Newmark_beta/obj%dt/obj%dt*solver%x &
        !- 1.0d0/2.0d0/obj%Newmark_beta*obj%A &
        !- 1.0d0/obj%Newmark_beta/obj%dt*obj%V ! two-checked
        !obj%V = obj%V  &
        !    + obj%dt*obj%A &
        !    + obj%Newmark_gamma*obj%dt*dA
        !obj%A = obj%A + dA
        !obj%U = solver%x
        !obj%V = obj%Newmark_gamma/(obj%Newmark_beta*obj%dt)*(solver%x-U_n)&
        !        + (1.0d0 - obj%Newmark_gamma/2.0d0/obj%Newmark_beta)*V_n&
        !        + obj%dt*(1.0d0 - obj%Newmark_gamma/2.0d0/obj%Newmark_beta )*A_n
        !
        !obj%A = (solver%x-U_n)/(obj%Newmark_beta*obj%dt*obj%dt)&
        !    - V_n/(obj%Newmark_beta*obj%dt) &
        !    + (1.0d0 - 1.0d0/(2.0d0*obj%Newmark_beta) )*A_n 
        
        
        ! >> revision based on https://www.sciencedirect.com/topics/engineering/newmark-method
        ! Eq. 5.43

        ! Nishioka et al T. Nishioka, in Comprehensive Structural Integrity, 
        !obj%U = solver%x
        !obj%V = obj%Newmark_gamma*(obj%U - U_n)/(obj%Newmark_beta*obj%dt)&
        !    + V_n * (1.0d0 - obj%Newmark_gamma/obj%Newmark_beta)&
        !    + obj%dt * A_n * (1.0d0 - obj%Newmark_gamma/(2.0d0*obj%Newmark_beta) )
!
        !obj%A = (obj%U - U_n)/(obj%Newmark_beta*obj%dt*obj%dt)&
        !    - V_n/(obj%Newmark_beta*obj%dt) &
        !    + (1.0d0 - 1.0d0/(2.0d0*obj%Newmark_beta) )*A_n 
!
        ! <<< revision 
        !if(obj%step <=20)then
        !    print *,obj%step,maxval(abs(obj%A)),maxval(abs(obj%A_ext)),maxval(abs(obj%A_ext_n))
        !    call print( [maxval(solver%x),minval(solver%x)] )
        !
        !    if(obj%step==20) stop
        !endif
                        

        print *, "U"
        print *, minval(obj%U),maxval(obj%U)
        print *, "U_n"
        print *, minval(U_n), maxval(U_n)
        print *, "V"
        print *, minval(obj%V),maxval(obj%V)
        print *, "V_n"
        print *, minval(V_n), maxval(A_n)
        print *, "A"
        print *, minval(obj%A), maxval(obj%A)
        print *, "A_n"
        print *, minval(A_n), maxval(A_n)

        !if(obj%step==100000000)then
        !    stop
        !endif
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

end module SeismicAnalysisClass