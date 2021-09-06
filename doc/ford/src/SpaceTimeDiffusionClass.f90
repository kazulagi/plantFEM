module SpaceTimeDiffusionClass
    use fem
    implicit none

    type :: SpaceTimeDiffusion_
        ! only 1-D problem under unsteady-state
        type(FEMDomain_),pointer :: femdomain => null()
        type(LinearSolver_) :: LinearSolver
        real(real64),allocatable :: P_AB(:,:)
        real(real64),allocatable :: K_AB(:,:)
        real(real64),allocatable :: f1_A(:)
        real(real64),allocatable :: f2_A(:)
        real(real64),allocatable :: h_B(:)
        logical :: initialized = .false.
    contains
        procedure,public :: init => initSpaceTimeDiffusion
        procedure,public :: run => runSpaceTimeDiffusion
    end type

contains

! ################################################################
subroutine initSpaceTimeDiffusion(obj,femdomain)
    class(SpaceTimeDiffusion_),intent(inout) :: obj
    type(FEMDomain_),target,intent(in) :: femdomain
    integer(int32) :: i,j,k,n
    integer(int32) :: num_space_node
    integer(int32) :: num_time_node
    if(associated(obj%femdomain) )then
        nullify(obj%femdomain)
    endif
    obj%femdomain => femdomain

    if(obj%femdomain%mesh%empty() .eqv. .true. )then
        print *, "obj%femdomain%mesh%empty() .eqv. .true."
        return
    endif

    ! #############################################################
    ! CAUTION!
    ! only 1D, with linear interporation for both space and time
    ! #############################################################
    n = obj%femdomain%nne()
    num_space_node = n
    num_time_node = 2
    
    allocate(obj%P_AB(num_space_node*num_time_node,num_space_node*num_time_node) )
    allocate(obj%K_AB(num_space_node*num_time_node,num_space_node*num_time_node) )
    
    allocate(obj%f1_A(num_space_node*num_time_node) )
    allocate(obj%f2_A(num_space_node*num_time_node) )
    
    allocate(obj%h_B(num_space_node*num_time_node) )
    
    obj%P_AB(:,:) = 0.0d0
    obj%K_AB(:,:) = 0.0d0
    obj%f1_A(:) = 0.0d0
    obj%f2_A(:) = 0.0d0
    obj%h_B(:)  = 0.0d0

    obj%initialized = .true.
end subroutine
! ################################################################

subroutine runSpaceTimeDiffusion(obj,dt,initialvalue)
    class(SpaceTimeDiffusion_),intent(inout) :: obj
    real(real64),intent(in) :: dt
    character(*),intent(in) :: initialvalue
    integer(int32) :: i,j,node_id1,node_id2,mat_id,node_id,layerid
    real(real64) :: x1, x2,Le,k,q,val
    real(real64),allocatable :: Mat(:,:),vec(:)

    if(obj%initialized .eqv. .false.)then
        print *, "runSpaceTimeDiffusion :: please initialize it before running simulations"
        return
    endif


    ! #############################################################
    ! CAUTION!
    ! only 1D, with linear interporation for both space and time
    ! #############################################################
    do i=1,obj%femdomain%ne()
        ! for each element,
        node_id1 = obj%femdomain%mesh%elemnod(i,1)
        node_id2 = obj%femdomain%mesh%elemnod(i,2)
        x1 = obj%femdomain%mesh%nodcoord(node_id1,1)
        x2 = obj%femdomain%mesh%nodcoord(node_id2,1)
        mat_id = obj%femdomain%mesh%elemmat(i)
        k = obj%femdomain%MaterialProp%matpara(mat_id,1)
        q = obj%femdomain%MaterialProp%matpara(mat_id,2)
        Le = abs(x1 - x2)


        obj%f1_A(:) = 0.0d0 ! Flux zero for simplicity
        obj%f2_A(:) = q * Le / 4.0d0*dt


        obj%P_AB(1,1) = -1.0d0/6.0d0  * Le
        obj%P_AB(1,2) = -1.0d0/12.0d0 * Le
        obj%P_AB(1,3) =  1.0d0/6.0d0  * Le
        obj%P_AB(1,4) =  1.0d0/12.0d0 * Le

        obj%P_AB(2,1) = -1.0d0/12.0d0  * Le
        obj%P_AB(2,2) = -1.0d0/6.0d0 * Le
        obj%P_AB(2,3) =  1.0d0/12.0d0  * Le
        obj%P_AB(2,4) =  1.0d0/6.0d0 * Le

        obj%P_AB(3,1) = -1.0d0/6.0d0  * Le
        obj%P_AB(3,2) = -1.0d0/12.0d0 * Le
        obj%P_AB(3,3) =  1.0d0/6.0d0  * Le
        obj%P_AB(3,4) =  1.0d0/12.0d0 * Le

        obj%P_AB(4,1) = -1.0d0/12.0d0  * Le
        obj%P_AB(4,2) = -1.0d0/6.0d0 * Le
        obj%P_AB(4,3) =  1.0d0/12.0d0  * Le
        obj%P_AB(4,4) =  1.0d0/6.0d0 * Le

        obj%K_AB(1,1) = 1.0d0/3.0d0*k/Le*dt
        obj%K_AB(1,2) =-1.0d0/3.0d0*k/Le*dt
        obj%K_AB(1,3) = 1.0d0/6.0d0*k/Le*dt
        obj%K_AB(1,4) =-1.0d0/6.0d0*k/Le*dt

        obj%K_AB(2,1) =-1.0d0/3.0d0*k/Le*dt
        obj%K_AB(2,2) = 1.0d0/3.0d0*k/Le*dt
        obj%K_AB(2,3) =-1.0d0/6.0d0*k/Le*dt
        obj%K_AB(2,4) = 1.0d0/6.0d0*k/Le*dt

        obj%K_AB(3,1) = 1.0d0/6.0d0*k/Le*dt
        obj%K_AB(3,2) =-1.0d0/6.0d0*k/Le*dt
        obj%K_AB(3,3) = 1.0d0/3.0d0*k/Le*dt
        obj%K_AB(3,4) =-1.0d0/3.0d0*k/Le*dt

        obj%K_AB(4,1) =-1.0d0/3.0d0*k/Le*dt
        obj%K_AB(4,2) = 1.0d0/3.0d0*k/Le*dt
        obj%K_AB(4,3) =-1.0d0/3.0d0*k/Le*dt
        obj%K_AB(4,4) = 1.0d0/3.0d0*k/Le*dt



        Mat = obj%P_AB
        Mat = Mat + obj%K_AB

        vec = obj%f1_A
        vec = vec + obj%f2_A
        ! time1-node1, time1-node2,time2-node1, time2-node2, time3-node1, ..., etc. 
        call obj%LinearSolver%set( 2*node_id1-1, 2*node_id1-1, entryvalue=Mat(1,1) )
        call obj%LinearSolver%set( 2*node_id1-1, 2*node_id2-1, entryvalue=Mat(1,2) )
        call obj%LinearSolver%set( 2*node_id1-1, 2*node_id1  , entryvalue=Mat(1,3) )
        call obj%LinearSolver%set( 2*node_id1-1, 2*node_id2  , entryvalue=Mat(1,4) )

        call obj%LinearSolver%set( 2*node_id2-1, 2*node_id1-1, entryvalue=Mat(2,1) )
        call obj%LinearSolver%set( 2*node_id2-1, 2*node_id2-1, entryvalue=Mat(2,2) )
        call obj%LinearSolver%set( 2*node_id2-1, 2*node_id1  , entryvalue=Mat(2,3) )
        call obj%LinearSolver%set( 2*node_id2-1, 2*node_id2  , entryvalue=Mat(2,4) )

        call obj%LinearSolver%set( 2*node_id1  , 2*node_id1-1, entryvalue=Mat(3,1) )
        call obj%LinearSolver%set( 2*node_id1  , 2*node_id2-1, entryvalue=Mat(3,2) )
        call obj%LinearSolver%set( 2*node_id1  , 2*node_id1  , entryvalue=Mat(3,3) )
        call obj%LinearSolver%set( 2*node_id1  , 2*node_id2  , entryvalue=Mat(3,4) )

        call obj%LinearSolver%set( 2*node_id2  , 2*node_id1-1, entryvalue=Mat(4,1) )
        call obj%LinearSolver%set( 2*node_id2  , 2*node_id2-1, entryvalue=Mat(4,2) )
        call obj%LinearSolver%set( 2*node_id2  , 2*node_id1  , entryvalue=Mat(4,3) )
        call obj%LinearSolver%set( 2*node_id2  , 2*node_id2  , entryvalue=Mat(4,4) )

        call obj%LinearSolver%set(2*node_id1-1, entryvalue=vec(1) )
        call obj%LinearSolver%set(2*node_id2-1, entryvalue=vec(2) )
        call obj%LinearSolver%set(2*node_id1  , entryvalue=vec(3) )
        call obj%LinearSolver%set(2*node_id2  , entryvalue=vec(4) )

    enddo

    do i=1, size(obj%femdomain%boundary%DboundNodID,1)
        node_id = obj%femdomain%boundary%DboundNodID(i,1)
        val     = obj%femdomain%boundary%DboundVal(i,1)
        call obj%LinearSolver%fix(2*node_id-1, entryvalue=val )
        call obj%LinearSolver%fix(2*node_id  , entryvalue=val )
    enddo

    ! このあと、初期条件tnをfixし、またDirichlet条件をFixする。
    do i=1, size(obj%femdomain%boundary%DboundNodID,1)
        node_id = obj%femdomain%boundary%DboundNodID(i,1)
        val     = obj%femdomain%boundary%DboundVal(i,1)
        call obj%LinearSolver%fix(2*node_id-1, entryvalue=val )
        call obj%LinearSolver%fix(2*node_id  , entryvalue=val )
    enddo
    
    layerid = obj%FEMDomain%getLayerID(name=initialvalue)
    if(size(obj%femdomain%physicalfield(layerid)%scalar,1)/=obj%femdomain%nn() )then
        print *, "ERROR >> runSpaceTimeDiffusion >> size(obj%femdomain%physicalfield(layerid)%scalar,1)/=obj%femdomain%nn"
        return
    endif

    ! node-by-node
    do i=1, size(obj%femdomain%physicalfield(layerid)%scalar,1)
        node_id = i
        val     = obj%femdomain%physicalfield(layerid)%scalar(i)
        call obj%LinearSolver%fix(2*node_id-1, entryvalue=val )
    enddo

    
    call obj%LinearSolver%solve(solver="BiCGSTAB",CRS=.true.)

    ! update value
    do i=1,size(obj%femdomain%physicalfield(layerid)%scalar)
        obj%femdomain%physicalfield(layerid)%scalar(i) = obj%LinearSolver%x(2*i)
    enddo

end subroutine

end module 