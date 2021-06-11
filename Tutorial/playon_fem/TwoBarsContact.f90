use FEMDomainClass
use LinearSolverClass
implicit none
type(FEMDomain_) :: domain1, domain2
type(LinearSolver_) :: solver 
type(ShapeFunction_) :: sf
integer(int32),allocatable :: domainIDs1(:),domainIDs2(:),domainIDs12(:),InterConnect(:)
real(real64) :: position(3)
real(real64),parameter :: penalty=100.0d0
integer(int32) :: ElementID, i,NodeID
integer(int32),allocatable :: FixBoundary(:)
real(real64),allocatable :: A_ij(:,:), x_i(:), b_i(:) ! A x = b

! A simple contact solver

call domain1%create(meshtype="Cube3D",y_num=3,z_num=3)
call domain1%resize(x=2.0d0,y=2.0d0,z=2.0d0)
call domain1%msh("domain")
call domain2%create(meshtype="Cube3D",y_num=3,z_num=3)
call domain2%resize(x=2.0d0,y=2.0d0,z=2.0d0)
call domain2%move(x=1.9d0)
call domain2%msh("domain2")

! initialize solver
call solver%init(NumberOfNode=(/ domain1%nn(), domain2%nn() /), DOF = 3 )

! Domain#1
! create Elemental Matrices and Vectors
print *, "Domain1: started."
allocate(DomainIDs1(domain1%nne()*domain1%nd() ) )
DomainIDs1(:) = 1
do ElementID=1, domain1%ne()
    ! For 1st element, create stiffness matrix
    A_ij = domain1%StiffnessMatrix(ElementID=ElementID, E=1000.0d0, v=0.40d0)
    b_i  = domain1%MassVector(&
        ElementID=ElementID,&
        DOF=domain1%nd() ,&
        Density=0.30d0,&
        Accel=(/0.0d0, 0.0d0, 0.0d0/)&
        )
    ! assemble them 
    call solver%assemble(&
        connectivity=domain1%connectivity(ElementID=ElementID),&
        DOF=domain1%nd() ,&
        eMatrix=A_ij,&
        DomainIDs=DomainIDs1)
    call solver%assemble(&
        connectivity=domain1%connectivity(ElementID=ElementID),&
        DOF=domain1%nd(),&
        eVector=b_i,&
        DomainIDs=DomainIDs1)
enddo
! Domain#2
print *, "Domain2: started."
! create Elemental Matrices and Vectors
allocate(DomainIDs2(domain2%nne()*domain2%nd() ) )
DomainIDs2(:) = 2
do ElementID=1, domain2%ne()
    ! For 1st element, create stiffness matrix
    A_ij = domain2%StiffnessMatrix(ElementID=ElementID, E=1000.0d0, v=0.40d0)
    b_i  = domain2%MassVector(&
        ElementID=ElementID,&
        DOF=domain2%nd() ,&
        Density=0.30d0,&
        Accel=(/0.0d0, 0.0d0, 0.0d0/)&
        )
    ! assemble them 
    call solver%assemble(&
        connectivity=domain2%connectivity(ElementID=ElementID),&
        DOF=domain2%nd() ,&
        eMatrix=A_ij,&
        DomainIDs=DomainIDs2)
    call solver%assemble(&
        connectivity=domain2%connectivity(ElementID=ElementID),&
        DOF=domain2%nd(),&
        eVector=b_i,&
        DomainIDs=DomainIDs2)
enddo
! Interface
print *, "Interface: started."
! create Elemental Matrices and Vectors
allocate(DomainIDs12(domain2%nne()+1 ) )
allocate(InterConnect(domain2%nne()+1 ) )
DomainIDs12(1) = 1
DomainIDs12(2:) = 2
do NodeID=1, domain2%nn()
    ! For 1st element, create stiffness matrix
    ! set global coordinate
    position(:) = domain1%mesh%nodcoord(NodeID,:)
    InterConnect(1) = NodeID
    if( domain2%mesh%nearestElementID(x=position(1),y=position(2),z=position(3))<=0 )then
        cycle
    endif
    InterConnect(2:) = domain2%connectivity(domain2%mesh%nearestElementID(x=position(1),y=position(2),z=position(3) ))
    A_ij = penalty*domain2%connectMatrix(position,DOF=domain2%nd() ) 
    ! assemble them 
    call solver%assemble(&
        connectivity=InterConnect,&
        DOF=domain2%nd() ,&
        eMatrix=A_ij,&
        DomainIDs=DomainIDs12)    
enddo

! fix deformation >> Dirichlet Boundary
FixBoundary = domain1%select(x_max=0.10d0)
do i=1,size(FixBoundary)
    call solver%fix(nodeid=FixBoundary(i), EntryID=1, entryvalue=0.0d0, DOF=domain2%nd() ,row_DomainID=1)
    call solver%fix(nodeid=FixBoundary(i), EntryID=2, entryvalue=0.0d0, DOF=domain2%nd() ,row_DomainID=1)
    call solver%fix(nodeid=FixBoundary(i), EntryID=3, entryvalue=0.0d0, DOF=domain2%nd() ,row_DomainID=1)
enddo
deallocate(FixBoundary)
FixBoundary = domain2%select(x_min=maxval(domain2%mesh%nodcoord(:,1) )-0.10d0 )
do i=1,size(FixBoundary)
    call solver%fix(nodeid=FixBoundary(i), EntryID=1, entryvalue=0.0d0, DOF=domain2%nd() ,row_DomainID=2)
    call solver%fix(nodeid=FixBoundary(i), EntryID=2, entryvalue=0.50d0, DOF=domain2%nd() ,row_DomainID=2)
    call solver%fix(nodeid=FixBoundary(i), EntryID=3, entryvalue=0.00d0, DOF=domain2%nd() ,row_DomainID=2)
enddo
! solve > get displacement
call solver%solve("BiCGSTAB")
domain1%mesh%nodcoord(:,:) = domain1%mesh%nodcoord(:,:) +&
    reshape( solver%x(1:domain1%nd()*domain1%nn() ),domain1%nn(),domain1%nd() )
domain2%mesh%nodcoord(:,:) = domain2%mesh%nodcoord(:,:) +&
    reshape( solver%x(domain1%nn()*domain1%nd()+1:),domain2%nn(),domain2%nd() )
call domain1%msh("domain1_result")
call domain2%msh("domain2_result")
end