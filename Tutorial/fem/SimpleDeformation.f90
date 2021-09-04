use LinearSolverClass
use FEMDomainClass
implicit none

type(LinearSolver_) :: solver
type(FEMDomain_) :: domain
type(IO_) :: f
integer(int32) :: ElementID, i
integer(int32),allocatable :: FixBoundary(:)
real(real64),allocatable :: A_ij(:,:), x_i(:), b_i(:) ! A x = b

! create Mesh
! create sample mesh (element=1~8, 8-node element)
call domain%create(meshtype="Cube3D",x_num=5, y_num=10,z_num=5)
call domain%resize(x=2.0d0)
call domain%resize(y=8.0d0)
call domain%resize(z=2.0d0)
call domain%move(x=3.0d0)

! show initial mesh
call domain%msh("init")
call domain%move(x=-3.0d0)

! initialize solver
call solver%init(NumberOfNode=[domain%nn()],DOF=3 )

! create Elemental Matrices and Vectors
do ElementID=1, domain%ne()

    ! For 1st element, create stiffness matrix
    A_ij = domain%StiffnessMatrix(ElementID=ElementID, E=100.0d0, v=0.40d0)
    b_i  = domain%MassVector(&
        ElementID=ElementID,&
        DOF=domain%nd() ,&
        Density=0.00d0,&
        Accel=(/0.0d0, 0.0d0, -9.80d0/)&
        )

    ! assemble them 
    call solver%assemble(&
        connectivity=domain%connectivity(ElementID=ElementID),&
        DOF=domain%nd() ,&
        eMatrix=A_ij)
    call solver%assemble(&
        connectivity=domain%connectivity(ElementID=ElementID),&
        DOF=domain%nd(),&
        eVector=b_i)
enddo

call solver%prepareFix()

! fix deformation >> Dirichlet Boundary
FixBoundary = domain%select(y_max=0.10d0)
do i=1,size(FixBoundary)
    call solver%fix(nodeid=FixBoundary(i)*3-2,entryvalue=0.0d0,row_DomainID=1)
    call solver%fix(nodeid=FixBoundary(i)*3-1,entryvalue=0.0d0,row_DomainID=1)
    call solver%fix(nodeid=FixBoundary(i)*3-0,entryvalue=0.0d0,row_DomainID=1)
enddo

FixBoundary = domain%select(y_min=maxval(domain%mesh%nodcoord(:,2) ) )
do i=1,size(FixBoundary)
    call solver%fix(nodeid=FixBoundary(i)*3-2,entryvalue=0.0d0,row_DomainID=1)
    call solver%fix(nodeid=FixBoundary(i)*3-1,entryvalue=0.0d0,row_DomainID=1)
    call solver%fix(nodeid=FixBoundary(i)*3-0,entryvalue=1.0d0,row_DomainID=1)
enddo


! solve > get displacement
call solver%solve("BiCGSTAB")

! x = X + u
domain%mesh%Nodcoord(:,:) = domain%mesh%Nodcoord(:,:) + reshape(solver%x,domain%nn(),domain%nd() )

! show result
call domain%msh("result")

end