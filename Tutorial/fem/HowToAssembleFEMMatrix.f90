use LinearSolverClass
use FEMDomainClass
implicit none

type(LinearSolver_) :: solver
type(FEMDomain_) :: domain
real(real64),allocatable :: A_ij(:,:), x_i(:), b_i(:) ! A x = b

! create Mesh
! create sample mesh (element=1~8, 8-node element)
call domain%create(meshtype="Cube3D",x_num=2, y_num=2,z_num=2)
call domain%resize(x=2.0d0)
call domain%resize(y=2.0d0)
call domain%resize(z=2.0d0)

! initialize solver
call solver%init()

! For 1st element, create stiffness matrix
A_ij = domain%StiffnessMatrix(ElementID=1, E=1000.0d0, v=0.30d0)
b_i  = domain%MassVector(&
    ElementID=1,&
    DOF=domain%nd() ,&
    Density=17.0d0,&
    Accel=(/0.0d0, 0.0d0, -9.80d0/)&
    )

! assemble them 
call solver%assemble(&
    connectivity=domain%connectivity(ElementID=1),&
    DOF=domain%nd() ,&
    eMatrix=A_ij)

! solver % show を実装したい。 
print *, "index_I"
call print(solver%index_I)
print *, "index_J"
call print(solver%index_J)

end