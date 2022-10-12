use ContactMechanicsClass
implicit none

type(FEMDomain_),allocatable :: domains(:)
type(ContactMechanics_) :: contact
integer(int32),allocatable :: FixBoundary(:)
integer(int32) :: contactList(3,3)=0

allocate(domains(3) )
! contact of three cubes
call domains(1)%create(meshtype="Cube3D",y_num=3,z_num=3)
call domains(1)%resize(x=2.0d0,y=2.0d0,z=2.0d0)

call domains(2)%create(meshtype="Cube3D",y_num=3,z_num=3)
call domains(2)%resize(x=2.0d0,y=2.0d0,z=2.0d0)
call domains(2)%move(x=1.9d0)

call domains(3)%create(meshtype="Cube3D",y_num=3,z_num=3)
call domains(3)%resize(x=2.0d0,y=2.0d0,z=2.0d0)
call domains(3)%move(x=3.9d0)

! contact domain#1 => domain#2
contactList(1,2) = 1
! contact domain#2 => domain#3
contactList(2,3) = 1
! initialize simulator

call contact%init(femdomains=domains,contactlist=contactlist)
contact%YoungModulus(1) = 1000.0d0
contact%YoungModulus(2) = 10000.0d0
contact%YoungModulus(3) = 1000.0d0
! setup solver
call contact%setup(penaltyparameter=500.0d0)
! fix displacement

call contact%fix(direction="x",disp= 0.0d0, DomainID=1,x_max=0.10d0)
call contact%fix(direction="y",disp= 0.0d0, DomainID=1,x_max=0.10d0)
call contact%fix(direction="z",disp= 0.0d0, DomainID=1,x_max=0.10d0)
call contact%fix(direction="x",disp= 0.0d0, DomainID=3,x_min=5.80d0)
call contact%fix(direction="y",disp= 0.0d0, DomainID=3,x_min=5.80d0)
call contact%fix(direction="z",disp=-0.50d0,DomainID=3,x_min=5.80d0)

! solve > get displacement
call contact%solver%solve("BiCGSTAB")
! update mesh
call contact%updateMesh()
! show results
call domains(1)%msh("domains(1)_result")
call domains(2)%msh("domains(2)_result")
call domains(3)%msh("domains(3)_result")

end