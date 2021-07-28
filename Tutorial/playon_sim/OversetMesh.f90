use ContactMechanicsClass
implicit none

type(FEMDomain_),allocatable :: domains(:)
type(ContactMechanics_) :: contact
type(Time_) :: time
integer(int32),allocatable :: FixBoundary(:)
integer(int32) :: contactList(3,3)=0

call time%start()

allocate(domains(3) )

! contact of three cubes
call domains(1)%create(meshtype="Cube3D",x_num=10,y_num=10,z_num=10)
call domains(1)%resize(x=2.0d0,y=2.0d0,z=2.0d0)

call domains(2)%create(meshtype="Cube3D",x_num=10,y_num=10,z_num=10)
call domains(2)%resize(x=2.0d0,y=2.0d0,z=2.0d0)
call domains(2)%move(x=1.9d0)

call domains(3)%create(meshtype="Cube3D",x_num=10,y_num=10,z_num=10)
call domains(3)%resize(x=2.0d0,y=2.0d0,z=2.0d0)
call domains(3)%move(x=3.9d0)

! contact domain#1 => domain#2
contactList(1,2) = 1
! contact domain#2 => domain#3
contactList(2,3) = 1
! initialize simulator
call contact%init(femdomains=domains,contactlist=contactlist)
contact%YoungModulus(1) = 1000.0d0
contact%YoungModulus(2) = 1000.0d0
contact%YoungModulus(3) = 1000.0d0

! setup solver
! Caution >> GaussPointProjection=.true. >> bug exists.
call contact%setup(penaltyparameter=1000.0d0,GaussPointProjection=.False.)
! fix displacement
call time%show()

call contact%fix(direction="x",disp= 0.0d0, DomainID=1,x_max=0.10d0)
call contact%fix(direction="y",disp= 0.0d0, DomainID=1,x_max=0.10d0)
call contact%fix(direction="z",disp= 0.0d0, DomainID=1,x_max=0.10d0)
call contact%fix(direction="z",disp=-0.50d0,DomainID=3,x_min=5.80d0)

! solve > get displacement
call time%show()
call contact%solver%solve("BiCGSTAB")
call time%show()
! update mesh
call contact%updateMesh()
! show results
call domains(1)%vtk("domains(1)_result")
call domains(2)%vtk("domains(2)_result")
call domains(3)%vtk("domains(3)_result")

print *, "Predicted computation time (sec.)"
print *, 10.0d0 ** (1.395460d0*log10(10000000.0d0) - 4.645620d0)/60.0d0/60.0d0

end