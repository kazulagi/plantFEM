use FEMDomainClass
implicit none

type(FEMDomain_) :: cubes(1:2)
type(Time_) :: time
integer(int32) :: n=1 !16

! mesh joint
call cubes(1)%create("Cube3D",x_num=3*n,y_num=4*n,z_num=5*n)
call cubes(1)%resize(x=3.0d0,y=4.0d0,z=5.0d0)
call cubes(2)%create("Cube3D",x_num=3*n,y_num=4*n,z_num=5*n)
call cubes(2)%resize(x=3.0d0,y=4.0d0,z=5.0d0)
call cubes(2)%move(z=4.80d0)

call cubes(1)%vtk("cube_001")
call cubes(2)%vtk("cube_002")

print *, cubes(1)%nn()+cubes(2)%nn()

call time%start()
call cubes(1)%bond(domain=cubes(2),radius=0.50d0)
call time%show()

print *, cubes(1)%nn(),cubes(2)%nn()

call cubes(1)%vtk("cube_001")
call cubes(2)%vtk("cube_002")
call cubes(1)%vtk("cube_1and2")

end