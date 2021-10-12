use IOClass
use ArrayClass
use FEMDomainClass
implicit none

type(IO_) :: f
type(FEMDomain_) :: domain

! allocatable array
real(real64),allocatable :: a(:), c(:)
integer(int32),allocatable :: c_int(:)
real(real64),allocatable :: c_matrix(:,:)


call domain%create("Sphere3D")
call domain%vtk("Shpere")

! dump JSON
a = linspace([0.0d0, 10.0d0],10)
call f%open("test.json","w")
call f%dump("sample_vector",a)
call f%dump("axis",[1,2,3])
call f%dump("Number_of_mesh",1)
call f%dump("Mesh_version",1.0)

! export character
call f%dump("Mesh_shape","Sphere")

! export nodes of mesh
call f%dump("cube_position_x",domain%x() )
call f%dump("cube_position_y",domain%y() )
call f%dump("cube_position_z",domain%z() )
call f%close()

! parse Vector from JSON
c = f%parse("test.json",key1="sample_vector") 
call print(c)

! parse Vector from JSON
c_int = f%parse("test.json",key1="axis") 
call print(c_int)

! bind vectors into a matrix
c_matrix = matrix(domain%x(),domain%y(),domain%z())



end