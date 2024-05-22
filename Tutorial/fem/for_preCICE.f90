program main
    use plantfem
    implicit none

type(FEMDomain_) :: domain
integer(int32),allocatable :: vertexIDs(:)
real(real64),allocatable :: vertices(:),vertexData(:),scalar_field(:)
type(Time_) :: time
type(IO_) :: f

call domain%create("Cube3D",x_num=100,y_num=100,z_num=100)
call domain%resize(x=10.0d0,y=30.0d0,z=80.0d0)
scalar_field = ones(domain%nn())

!call domain%vtk(name="input",scalar=scalar_field)

! mesh to vertices
call domain%getVertices(vertices,vertexIDs) !***NEW***!

! mesh data to vertex data
vertexData = domain%to_vertexData(vertexIDs,scalar_field)  !***NEW***!

call vtk_file("vertices",vertices=vertices,vertexData=vertexData)


! update vertex data
vertexData = 2.0d0

! vertex data to mesh data
scalar_field(vertexIDs(:)) = vertexData(:) 

call domain%vtk("result",scalar=scalar_field)

end program main