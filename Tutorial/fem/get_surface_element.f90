use MeshClass
use FEMDomainClass
implicit none

type(Mesh_) :: mesh
type(FEMDomain_) :: cube


call cube%create("Cube3D",x_num=2,y_num=2,z_num=2)
call cube%vtk("cube")
mesh = cube%mesh
call mesh%getFacetElement()
call print(mesh%FacetElemNod)

end