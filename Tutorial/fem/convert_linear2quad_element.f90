use MeshClass
use FEMDomainClass
implicit none

type(FEMDomain_) :: domain
real(real64),allocatable :: scalar_field(:)
type(Random_) :: random

call domain%create("Cube3D",x_num=20,y_num=30,z_num=40)
call domain%resize(x=20.0d0,y=30.0d0,z=40.0d0)
call domain%vtk("domain_linear_elem.vtk")

call domain%changeElementType(ElementType=[3,20,8])
scalar_field = random%randn(domain%nn())
call domain%vtk("domain_quad_elem.vtk",scalar=scalar_field)

end