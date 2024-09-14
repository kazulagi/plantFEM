use FEMDomainClass
implicit none

type(FEMDomain_) :: cube
integer(int32),allocatable :: SurfaceElements(:,:)

call cube%create("Cylinder3D",x_num=10,y_num=10,z_num=10)
call cube%resize(x=4.0d0,y=4.0d0,z=4.0d0)
call cube%vtk("before")

SurfaceElements = cube%getSurfaceElements(to_range(z_min=1.0d0,z_max=1.30d0))
call cube%extract(SurfaceElements=SurfaceElements,repeat=20)

call cube%vtk("after")



end