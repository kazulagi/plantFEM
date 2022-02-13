use FEMDomainClass
implicit none

type(FEMDomain_) :: cube
real(real64),allocatable :: scalar_field(:)


call cube%create("Cube3D",x_num=20,y_num=20,z_num=20)
scalar_field = linspace([1.0d0, dble(cube%ne()) ],cube%ne())
call cube%vtk("cube_1",scalar = scalar_field )
! about 3 min.
scalar_field = cube%MovingAverageFilter(scalar_field)
call cube%vtk("cube_2",scalar = scalar_field )



end