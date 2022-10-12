use FEMDomainClass
implicit none

type(FEMDomain_) :: cube
real(real64),allocatable :: center(:),rotation(:)

call cube%create("Cube3D")

! set initial position
center = cube%centerPosition()
call cube%move(x=-center(1),y=-center(2),z=-center(3) )
call cube%vtk("cube_"+zfill(1, 4) )

! rotate!
call cube%rotate(x=radian(10.0d0),y=radian(10.0d0),z=radian(10.0d0))
call cube%vtk("cube_"+zfill(2, 4) )

! roll back!
call cube%rotate(x=-radian(10.0d0),y=-radian(10.0d0),z=-radian(10.0d0))
call cube%vtk("cube_"+zfill(3, 4) )

end