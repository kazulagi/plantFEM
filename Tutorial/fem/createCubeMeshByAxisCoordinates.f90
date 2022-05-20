use ArrayClass
use FEMDomainClass

type(FEMDomain_) :: cube
real(real64),allocatable :: x(:),y(:),z(:)

x = [0.0d0, 3.0d0, 10.0d0]
y = [0.0d0, 1.0d0, 12.0d0]
z = [0.0d0, 4.0d0, 20.0d0]


call cube%create("Cube3D",x_axis=x,y_axis=y,z_axis=z)
call cube%vtk("cube_0")

do i_i=1,5
    call cube%remove()
    call refine(x,1)
    call refine(y,1)
    call refine(z,1)
    call cube%create("Cube3D",x_axis=x,y_axis=y,z_axis=z)
    call cube%vtk("cube_"+str(i_i) )
enddo



end