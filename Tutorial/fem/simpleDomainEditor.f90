use FEMDomainClass
implicit none

type(FEMDomain_) :: cube
real(real64),allocatable :: x_axis(:),y_axis(:),z_axis(:)

! modeling/editing object
x_axis = [0.0d0,1.0d0,2.0d0,5.0d0]
y_axis = [0.0d0,0.10d0,0.30d0,1.0d0]
z_axis = [0.0d0,0.10d0,0.40d0,1.0d0]

call refine(x_axis,10)
call refine(y_axis,10)
call refine(z_axis,10)

call cube%create("Cube3D",&
    x_axis=x_axis,&
    y_axis=y_axis,&
    z_axis=z_axis &
    )

! two alternative way to remove elements
call cube%removeElement(x_min=1.0d0,x_max=2.0d0,y_max=0.10d0,z_min=0.10d0)
call cube%removeElement(xr=[2.0d0,5.0d0],yr=[0.10d0,0.30d0])

! check info
print *, "x-range", cube%xr()
print *, "y-range", cube%yr()
print *, "z-range", cube%zr()
print *, "number of nodal points", cube%nn()
print *, "number of elements    ", cube%ne()

call cube%vtk("object")


end