use FEMDomainClass
implicit none

! Get Element By Coordinate
type(FEMDomain_) :: cube,element
integer(int32) :: i, ElementID
type(MPI_) :: mpid

call cube%create("Cube3D",x_num=100,y_num=100,z_num=100)

call cube%resize(x=10.0d0,y=10.0d0,z=10.0d0)

! Id of Element which contains a point x_i = (100, 1, 1).
ElementID = cube%getElementID(x=[1.1d0, 5.2d0, 8.3d0])
Element   = cube%getElement(ElementID)
print *, cube%getLocalCoordinate(ElementID=ElementID, x=1.1d0, y=5.20d0, z= 8.30d0)

call cube%vtk("cube")
call element%vtk("element")


end