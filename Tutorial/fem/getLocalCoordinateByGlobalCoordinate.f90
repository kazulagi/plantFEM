use FEMDomainClass
implicit none

! Get Element/Local coordinate By Coordinate
type(FEMDomain_) :: cube,element
integer(int32) :: i, ElementID
type(MPI_) :: mpid

call cube%create("Cube3D",x_num=100,y_num=100,z_num=100)

call cube%resize(x=10.0d0,y=10.0d0,z=10.0d0)

! Id of Element which contains a point x_i = (100, 1, 1).
ElementID = cube%getElementID(x=[1.01d0, 5.05d0, 8.08d0])
Element   = cube%getElement(ElementID)
print *, cube%getLocalCoordinate(ElementID=ElementID, x=1.01d0, y=5.050d0, z= 8.080d0)

call cube%vtk("cube")
call element%vtk("element")


end