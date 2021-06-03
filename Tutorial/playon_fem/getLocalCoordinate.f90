use FEMDomainClass

type(FEMDomain_) :: domain
type(ShapeFunction_) :: sf
real(real64) :: x=0.0d0,y=0.0d0,z=0.0d0,position(3)

call domain%create(meshtype="Cube3D",x_num=2,y_num=2,z_num=2)
call domain%resize(x=2.0d0,y=2.0d0,z=2.0d0)
call domain%msh("domain")
call domain%move(x=x,y=y,z=z)

! Get the nearest Element to (x,y,z) = (0,0,1)
print *, domain%mesh%nearestElementID(x=0.0d0,y=0.0d0,z=1.0d0)

! P(51,51,100) is in the 5th element?
print *, domain%mesh%InsideOfElement(ElementID=5,x=0.0d0,y=0.0d0,z=1.0d0)

! get local coordinates
print *, domain%getLocalCoordinate(ElementID=1,x=0.20d0+x,y=0.20d0+y,z=0.000d0+z)
print *, domain%getLocalCoordinate(ElementID=1,x=0.00d0+x,y=0.00d0+y,z=0.000d0+z)
print *, domain%getLocalCoordinate(ElementID=1,x=0.300d0+x,y=0.300d0+y,z=0.5000d0+z)

! set global coordinate
position(1) = 1.500d0
position(2) = 0.00d0
position(3) = 0.00d0

! get shape-function
sf = domain%getShapeFunction(position=position)

! show global coordinate
print *, "x",position(1)
print *, "y",position(2)
print *, "z",position(3)

! show element-id
print *, "ELEMENT ID: ",sf%ElementID 
! show shape-function
print *, "Shape Function "
do i=1,size(sf%Nmat)
     print *, "N"//str(i), sf%Nmat(i) 
enddo

end