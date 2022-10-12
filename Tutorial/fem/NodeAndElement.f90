use FEMDomainClass

type(FEMDomain_) :: domain
real(real64) :: x=100.0d0,y=100.0d0,z=100.0d0

call domain%create(meshtype="Cube3D",x_num=2,y_num=2,z_num=2)
call domain%resize(x=1.0d0,y=1.0d0,z=1.0d0)
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

end