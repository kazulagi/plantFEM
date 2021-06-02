use FEMDomainClass

type(FEMDomain_) :: domain

call domain%create(meshtype="Cube3D",x_num=2,y_num=2,z_num=2)
call domain%resize(x=100.0d0,y=100.0d0,z=100.0d0)
call domain%msh("domain")

! Get the nearest Element to (x,y,z) = (0,0,1)
print *, domain%mesh%nearestElementID(x=0.0d0,y=0.0d0,z=1.0d0)

! P(51,51,100) is in the 5th element?
print *, domain%mesh%InsideOfElement(ElementID=5,x=51.0d0,y=51.0d0,z=100.00d0)

end