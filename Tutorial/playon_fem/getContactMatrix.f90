use FEMDomainClass

type(FEMDomain_) :: domain, domain2
type(ShapeFunction_) :: sf
type(IO_) :: f
real(real64) :: position(3)

call f%open("ContactMatrix.txt")

call domain%create(meshtype="Cube3D",x_num=2,y_num=2,z_num=2)
call domain%resize(x=2.0d0,y=2.0d0,z=2.0d0)
call domain%msh("domain")


call domain2%create(meshtype="Cube3D",x_num=2,y_num=2,z_num=2)
call domain2%resize(x=2.0d0,y=2.0d0,z=2.0d0)
call domain2%move(x=1.9d0,z=0.10d0)
call domain2%msh("domain2")

! set global coordinate
position(:) = domain2%mesh%nodcoord(7,:)


! Get the nearest Element to (x,y,z) = (0,0,1)
print *, "domain2, Node ID : ", 7
print *, "domain1, ELEMENT ID: ", domain%mesh%nearestElementID(x=position(1),y=position(2),z=position(3) )
print *, "domain1, Node ID: ", domain%connectivity(domain%mesh%nearestElementID(x=position(1),y=position(2),z=position(3) ))
call f%write( "ELEMENT ID: "//str(domain%mesh%nearestElementID(x=position(1),y=position(2),z=position(3) )))


! get shape-function
sf = domain%getShapeFunction(position=position)

call f%write(domain%connectMatrix(position,DOF=1) )
call f%write(domain%connectMatrix(position,DOF=1),sparse=.true. )
call f%close()

! show global coordinate
print *, "x",position(1)
print *, "y",position(2)
print *, "z",position(3)



end