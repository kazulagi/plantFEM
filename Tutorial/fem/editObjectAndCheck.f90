use FEMDomainClass
implicit none

type(FEMDomain_) :: Cube
type(Random_) :: random
real(real64),allocatable :: displacement(:),YoungModulus(:),PoissonRatio(:)
real(real64),allocatable :: traction(:)
integer(int32),allocatable :: nodeList(:)

call Cube%create("Cube3D",x_num=2, y_num=2,z_num=2)
call Cube%resize(x=2.0d0,y=2.0d0,z=2.0d0)

! select nodes by range
nodeList = Cube%getNodeList(zmin=2.0d0)

! export cube and nodes
call Cube%vtk("cube")
call Cube%vtk("SelectedNode",NodeList=NodeList)
! You can check it in paraview

! Please comment out to run following commands
! edit cube
call Cube%move(x=0.10d0,NodeList=NodeList)
call Cube%move(y=0.10d0,NodeList=[2,3])
call Cube%move(z=0.10d0,NodeList=NodeList)

! and export again (Type F5)  
call Cube%vtk("cube")



end