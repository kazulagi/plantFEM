use FEMDomainClass
implicit none

type(FEMDomain_) :: Cube
real(real64),allocatable :: displacement(:),YoungModulus(:),PoissonRatio(:)
real(real64),allocatable :: traction(:)
integer(int32),allocatable :: nodeList(:)
real(real64) :: disp_x

disp_x = 0.50d0

call Cube%create("Cube3D",x_num=2, y_num=2,z_num=2)
call Cube%resize(x=2.0d0,y=2.0d0,z=2.0d0)

! select nodes by range
NodeList = Cube%getNodeList(xmax=0.0d0)

call Cube%vtk("cube")

Displacement = zeros(cube%nn()*cube%nd())
YoungModulus = zeros(cube%ne())
PoissonRatio = zeros(cube%ne())

Displacement(3*NodeList(:)-2) = disp_x
call print(reshape(Displacement,cube%nn(),cube%nd()))

YoungModulus = 1.0d0
PoissonRatio = 0.00d0
Traction     = Cube%TractionVector(&
    displacement=displacement,&
    YoungModulus=YoungModulus,&
    PoissonRatio=PoissonRatio)

call print(reshape(Traction,cube%nn(),cube%nd()))
call print(sum(Traction(3*NodeList(:)-2 ) ) )
call Cube%move(x=disp_x,NodeList=NodeList)
call Cube%vtk("cube")
call Cube%vtk("SelectedNode",NodeList=Nodelist)


end