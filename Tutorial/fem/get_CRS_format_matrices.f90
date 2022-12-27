use FEMDomainClass
implicit none

type(FEMDomain_) :: cube(1)
type(CRS_) :: DiffusionMatrix,StiffnessMatrix,MassMatrix
type(Time_) :: time

integer(int32) :: ElementID

call cube(1)%create("Cube3D",x_num=100,y_num=100,z_num=100)
print *, "mesh is created."

! CRS-formatted 3-D DiffusionMatrix
call time%start()
DiffusionMatrix = cube(1)%DiffusionMatrix(coefficient=ones(cube(1)%nn()) )
call time%show()


! CRS-formatted 3-D MassMatrix
call time%start()
MassMatrix = cube(1)%MassMatrix(&
    Density=ones(cube(1)%nn()),&
    DOF=3)
call time%show()

! CRS-formatted 3-D StiffnessMatrix
call time%start()
StiffnessMatrix = cube(1)%StiffnessMatrix(&
    YoungModulus=ones(cube(1)%nn()),&
    PoissonRatio=0.30d0*ones(cube(1)%nn()) )
call time%show()




end