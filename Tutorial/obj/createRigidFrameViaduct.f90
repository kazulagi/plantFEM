use FEMDomainClass
use CivilItemClass
implicit none

type(CivilItem_) :: ci
type(FEMdomain_) :: rigidFrames, ground

call ground%create("Cube3D")
call ground%resize(x=400.0d0,y=400.0d0)
call ground%move(x=-200.0d0,y=-200.0d0,z=-ground%zmax() )
call ground%vtk("ground")

! create RigidFrameViaduct
rigidFrames = ci%RigidFrameViaduct(&
        NumPiers = [2, 10]  ,&! 2 x 10 piers
        length = 200.0d0 , & ! 200 m
        width =  10.0d0 ,&
        height = 20.0d0  ,&
        PierThickness = 2.0d0  ,&
        divisions = [8,8,8] ,&
        MiddlePierHeights = [10.0d0]  &
    )

call rigidFrames%vtk("RigidFrames_2")

end