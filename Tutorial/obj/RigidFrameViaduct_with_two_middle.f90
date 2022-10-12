use FEMDomainClass
use CivilItemClass
implicit none

type(CivilItem_) :: ci
type(FEMdomain_) :: rigidFrames, ground

call ground%create("Cube3D",x_num=30,y_num=30,z_num=30)
call ground%resize(x=400.0d0,y=400.0d0,z=400.0d0)
call ground%move(x=-100.0d0,y=-100.0d0,z=-ground%zmax() )
call ground%vtk("ground")

! create RigidFrameViaduct
rigidFrames = ci%RigidFrameViaduct(&
        NumPiers = [2, 5]  ,&! 2 x 10 piers
        width =  14.0d0 ,&
        length = 50.0d0 , & ! 200 m
        height = 20.0d0  ,&
        PierThickness =2.0d0  ,&
        divisions = [14,50,20]*2 ,&
        MiddlePierHeights = [7.0d0,11.0d0]  &
    )

call rigidFrames%vtk("RigidFrames_2")

end