use CivilItemClass
implicit none

type(FEMDomain_) :: dam
type(CivilItem_) :: ci

dam = ci%EarthDam(&
    height=2.20d0, &
    width =11.0d0, &
    length=117.0d0,&
    depth =30.0d0,&
    margin=50.0d0,&
    angles=[20.0d0, 20.0d0],& ! 20.0 degrees & 15.0d0 degrees
    top_width=4.0d0, & ! top width
    refine_level = [5,5,3] ,& ! refinement level
    depth_cut    = 10, &
    margin_cut   = 10 &
    )

call dam%vtk("dam")

end