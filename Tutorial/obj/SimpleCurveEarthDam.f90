use CivilItemClass
implicit none

type(FEMDomain_) :: dam
type(CivilItem_) :: ci

! simple curved Earth dam

dam = ci%EarthDam(&
    height=2.20d0, &
    width =11.0d0, &
    length=117.0d0,&
    depth =30.0d0,&
    margin=50.0d0,&
    angles=[30.0d0, 70.0d0],& ! 20.0 degrees & 15.0d0 degrees
    top_width=4.0d0, & ! top width
    refine_level = [5,7,3] ,& ! refinement level
    depth_cut    = 10, &
    margin_cut   = 7, &
    R = 1000.0d0 & ! optional :: curvature radius
    )

call dam%vtk("dam")

end