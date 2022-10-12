program main
    use CivilItemClass
    implicit none

    type(FEMDomain_),target :: ramen
    type(CivilItem_) :: ci

    ! create RigidFrameViaduct
    ! full-set argument
    ramen = ci%RigidFrameViaduct(&
        NumPiers = [2,4]  ,&! 2 x 4 piers
        width =  10.0d0 ,&
        length = 35.00d0 , & ! 
        height = 12.00d0  ,&
        PierThickness = 1.00d0 ,&
        GirderThickness = 0.30d0, &
        MiddlePierHeights = [5.0d0], & ! optional
        divisions = [6,3,5], & ![17,7,7][22,7,5]
        GirderWidth = 11.000d0, & ! Done!
        GirderEdgeHeight  = 1.00d0, &   ! Girder Edge
        GirderEdgeThickness  = 0.2500d0 &
    )

    call ramen%vtk("test")

end