program main
    use CivilItemClass
    implicit none

    type(FEMDomain_),target :: ramen
    type(CivilItem_) :: ci

    ! create RigidFrameViaduct
    ! full-set argument
    ramen = ci%RigidFrameViaduct(&
        NumPiers = [2,4]  ,&! 2 x 4 piers
        width =  9.000d0 - 2.0d0*1.350d0 ,&
        length = 30.00d0 , & ! 
        height = 11.900d0  ,&
        PierThickness = 0.900d0 ,&
        GirderThickness = 0.30d0, &
        MiddlePierHeights = [5.0d0], &
        divisions = [6,3,5], & ![17,7,7][22,7,5]
        GirderWidth = 11.300d0, & ! Done!
        GirderEdgeHeight  = 1.2050d0, &   ! 追加
        GirderEdgeThickness  = 0.200d0, & ! 追加
        JointHeight    = 9.90d0, & ! 追加
        JointThickness = 0.903d0,& ! 追加
        JointLength    = 0.750d0 & ! 追加
    )

    call ramen%vtk("test")

end