module LeafClass
    use, intrinsic :: iso_fortran_env
    use KinematicClass
    use FEMDomainClass
    use StemClass
    use PetiClass
    implicit none


    type :: Leaf_
        type(FEMDomain_)    ::  FEMDomain
        real(real64),allocatable ::  LeafSurfaceNode2D(:,:)
        real(real64)             ::  ShapeFactor,Thickness,length,width,center(3)
        real(real64)             ::  MaxThickness,Maxlength,Maxwidth
        real(real64)             ::  center_bottom(3),center_top(3)
        real(real64)             ::  outer_normal_bottom(3),outer_normal_top(3)
        integer(int32)             ::  Division
        type(Stem_),pointer ::  pStem
        type(Peti_),pointer ::  pPeti
    contains
        procedure, public :: Init => initLeaf
    end type
contains

! ########################################
    subroutine initLeaf(obj,Thickness,length,width,ShapeFactor,MaxThickness,Maxlength,Maxwidth,rotx,roty,rotz,location)
        class(leaf_),intent(inout) :: obj
        real(real64),optional,intent(in) :: Thickness,length,width,ShapeFactor
        real(real64),optional,intent(in) :: MaxThickness,Maxlength,Maxwidth
        real(real64),optional,intent(in)::  rotx,roty,rotz,location(3)
        real(real64) :: loc(3)
        loc(:)=0.0d0
        if(present(location) )then
            loc(:)=location(:)
        endif
        obj%ShapeFactor = input(default=0.30d0  ,option= ShapeFactor  ) 
        obj%Thickness   = input(default=0.10d0,option= Thickness     )
        obj%length      = input(default=0.10d0,option= length      )
        obj%width       = input(default=0.10d0,option= width)
    
        obj%MaxThickness   = input(default=0.10d0  ,option= MaxThickness     )
        obj%Maxlength      = input(default=10.0d0  ,option= Maxlength      )
        obj%Maxwidth       = input(default=2.0d0   ,option= Maxwidth)
    
        obj%outer_normal_bottom(:)=0.0d0
        obj%outer_normal_bottom(1)=1.0d0
    
        obj%outer_normal_top(:)=0.0d0
        obj%outer_normal_top(1)=1.0d0
    
        ! rotate
        obj%outer_normal_Bottom(:) = Rotation3D(vector=obj%outer_normal_bottom,rotx=rotx,roty=roty,rotz=rotz)
        obj%outer_normal_top(:) = Rotation3D(vector=obj%outer_normal_top,rotx=rotx,roty=roty,rotz=rotz)
    
        obj%center_bottom(:)=loc(:)
        obj%center_top(:) = obj%center_bottom(:) + obj%length*obj%outer_normal_bottom(:)
        
    
    end subroutine 
    ! ########################################
    
    
end module 