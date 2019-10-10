module LsystemClass
    use PreprocessingClass


    type :: Leaf_
        type(FEMDomain_)    ::  FEMDomain
        real(8),allocatable ::  LeafSurfaceNode2D(:,:)
        real(8)             ::  Thickness,length,width,center(3)
        integer             ::  Division
    end type

    type :: Flower_
        type(FEMDomain_)    ::  FEMDomain
        real(8),allocatable ::  LeafSurfaceNode2D(:,:)
        real(8)             ::  Thickness,length,width,center(3)
        integer             ::  Division
    end type

    type :: Pod_
        type(FEMDomain_)    ::  FEMDomain
        real(8),allocatable ::  LeafSurfaceNode2D(:,:)
        real(8)             ::  Thickness,length,width,center(3)
        integer             ::  Division
    end type

    type :: Stem_
        type(FEMDomain_)    ::  FEMDomain
        real(8)             ::  Thickness,length,width
        real(8)             ::  center_bottom(3),center_top(3)
        real(8)             ::  radius_bottom(3),radius_top(3)
        real(8)             ::  outer_normal_bottom(3),outer_normal_top(3)
        integer             ::  Division
    end type
    
    type :: Peti_
        type(FEMDomain_)    ::  FEMDomain
        real(8)             ::  Thickness,length,width
        real(8)             ::  center_bottom(3),center_top(3)
        real(8)             ::  radius_bottom(3),radius_top(3)
        real(8)             ::  outer_normal_bottom(3),outer_normal_top(3)
        integer             ::  Division
    end type


    type :: Root_
        type(FEMDomain_)    ::  FEMDomain
        real(8)             ::  Thickness,length,width
        real(8)             ::  center_bottom(3),center_top(3)
        real(8)             ::  radius_bottom(3),radius_top(3)
        real(8)             ::  outer_normal_bottom(3),outer_normal_top(3)
        integer             ::  Division
    end type

    

    type :: Lsystem_
        type(Leaf_),allocatable::LeafList(:)
        type(Peti_),allocatable::PetiList(:)
        type(Flower_),allocatable::FlowerList(:)
        type(Stem_),allocatable::StemList(:)
        type(Root_),allocatable::RootList(:)
    contains
        procedure,public :: Init => InitLsystem 
    end type

contains

! ########################################
subroutine InitLsystem(obj,InObj)
    class(Lsystem_),intent(inout)::obj
    type(Lsystem_),optional,intent(in) ::InObj

    if(present(Import) )then
        ! copy object
        obj=InObj
    endif
    
end subroutine
! ########################################



end module LsystemClass