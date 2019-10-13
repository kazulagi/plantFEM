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
        type(Stem_),pointer ::  Parent
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
        type(Root_)         ::  Parent
    end type
    
    type :: Node_
        character(200)  :: crop_name
        logical         :: Reproductive
        type(Leaf_)     ,pointer,allocatable :: Leaf(:) 
        type(Peti_)     ,pointer,allocatable :: Peti(:)
        type(Flower_)   ,pointer,allocatable :: Flower(:)
        type(Pod_)      ,pointer,allocatable :: Pod(:)
        type(Stem_)     ,pointer,allocatable :: Stem(:)
        type(Root_)     ,pointer,allocatable :: Root(:)
    end Type

    type :: NodeSystem_
        type(Node_),pointer,allocatable :: NodeSystem(:)
    end type

    type :: RootSystem_
        type(Root_),pointer,allocatable :: Root(:)
    end type

    type :: soybean_
        ! growth_habit = determinate, indeterminate, semi-indeterminate, or vine
        character*20 :: growth_habit
        character*2  :: growth_stage
        type(NodeSystem_) :: NodeSystem
        type(RootSystem_) :: RootSystem
    contains
        procedure,public :: Init => initsoybean
    end type

    type :: Canopy
        real(8) :: inter_row, intra_row
        type(soybean_),pointer,allocatable :: Canopy(:,:)
        
    end type

    type :: Lsystem_
        type(Leaf_),pointer,allocatable::LeafList(:)
        type(Peti_),pointer,allocatable::PetiList(:)
        type(Flower_),pointer,allocatable::FlowerList(:)
        type(Stem_),pointer,allocatable::StemList(:)
        type(Root_),pointer,allocatable::RootList(:)
    contains
        procedure,public :: Init => InitLsystem 
    end type

contains

! ########################################
subroutine initsoybean(obj,growth_habit)
    class(soybean_) :: obj
    character*,optional,intent(in) :: growth_habit

    if(present(growth_habit) )then
        obj%growth_habit=growth_habit
    else
        obj%growth_habit="determinate"
    endif

    obj%growth_stage="CV"

end subroutine
! ########################################



! ########################################
subroutine InitLsystem(obj,InObj,MaxSize)
    class(Lsystem_),intent(inout)::obj
    type(Lsystem_),optional,intent(in) ::InObj
    integer,optional,intent(in)::MaxSize
    integer :: n

    if(present(Import) )then
        ! copy object
        obj=InObj
        return
    endif

    n=input(default=100,option=MaxSize)
    allocate( obj%LeafList(n) )
    allocate( obj%PetiList(n) )
    allocate( obj%FlowerList(n) )
    allocate( obj%StemList(n) )
    allocate( obj%RootList(n) )

end subroutine
! ########################################


! ########################################
subroutine Add(obj,Type,id)
    class(Lsystem_),intent(in)::obj
    character(*),intent(in)::Type
    integer,intent(in) :: id

    if(Type=="Stem")then
        
    endif

end subroutine
! ########################################



end module LsystemClass