module LsystemClass
    use PreprocessingClass


    type :: Leaf_
        type(FEMDomain_)    ::  FEMDomain
        real(8),allocatable ::  LeafSurfaceNode2D(:,:)
        real(8)             ::  Thickness,length,width,center(3)
        integer             ::  Division
        type(Stem_),pointer ::  pStem
        type(Peti_),pointer ::  pPeti
    contains
        procedure, public :: Init => initLeaf
    end type

    type :: Flower_
        type(FEMDomain_)    ::  FEMDomain
        real(8),allocatable ::  LeafSurfaceNode2D(:,:)
        real(8)             ::  Thickness,length,width,center(3)
        integer             ::  Division
        type(Stem_),pointer ::  pStem
    contains
        procedure, public :: Init => initFlower
    end type

    type :: Pod_
        type(FEMDomain_)    ::  FEMDomain
        real(8),allocatable ::  LeafSurfaceNode2D(:,:)
        real(8)             ::  Thickness,length,width,center(3)
        integer             ::  Division
        type(Stem_),pointer ::  pStem
    contains
        procedure, public :: Init => initPod
    end type

    type :: Stem_
        type(FEMDomain_)    ::  FEMDomain
        real(8)             ::  Thickness,length,width
        real(8)             ::  center_bottom(3),center_top(3)
        real(8)             ::  radius_bottom(3),radius_top(3)
        real(8)             ::  outer_normal_bottom(3),outer_normal_top(3)
        integer             ::  Division
        type(Stem_),pointer ::  pStem
    contains
        procedure, public :: Init => initStem
    end type

    type :: Peti_
        type(FEMDomain_)    ::  FEMDomain
        real(8)             ::  Thickness,length,width
        real(8)             ::  center_bottom(3),center_top(3)
        real(8)             ::  radius_bottom(3),radius_top(3)
        real(8)             ::  outer_normal_bottom(3),outer_normal_top(3)
        integer             ::  Division
        type(Stem_),pointer ::  pStem
    contains
        procedure, public :: Init => initPeti
    end type


    type :: Root_
        type(FEMDomain_)    ::  FEMDomain
        real(8)             ::  Thickness,length,width
        real(8)             ::  center_bottom(3),center_top(3)
        real(8)             ::  radius_bottom(3),radius_top(3)
        real(8)             ::  outer_normal_bottom(3),outer_normal_top(3)
        integer             ::  Division
        type(Stem_),pointer ::  pStem
        type(Root_),pointer ::  pRoot
    contains
        procedure, public :: Init => initRoot
    end type
    
    type :: Node_
        character(200)  :: crop_name
        logical         :: Reproductive
        type(Leaf_)     ,allocatable :: Leaf(:) 
        type(Peti_)     ,allocatable :: Peti(:)
        type(Flower_)   ,allocatable :: Flower(:)
        type(Pod_)      ,allocatable :: Pod(:)
        type(Stem_)     ,allocatable :: Stem(:)
        type(Root_)     ,allocatable :: Root(:)
    end Type

    type :: NodeSystem_
        type(Node_),allocatable :: NodeSystem(:)
    end type

    type :: RootSystem_
        type(Root_),allocatable :: Root(:)
    end type

    type :: soybean_
        ! growth_habit = determinate, indeterminate, semi-indeterminate, or vine
        character*20 :: growth_habit
        character*2  :: growth_stage
        integer :: NumOfNode
        integer :: NumOfRoot
        type(Node_),allocatable :: NodeSystem(:)
        type(Root_),allocatable :: RootSystem(:)
    contains
        procedure,public :: Init => initsoybean
        procedure,public :: AddNode => AddNodeSoybean
    end type

    type :: Canopy
        real(8) :: inter_row, intra_row
        type(soybean_),allocatable :: Canopy(:,:)
        
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
subroutine initLeaf(obj,Thickness,length,width)
    class(leaf_),intent(inout) :: obj
    real(8),optional :: Thickness,length,width

    if(present(length) .and. present(width) )then
        obj%length  = length
        obj%width   = width
        if(present(Thickness) )then
            obj%Thickness=Thickness
        endif
        return
    endif

    print *, "Caution :: no input is in initleaf"

end subroutine 
! ########################################


! ########################################
subroutine initPeti(obj,Thickness,length,width)
    class(Peti_),intent(inout) :: obj
    real(8),optional :: Thickness,length,width

    if(present(length) .and. present(width) )then
        obj%length  = length
        obj%width   = width
        if(present(Thickness) )then
            obj%Thickness=Thickness
        endif
        return
    endif

    print *, "Caution :: no input is in initleaf"

end subroutine 
! ########################################


! ########################################
subroutine initStem(obj,Thickness,length,width)
    class(Stem_),intent(inout) :: obj
    real(8),optional :: Thickness,length,width

    if(present(length) .and. present(width) )then
        obj%length  = length
        obj%width   = width
        if(present(Thickness) )then
            obj%Thickness=Thickness
        endif
        return
    endif

    print *, "Caution :: no input is in initleaf"

end subroutine 
! ########################################


! ########################################
subroutine initflower(obj,Thickness,length,width)
    class(flower_),intent(inout) :: obj
    real(8),optional :: Thickness,length,width

    if(present(length) .and. present(width) )then
        obj%length  = length
        obj%width   = width
        if(present(Thickness) )then
            obj%Thickness=Thickness
        endif
        return
    endif

    print *, "Caution :: no input is in initleaf"

end subroutine 
! ########################################


! ########################################
subroutine initPod(obj,Thickness,length,width)
    class(Pod_),intent(inout) :: obj
    real(8),optional :: Thickness,length,width

    if(present(length) .and. present(width) )then
        obj%length  = length
        obj%width   = width
        if(present(Thickness) )then
            obj%Thickness=Thickness
        endif
        return
    endif

    print *, "Caution :: no input is in initleaf"

end subroutine 
! ########################################


! ########################################
subroutine initRoot(obj,Thickness,length,width)
    class(Root_),intent(inout) :: obj
    real(8),optional :: Thickness,length,width

    if(present(length) .and. present(width) )then
        obj%length  = length
        obj%width   = width
        if(present(Thickness) )then
            obj%Thickness=Thickness
        endif
        return
    endif

    print *, "Caution :: no input is in initleaf"

end subroutine 
! ########################################


! ########################################
subroutine initsoybean(obj,growth_habit,Max_Num_of_Node)
    class(soybean_) :: obj
    character(*),optional,intent(in) :: growth_habit
    integer,optional,intent(in)::Max_Num_of_Node
    integer ::n

    if(present(growth_habit) )then
        obj%growth_habit=growth_habit
    else
        obj%growth_habit="determinate"
    endif

    obj%growth_stage="CV"

    n=input(default=100,option=Max_Num_of_Node)

    allocate(obj%NodeSystem(n))
    obj%NumOfNode=0
    obj%NumOfRoot=0

    ! set an initial node and root
    ! two leaves, one root.

    call obj%AddNode()

end subroutine
! ########################################



! ########################################
subroutine AddNodeSoybean(obj,SizeRatio)
    class(soybean_),intent(inout)::obj
    real(8),optional,intent(in)::SizeRatio
    real(8) :: magnif

    magnif=input(default=1.0d0,option=SizeRatio)
    obj%NumOfNode=obj%NumOfNode+1
    
    ! add leaves
    if(obj%NumOfNode==1 .or. obj%NumOfNode==2)then
        allocate(obj%NodeSystem(obj%NumOfNode)%leaf(2) )
        call obj%NodeSystem(obj%NumOfNode)%leaf(1)%init(thickness=0.10d0*magnif,length=3.0d0*magnif,width=2.0d0*magnif)
        call obj%NodeSystem(obj%NumOfNode)%leaf(1)%init(thickness=0.10d0*magnif,length=3.0d0*magnif,width=2.0d0*magnif)
    else        
        allocate(obj%NodeSystem(obj%NumOfNode)%leaf(3) )
        call obj%NodeSystem(obj%NumOfNode)%leaf(1)%init(thickness=0.10d0*magnif,length=4.0d0*magnif,width=2.0d0*magnif)
        call obj%NodeSystem(obj%NumOfNode)%leaf(1)%init(thickness=0.10d0*magnif,length=4.0d0*magnif,width=2.0d0*magnif)
        call obj%NodeSystem(obj%NumOfNode)%leaf(1)%init(thickness=0.10d0*magnif,length=4.0d0*magnif,width=2.0d0*magnif)
    endif

    ! add stem
    if(obj%NumOfNode==1 .or. obj%NumOfNode==2)then
        allocate(obj%NodeSystem(obj%NumOfNode)%Stem(1) )
        call obj%NodeSystem(obj%NumOfNode)%leaf(1)%init(thickness=0.10d0*magnif,length=3.0d0*magnif,width=2.0d0*magnif)
    endif


    ! add Peti
    if(obj%NumOfNode==1 .or. obj%NumOfNode==2)then
        allocate(obj%NodeSystem(obj%NumOfNode)%Peti(1) )
        call obj%NodeSystem(obj%NumOfNode)%Peti(1)%init(thickness=0.10d0*magnif,length=3.0d0*magnif,width=2.0d0*magnif)
    endif

end subroutine
! ########################################



! ########################################
subroutine InitLsystem(obj,InObj,MaxSize)
    class(Lsystem_),intent(inout)::obj
    type(Lsystem_),optional,intent(in) ::InObj
    integer,optional,intent(in)::MaxSize
    integer :: n

    !if(present(InObj) )then
    !    ! copy object
    !    obj=InObj
    !    return
    !endif

    n=input(default=100,option=MaxSize)
    allocate( obj%LeafList(n) )
    allocate( obj%PetiList(n) )
    allocate( obj%FlowerList(n) )
    allocate( obj%StemList(n) )
    allocate( obj%RootList(n) )

end subroutine
! ########################################





end module LsystemClass