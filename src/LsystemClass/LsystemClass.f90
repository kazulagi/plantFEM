module LsystemClass
    use PreprocessingClass


    type :: Leaf_
        type(FEMDomain_)    ::  FEMDomain
        real(8),allocatable ::  LeafSurfaceNode2D(:,:)
        real(8)             ::  ShapeFactor,Thickness,length,width,center(3)
        real(8)             ::  MaxThickness,Maxlength,Maxwidth
        real(8)             ::  center_bottom(3),center_top(3)
        real(8)             ::  outer_normal_bottom(3),outer_normal_top(3)
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
        real(8)             ::  MaxThickness,Maxlength,Maxwidth
        real(8)             ::  center_bottom(3),center_top(3)
        real(8)             ::  radius_bottom(3),radius_top(3)
        real(8)             ::  outer_normal_bottom(3),outer_normal_top(3)
        integer             ::  Division
        type(Stem_),pointer ::  pStem
    contains
        procedure, public :: Init => initStem
        procedure, public :: export => exportStem
    end type

    type :: Panicle
        type(FEMDomain_)    ::  FEMDomain
        real(8)             ::  Thickness,length,width
        real(8)             ::  center_bottom(3),center_top(3)
        real(8)             ::  radius_bottom(3),radius_top(3)
        real(8)             ::  outer_normal_bottom(3),outer_normal_top(3)
        integer             ::  Division
        type(Panicle),pointer ::  pPanicle
    contains
        !procedure, public :: Init => initPanicle
    end type
    type :: Peti_
        type(FEMDomain_)    ::  FEMDomain
        real(8)             ::  Thickness,length,width
        real(8)             ::  MaxThickness,Maxlength,Maxwidth
        real(8)             ::  center_bottom(3),center_top(3)
        real(8)             ::  radius_bottom(3),radius_top(3)
        real(8)             ::  outer_normal_bottom(3),outer_normal_top(3)
        integer             ::  Division
        type(Stem_),pointer ::  pStem
    contains
        procedure, public :: Init => initPeti
        procedure, public :: export => exportPeti
    end type


    type :: Root_
        type(FEMDomain_)    ::  FEMDomain
        real(8)             ::  Thickness,length,width
        real(8)             ::  MaxThickness,Maxlength,Maxwidth
        real(8)             ::  center_bottom(3),center_top(3)
        real(8)             ::  radius_bottom(3),radius_top(3)
        real(8)             ::  outer_normal_bottom(3),outer_normal_top(3)
        integer             ::  Division
        type(Stem_),pointer ::  pStem
        type(Root_),pointer ::  pRoot
    contains
        procedure, public :: Init => initRoot
        procedure, public :: export => exportRoot
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
    contains
        procedure :: init => initNode
        procedure :: export => exportNode
    end Type

    type :: NodeSystem_
        type(Node_),allocatable :: NodeSystem(:)
        integer :: num_of_node
    contains
        procedure :: export => exportNodeSystem
    end type

    type :: RootSystem_
        type(Root_),allocatable :: RootSystem(:)
        integer :: num_of_root
    contains
        procedure :: export => exportRootSystem
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
subroutine initLeaf(obj,Thickness,length,width,ShapeFactor,MaxThickness,Maxlength,Maxwidth)
    class(leaf_),intent(inout) :: obj
    real(8),optional,intent(in) :: Thickness,length,width,ShapeFactor
    real(8),optional,intent(in) :: MaxThickness,Maxlength,Maxwidth


    obj%ShapeFactor = input(default=0.30d0  ,option= ShapeFactor  ) 
    obj%Thickness   = input(default=0.0010d0,option= Thickness     )
    obj%length      = input(default=0.0010d0,option= length      )
    obj%width       = input(default=0.0010d0,option= width)

    obj%MaxThickness   = input(default=0.10d0  ,option= MaxThickness     )
    obj%Maxlength      = input(default=10.0d0  ,option= Maxlength      )
    obj%Maxwidth       = input(default=2.0d0   ,option= Maxwidth)


end subroutine 
! ########################################


! ########################################
subroutine initPeti(obj,Thickness,length,width,MaxThickness,Maxlength,Maxwidth)
    class(Peti_),intent(inout) :: obj
    real(8),optional,intent(in)::  Thickness,length,width
    real(8),optional,intent(in)::  MaxThickness,Maxlength,Maxwidth

    obj%Thickness   = input(default=0.0010d0,option= Thickness     )
    obj%length      = input(default=0.0010d0,option= length      )
    obj%width       = input(default=0.0010d0,option= width)

    obj%MaxThickness   = input(default=0.50d0  ,option= MaxThickness     )
    obj%Maxlength      = input(default=10.0d0  ,option= Maxlength      )
    obj%Maxwidth       = input(default=0.50d0  ,option= Maxwidth)

end subroutine 
! ########################################


! ########################################
subroutine initStem(obj,Thickness,length,width,MaxThickness,Maxlength,Maxwidth)
    class(Stem_),intent(inout) :: obj
    real(8),optional,intent(in)::  Thickness,length,width
    real(8),optional,intent(in)::  MaxThickness,Maxlength,MaxWidth

    obj%Thickness   = input(default=0.0010d0,option= Thickness     )
    obj%length      = input(default=0.2500d0,option= length      )
    obj%width       = input(default=0.0010d0,option= width)

    obj%MaxThickness   = input(default=0.50d0  ,option=MaxThickness      )
    obj%Maxlength      = input(default=10.0d0  ,option=Maxlength       )
    obj%Maxwidth       = input(default=0.50d0  ,option=Maxwidth )

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
subroutine InitLsystem(obj,InObj,MaxSize)
    class(Lsystem_),intent(inout)::obj
    type(Lsystem_),optional,intent(in) ::InObj
    integer,optional,intent(in)::MaxSize
    integer :: n

    !if(prent(InObj) )then
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

! ########################################
subroutine initNode(obj,PlantName,Stage,&
    LeafThickness,Leaflength,Leafwidth,LeafShapeFactor,&
    MaxLeafThickness,MaxLeaflength,MaxLeafwidth,PetiThickness,Petilength,Petiwidth,PetiShapeFactor,&
    MaxPetiThickness,MaxPetilength,MaxPetiwidth,StemThickness,Stemlength,Stemwidth,StemShapeFactor,&
    MaxStemThickness,MaxStemlength,MaxStemwidth)
    class(Node_),intent(inout),target::obj
    character(*),intent(in) :: PlantName
    character(2),intent(in) :: Stage
    real(8),optional,intent(in) :: LeafThickness,Leaflength,Leafwidth,LeafShapeFactor
    real(8),optional,intent(in) :: MaxLeafThickness,MaxLeaflength,MaxLeafwidth
    real(8),optional,intent(in) :: PetiThickness,Petilength,Petiwidth,PetiShapeFactor
    real(8),optional,intent(in) :: MaxPetiThickness,MaxPetilength,MaxPetiwidth
    real(8),optional,intent(in) :: StemThickness,Stemlength,Stemwidth,StemShapeFactor
    real(8),optional,intent(in) :: MaxStemThickness,MaxStemlength,MaxStemwidth

    if(trim(PlantName) == "soybean" .or. trim(PlantName) == "Soybean")then
        if(Stage == "EV")then
            allocate(obj%Leaf(2) )
            allocate(obj%Peti(2) )
            allocate(obj%Flower(0) )
            allocate(obj%Pod(0) )
            allocate(obj%Stem(1) )
            allocate(obj%Root(0) )
            

            ! initialize leaf
            call obj%Leaf(1)%init(ShapeFactor=LeafShapeFactor,Thickness=LeafThickness,length=Leaflength,&
            width=Leafwidth,MaxThickness=MaxLeafThickness,Maxlength=MaxLeaflength,MaxWidth=MaxLeafWidth)
            call obj%Leaf(2)%init(ShapeFactor=LeafShapeFactor,Thickness=LeafThickness,length=Leaflength,&
            width=Leafwidth,MaxThickness=MaxLeafThickness,Maxlength=MaxLeaflength,MaxWidth=MaxLeafWidth)
            ! initialize peti
            call obj%Peti(1)%init(Thickness=PetiThickness,length=Petilength,&
            width=Petiwidth,MaxThickness=MaxPetiThickness,Maxlength=MaxPetilength,MaxWidth=MaxPetiWidth)
            call obj%Peti(2)%init(Thickness=PetiThickness,length=Petilength,&
            width=Petiwidth,MaxThickness=MaxPetiThickness,Maxlength=MaxPetilength,MaxWidth=MaxPetiWidth)
            ! initialize stem
            call obj%Stem(1)%init(Thickness=StemThickness,length=Stemlength,&
            width=Stemwidth,MaxThickness=MaxStemThickness,Maxlength=MaxStemlength,MaxWidth=MaxStemWidth)
            
            ! joint leaves
            obj%Leaf(1)%pPeti => obj%Peti(1)
            obj%Leaf(2)%pPeti => obj%Peti(2)
            
            ! joint peti
            obj%Peti(1)%pStem => obj%Stem(1)
            obj%Peti(2)%pStem => obj%Stem(1)

            ! set direction of plumule
            obj%Stem(1)%outer_normal_bottom(:)=0.0d0
            obj%Stem(1)%outer_normal_bottom(1)=1.0d0
            
            obj%Stem(1)%outer_normal_top(:)=0.0d0
            obj%Stem(1)%outer_normal_top(1)=1.0d0
            
            obj%Stem(1)%center_bottom(:)=0.0d0
            obj%Stem(1)%center_top(:)=0.0d0
            obj%Stem(1)%center_top(:) = obj%Stem(1)%center_bottom(:) + obj%Stem(1)%length*obj%Stem(1)%outer_normal_bottom(:)

            return
        else
            return
        endif
    endif
end subroutine
! ########################################


! ########################################
subroutine initRoot(obj,PlantName,Stage,&
    Thickness,length,width,&
    MaxThickness,Maxlength,Maxwidth)
    class(Root_),intent(inout)::obj
    character(*),intent(in) :: PlantName
    character(2),intent(in) :: Stage
    real(8),optional,intent(in) :: Thickness,length,width
    real(8),optional,intent(in) :: MaxThickness,Maxlength,Maxwidth

    if(trim(PlantName) == "soybean" .or. trim(PlantName) == "Soybean")then
        if(Stage == "EV")then
            

            ! initialize 
            obj%Thickness   = input(default=0.0010d0,option= Thickness     )
            obj%length      = input(default=0.250d0,  option= length      )
            obj%width       = input(default=0.0010d0,option= width)
        
            obj%MaxThickness   = input(default=0.50d0  ,option=MaxThickness      )
            obj%Maxlength      = input(default=10.0d0  ,option=Maxlength       )
            obj%Maxwidth       = input(default=0.50d0  ,option=Maxwidth )
        
        
            ! set direction of plumule
            obj%outer_normal_bottom(:)=0.0d0
            obj%outer_normal_bottom(1)=-1.0d0
            
            obj%outer_normal_top(:)=0.0d0
            obj%outer_normal_top(1)=-1.0d0
            
            obj%center_bottom(:)=0.0d0
            obj%center_top(:) = obj%center_bottom(:) + obj%length*obj%outer_normal_bottom(:)

            return
        else
            return
        endif
    endif
end subroutine
! ########################################




! ########################################
subroutine exportRoot(obj,FileName,RootID)
    class(Root_),intent(in)::obj
    character(*),intent(in) :: FileName
    integer,optional,intent(in) :: RootID
    real(8) :: radius    
    radius=0.50d0*obj%width+0.50d0*obj%Thickness
    open(12,file=FileName)
    write(12,'(A)') "//+"
    write(12,'(A)') 'SetFactory("OpenCASCADE");'
    write(12,*) "Cylinder(",input(default=1,option=RootID),") = {",&
    obj%center_bottom(1),",", obj%center_bottom(2),",", obj%center_bottom(3),",",&
    obj%outer_normal_bottom(1),",", obj%outer_normal_bottom(2),",", obj%outer_normal_bottom(3),",",&
    radius,", 2*Pi};"
    close(12)

end subroutine
! ########################################


! ########################################
subroutine exportStem(obj,FileName,StemID)
    class(Stem_),intent(in)::obj
    character(*),intent(in) :: FileName
    integer,optional,intent(in) :: StemID
    real(8) :: radius

    radius=0.50d0*obj%width+0.50d0*obj%Thickness
    
    open(13,file=FileName)
    write(13,'(A)') "//+"
    write(13,'(A)') 'SetFactory("OpenCASCADE");'
    write(13,*) "Cylinder(",input(default=1,option=StemID),") = {",&
    obj%center_bottom(1),",", obj%center_bottom(2),",", obj%center_bottom(3),",",&
    obj%outer_normal_bottom(1),",", obj%outer_normal_bottom(2),",", obj%outer_normal_bottom(3),",",&
    radius,", 2*Pi};"
    close(13)

end subroutine
! ########################################


! ########################################
subroutine exportPeti(obj,FileName,PetiID)
    class(Peti_),intent(in)::obj
    character(*),intent(in) :: FileName
    integer,optional,intent(in) :: PetiID
    real(8) :: radius
    
    radius=0.50d0*obj%width+0.50d0*obj%Thickness
    open(14,file=FileName)
    write(14,'(A)') "//+"
    write(14,'(A)') 'SetFactory("OpenCASCADE");'
    write(14,*) "Cylinder(",input(default=1,option=PetiID),") = {",&
    obj%center_bottom(1),",", obj%center_bottom(2),",", obj%center_bottom(3),",",&
    obj%outer_normal_bottom(1),",", obj%outer_normal_bottom(2),",", obj%outer_normal_bottom(3),",",&
    radius,", 2*Pi};"
    close(14)

end subroutine
! ########################################




! ########################################
subroutine exportNode(obj,FileName,NodeID)
    class(Node_),intent(in) :: obj
    character(*),optional,intent(in) :: FileName
    integer :: i,max_num_of_peti_per_node,n
    integer,intent(in) :: NodeID

    max_num_of_peti_per_node=5

    do i=1,size(obj%Stem,1)
        call obj%Stem(i)%export(FileName= "Stem_"//trim(adjustl(fstring(i)))//FileName,StemID=i)
    enddo

    do i=1,size(obj%Peti,1)
        n=max_num_of_peti_per_node*(NodeID-1)+i
        call obj%Peti(i)%export(FileName="Peti_"//trim(adjustl(fstring(i)))//FileName,PetiID=n)
    enddo
   
end subroutine
! ########################################


! ########################################
subroutine exportNodeSystem(obj,FilePath,FileName)
    class(NodeSystem_),intent(in) :: obj

    ! export stem
    character(*),optional,intent(in) :: FilePath,FileName
    integer :: i
    do i=1,size(obj%NodeSystem)
        if(present(FileName) )then
            call obj%NodeSystem(i)%export(FileName="Node_"//trim(FileName),NodeID=i)
        elseif(present(FilePath) )then
            call obj%NodeSystem(i)%export(FileName=trim(FilePath)//"/Node.geo",NodeID=i)
        else
            call obj%NodeSystem(i)%export(FileName="/Node.geo",NodeID=i)
        endif
    enddo
    


end subroutine
! ########################################

! ########################################
subroutine exportRootSystem(obj,FilePath,FileName)
    class(RootSystem_),intent(in) :: obj
    character(*),optional,intent(in) :: FilePath,FileName
    integer :: i
    do i=1,size(obj%RootSystem)

        if(present(FileName) )then
            call obj%RootSystem(i)%export(FileName="Root_"//trim(FileName),RootID=i)
        elseif(present(FilePath) )then
            call obj%RootSystem(i)%export(FileName=trim(FilePath)//"/Root.geo",RootID=i)
        else
            call obj%RootSystem(i)%export(FileName="/Root.geo",RootID=i)
        endif
        if(i==obj%num_of_root  )then
            return
        endif
    enddo
end subroutine
! ########################################

end module LsystemClass