module SoybeanClass
    use MathClass
    use SeedClass
    use LsystemClass
    implicit none
    
    type :: soybean_
        ! growth_habit = determinate, indeterminate, semi-indeterminate, or vine
        character*20 :: growth_habit
        character*2  :: growth_stage
        integer :: Num_Of_Node
        integer :: Num_Of_Root
        character(2) :: Stage ! VE, CV, V1,V2, ..., R1, R2, ..., R8
        type(Seed_) :: Seed
        type(Node_),allocatable :: NodeSystem(:)
        type(Root_),allocatable :: RootSystem(:)
    contains
        procedure,public :: Init => initsoybean
        procedure,public :: sowing => initsoybean
        procedure,public :: export => exportSoybean
        procedure,public :: grow => growSoybean
        !procedure,public :: AddNode => AddNodeSoybean
    end type

    type :: SoybeanCanopy_
        real(8) :: inter_row, intra_row
        type(soybean_),allocatable :: Canopy(:,:)
        
    end type


contains






! ########################################
subroutine initsoybean(obj,mass,water_content,radius,location,x,y,z,&
    root_diameter_per_seed_radius,max_node_num)
    class(Soybean_),intent(inout) :: obj
    real(8),optional,intent(in) :: mass,water_content,radius,location(3),x,y,z
    real(8),optional,intent(in) :: root_diameter_per_seed_radius
    integer,optional,intent(in) :: max_node_num
    real(8) :: MaxThickness,Maxwidth

    obj%Stage = "VE"

    ! initialize RootSystem and NodeSystem
    if(.not.allocated( obj%RootSystem) )then
        allocate(obj%RootSystem( input(default=1000,option=max_node_num) ) ) 
        obj%num_of_root=1
    endif
    if(.not.allocated( obj%NodeSystem) )then
        allocate(obj%NodeSystem( input(default=1000,option=max_node_num) ) ) 
        obj%num_of_node=1
    endif

    ! setup seed
    call obj%Seed%init(mass,water_content,radius,location,x,y,z)
    ! setup primary node (plumule)
    call obj%NodeSystem(1)%init(Stage=obj%Stage,Plantname="soybean",location=location)

    ! setup primary node (radicle))
    MaxThickness=input(default=0.20d0,option=root_diameter_per_seed_radius)*obj%Seed%radius
    Maxwidth    =input(default=0.20d0,option=root_diameter_per_seed_radius)*obj%Seed%radius
    call obj%RootSystem(1)%init(Plantname="soybean",Stage=obj%Stage,MaxThickness=MaxThickness,Maxwidth=Maxwidth,location=location)

end subroutine
! ########################################

! ########################################
subroutine growSoybean(obj,dt)
    class(Soybean_),intent(inout) :: obj
    real(8),intent(in) :: dt ! time-interval

    if(obj%Stage=="VE")then
        ! VE
        ! Seed => VE

        ! now ignore time-scale


        ! Update RootSystem
        !call obj%UpdateRootSystemVE()

        ! Update NodeSystem
        !call obj%UpdateNodeSystemVE()


    elseif(obj%Stage=="CV" )then
        ! CV stage
    elseif(obj%Stage(1:1)=="R")then
        ! Reproductive Stage
    else
        ! Vagetative
    endif

end subroutine
! ########################################



! ########################################
subroutine exportSoybean(obj,FilePath,FileName,SeedID)
    class(Soybean_),intent(inout) :: obj
    character(*),optional,intent(in) :: FilePath,FileName
    integer,optional,intent(inout) :: SeedID
    integer :: i,itr

    itr=SeedID
    ! if seed exists => output
    if(obj%Seed%num_of_seed>=0)then
        if(present(FileName) )then
            call obj%Seed%export(FileName=trim(FileName),SeedID=itr)
        elseif(present(FilePath) )then
            call obj%Seed%export(FileName=trim(FilePath)//"/seed.geo",SeedID=itr)
        else
            call obj%Seed%export(FileName="/seed.geo",SeedID=itr)
        endif
    endif

    itr=itr+1
    ! export NodeSystem
    do i=1,size(obj%NodeSystem)
        if(present(FileName) )then
            call obj%NodeSystem(i)%export(FileName=trim(FileName)//"_Node.geo",objID=itr)
        elseif(present(FilePath) )then
            call obj%NodeSystem(i)%export(FileName=trim(FilePath)//"/Node.geo",objID=itr)
        else
            call obj%NodeSystem(i)%export(FileName="./Node.geo",objID=itr)
        endif
        if(i==obj%num_of_node  )then
            exit
        endif
    enddo

    
    ! export RootSystem
    do i=1,size(obj%RootSystem)
        if(present(FileName) )then
            call obj%RootSystem(i)%export(FileName=trim(FileName)//"_Root.geo",RootID=itr)
        elseif(present(FilePath) )then
            call obj%RootSystem(i)%export(FileName=trim(FilePath)//"/Root.geo",RootID=itr)
        else
            call obj%RootSystem(i)%export(FileName="./Root.geo",RootID=i)
        endif
        if(i==obj%num_of_root  )then
            exit
        endif
    enddo
    SeedID=itr


end subroutine
! ########################################



! ########################################

! ########################################
!subroutine initsoybean(obj,growth_habit,Max_Num_of_Node)
!    class(soybean_) :: obj
!    character(*),optional,intent(in) :: growth_habit
!    integer,optional,intent(in)::Max_Num_of_Node
!    integer ::n
!
!    if(present(growth_habit) )then
!        obj%growth_habit=growth_habit
!    else
!        obj%growth_habit="determinate"
!    endif
!
!    obj%growth_stage="VE"
!
!    n=input(default=100,option=Max_Num_of_Node)
!
!    allocate(obj%NodeSystem(n))
!    obj%NumOfNode=0
!    obj%NumOfRoot=0
!
!    ! set an initial node and root
!    ! two leaves, one root.
!
!    call obj%AddNode()
!
!end subroutine
!! ########################################
!
!
!
!
!
!
!! ########################################
!subroutine AddNodeSoybean(obj,SizeRatio)
!    class(soybean_),intent(inout)::obj
!    real(8),optional,intent(in)::SizeRatio
!    real(8) :: magnif
!
!    magnif=input(default=1.0d0,option=SizeRatio)
!    obj%NumOfNode=obj%NumOfNode+1
!    
!    ! add leaves
!    if(obj%NumOfNode==1 .or. obj%NumOfNode==2)then
!        allocate(obj%NodeSystem(obj%NumOfNode)%leaf(2) )
!        call obj%NodeSystem(obj%NumOfNode)%leaf(1)%init(thickness=0.10d0*magnif,length=3.0d0*magnif,width=2.0d0*magnif)
!        call obj%NodeSystem(obj%NumOfNode)%leaf(1)%init(thickness=0.10d0*magnif,length=3.0d0*magnif,width=2.0d0*magnif)
!    else        
!        allocate(obj%NodeSystem(obj%NumOfNode)%leaf(3) )
!        call obj%NodeSystem(obj%NumOfNode)%leaf(1)%init(thickness=0.10d0*magnif,length=4.0d0*magnif,width=2.0d0*magnif)
!        call obj%NodeSystem(obj%NumOfNode)%leaf(1)%init(thickness=0.10d0*magnif,length=4.0d0*magnif,width=2.0d0*magnif)
!        call obj%NodeSystem(obj%NumOfNode)%leaf(1)%init(thickness=0.10d0*magnif,length=4.0d0*magnif,width=2.0d0*magnif)
!    endif
!
!    ! add stem
!    if(obj%NumOfNode==1 .or. obj%NumOfNode==2)then
!        allocate(obj%NodeSystem(obj%NumOfNode)%Stem(1) )
!        call obj%NodeSystem(obj%NumOfNode)%leaf(1)%init(thickness=0.10d0*magnif,length=3.0d0*magnif,width=2.0d0*magnif)
!    endif
!
!    ! add Peti
!    if(obj%NumOfNode==1 .or. obj%NumOfNode==2)then
!        allocate(obj%NodeSystem(obj%NumOfNode)%Peti(1) )
!        call obj%NodeSystem(obj%NumOfNode)%Peti(1)%init(thickness=0.10d0*magnif,length=3.0d0*magnif,width=2.0d0*magnif)
!    endif
!
!end subroutine
!! ########################################
!


end module