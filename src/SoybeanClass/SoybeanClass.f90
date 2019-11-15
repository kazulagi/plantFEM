module SoybeanClass
    use MathClass
    use SeedClass
    use PlantNodeClass
    implicit none
    
    type :: soybean_
        ! growth_habit = determinate, indeterminate, semi-indeterminate, or vine
        character*20 :: growth_habit
        character*2  :: growth_stage
        integer :: Num_Of_Node
        integer :: Num_Of_Root
        character(2) :: Stage ! VE, CV, V1,V2, ..., R1, R2, ..., R8
        type(Seed_) :: Seed
        type(PlantNode_),allocatable :: NodeSystem(:)
        type(PlantRoot_),allocatable :: RootSystem(:)
        real(8) :: time
    contains
        procedure,public :: Init => initsoybean
        procedure,public :: sowing => initsoybean
        procedure,public :: export => exportSoybean
        procedure,public :: grow => growSoybean
        procedure,public :: WaterAbsorption => WaterAbsorptionSoybean
        !procedure,public :: AddNode => AddNodeSoybean
    end type

    type :: SoybeanCanopy_
        real(8) :: inter_row, intra_row
        type(soybean_),allocatable :: Canopy(:,:)
        
    end type


contains






! ########################################
subroutine initsoybean(obj,mass,water_content,radius,location,x,y,z,&
    PlantRoot_diameter_per_seed_radius,max_PlantNode_num,Variety,FileName)
    class(Soybean_),intent(inout) :: obj
    real(8),optional,intent(in) :: mass,water_content,radius,location(3),x,y,z
    real(8),optional,intent(in) :: PlantRoot_diameter_per_seed_radius
    character(*),optional,intent(in) :: Variety,FileName
    character(200) :: fn
    integer,optional,intent(in) :: max_PlantNode_num
    real(8) :: MaxThickness,Maxwidth,loc(3)

    obj%Stage = "VE"
    if(present(FileName) )then
        fn=FileName
    else
        fn="untitled"
    endif

    loc(:)=0.0d0

    if(present(x) )then
        loc(1)=x
    endif

    if(present(y) )then
        loc(2)=y
    endif

    if(present(z) )then
        loc(3)=z
    endif

    if(present(location) )then
        loc(:)=location(:)    
    endif

    ! initialize RootSystem and NodeSystem
    if(.not.allocated( obj%RootSystem) )then
        allocate(obj%RootSystem( input(default=1000,option=max_PlantNode_num) ) ) 
        obj%num_of_root=1
    endif
    if(.not.allocated( obj%NodeSystem) )then
        allocate(obj%NodeSystem( input(default=1000,option=max_PlantNode_num) ) ) 
        obj%num_of_node=1
    endif

    ! setup seed
    if(Variety=="Tachinagaha" .or. Variety=="tachinagaha" )then
        call obj%Seed%init(mass=mass,width1=9.70d0,width2=8.20d0,width3=7.70d0,&
            water_content=water_content,radius=radius,location=loc)    
        call obj%Seed%createMesh(FileName=trim(fn)//".stl",ElemType="Tetrahedra")

        call obj%Seed%convertMeshType(Option="TetraToHexa")
               
    else
        print *, "Variety name :: is not implemented."
        stop
    endif

    
    ! setup primary node (plumule)
    call obj%NodeSystem(1)%init(Stage=obj%Stage,Plantname="soybean",location=loc)

    ! setup primary node (radicle))
    MaxThickness=input(default=0.20d0,option=PlantRoot_diameter_per_seed_radius)*obj%Seed%radius
    Maxwidth    =input(default=0.20d0,option=PlantRoot_diameter_per_seed_radius)*obj%Seed%radius
    call obj%RootSystem(1)%init(Plantname="soybean",Stage=obj%Stage,MaxThickness=MaxThickness,Maxwidth=Maxwidth,location=loc)

    obj%time=0.0d0
end subroutine
! ########################################

! ########################################
subroutine growSoybean(obj,dt,Temp)
    class(Soybean_),intent(inout) :: obj
    real(8),intent(in) :: dt,temp ! time-interval

    if(trim(obj%Stage)=="VE")then
        ! VE
        ! Seed => VE

        ! water-absorption
        call obj%WaterAbsorption(dt=dt,temp=temp)

        ! Update RootSystem
        !call obj%UpdateRootSystemVE()

        ! Update NodeSystem
        !call obj%UpdateNodeSystemVE()


    elseif(trim(obj%Stage)=="CV" )then
        ! CV stage
    elseif(obj%Stage(1:1)=="R")then
        ! Reproductive Stage
    else
        print *, "Invalid growth stage"
        stop 
        ! Vagetative
    endif

end subroutine
! ########################################


! ########################################
subroutine WaterAbsorptionSoybean(obj,temp,dt)
    class(Soybean_),intent(inout) :: obj
    real(8),intent(in) :: temp,dt
    real(8) :: a,b,c,d,AA,BB,w1max,w2max,w3max,time
    real(8) :: x_rate,y_rate,z_rate,wx,wy,wz

    obj%time=obj%time+dt


    ! tested by tachinagaha, 2019
    a=0.00910d0
    b=-1.76450d0
    c=3.32E-04	
    d=-0.0905180d0
    AA=a*temp+b
    !BB=c*exp(d*temp)
    BB=c*temp+d
    ! width1 becomes 1.7 times, width2 becomes 1.2, width3 becomes 1.1
    w1max=1.70d0
    w2max=1.20d0
    w3max=1.10d0
    obj%seed%width1=obj%seed%width1_origin*(w1max - AA*exp(-BB*obj%time)   ) 
    obj%seed%width2=obj%seed%width2_origin*(w2max - AA*exp(-BB*obj%time)   ) 
    obj%seed%width3=obj%seed%width3_origin*(w3max - AA*exp(-BB*obj%time)   ) 

    ! linear model; it should be changed in near future.
    if(obj%time > 60.0d0*6.0d0)then
        obj%seed%width2=obj%seed%width2_origin*(w2max ) 
        obj%seed%width3=obj%seed%width3_origin*(w3max ) 
    else
        obj%seed%width2=obj%seed%width2_origin + obj%seed%width2_origin*(w2max-1.0d0 )*(obj%time)/(60.0d0*6.0d0) 
        obj%seed%width3=obj%seed%width3_origin + obj%seed%width3_origin*(w3max-1.0d0 )*(obj%time)/(60.0d0*6.0d0)
    endif

    wx = maxval(obj%Seed%FEMDomain%Mesh%NodCoord(:,1))-minval(obj%Seed%FEMDomain%Mesh%NodCoord(:,1)) 
    wy = maxval(obj%Seed%FEMDomain%Mesh%NodCoord(:,2))-minval(obj%Seed%FEMDomain%Mesh%NodCoord(:,2)) 
    wz = maxval(obj%Seed%FEMDomain%Mesh%NodCoord(:,3))-minval(obj%Seed%FEMDomain%Mesh%NodCoord(:,3)) 
    print *, wx,wy,wz
    x_rate =  1.0d0/wx
    y_rate =  1.0d0/wy
    z_rate =  1.0d0/wz
    call obj%Seed%FEMDomain%resize(x_rate=x_rate,y_rate=y_rate,z_rate=z_rate)
    x_rate = obj%seed%width1
    y_rate = obj%seed%width2
    z_rate = obj%seed%width3
    call obj%Seed%FEMDomain%resize(x_rate=x_rate,y_rate=y_rate,z_rate=z_rate)


end subroutine
! ########################################


! ########################################
subroutine exportSoybean(obj,FilePath,FileName,SeedID,withSTL,withMesh)
    class(Soybean_),intent(inout) :: obj
    character(*),optional,intent(in) :: FilePath
    character(*),intent(in) :: FileName
    integer,optional,intent(inout) :: SeedID
    logical,optional,intent(in) :: withSTL,withMesh
    integer :: i,itr

    itr=SeedID
    ! if seed exists => output
    if(obj%Seed%num_of_seed>=0)then
        if(present(withSTL) )then
            if(withSTL .eqv. .true.)then
                call obj%Seed%export(FileName=trim(FileName),SeedID=itr,extention=".stl")    
            endif
        endif
        if(present(withMesh) )then
            if(withMesh .eqv. .true.)then
                call obj%Seed%export(FileName=trim(FileName),SeedID=itr,extention=".pos")    
            endif
        endif

            
        if(present(FilePath) )then
            call obj%Seed%export(FileName=trim(FilePath)//"/seed.geo",SeedID=itr)
        else
            call obj%Seed%export(FileName=trim(FileName),SeedID=itr)
        endif
    endif

    itr=itr+1
    ! export NodeSystem
    do i=1,size(obj%NodeSystem)
            
        if(present(FilePath) )then
            call obj%NodeSystem(i)%export(FileName=trim(FilePath)//"/Node.geo",objID=itr)
        else
            call obj%NodeSystem(i)%export(FileName=trim(FileName)//"_Node.geo",objID=itr)
        endif
        if(i==obj%num_of_node  )then
            exit
        endif
    enddo

    
    ! export RootSystem
    do i=1,size(obj%RootSystem)
            
        if(present(FilePath) )then
            call obj%RootSystem(i)%export(FileName=trim(FilePath)//"/Root.geo",RootID=itr)
        else
            call obj%RootSystem(i)%export(FileName=trim(FileName)//"_Root.geo",RootID=itr)
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