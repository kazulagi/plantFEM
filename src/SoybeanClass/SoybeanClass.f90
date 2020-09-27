module SoybeanClass
    use, intrinsic :: iso_fortran_env
    use MathClass
    use SeedClass
    use LeafClass
    use RootClass
    use SoilClass
    use LightClass
    use PlantNodeClass
    implicit none
    
    type :: soybean_
        ! growth_habit = determinate, indeterminate, semi-indeterminate, or vine
        character*20 :: growth_habit
        character*2  :: growth_stage
        integer(int32) :: Num_Of_Node
        integer(int32) :: Num_Of_Root
        integer(int32) :: MaxLeafNum= 50
        integer(int32) :: MaxRootNum=200
        integer(int32) :: MaxStemNum= 50
        character(2) :: Stage ! VE, CV, V1,V2, ..., R1, R2, ..., R8
        integer(int32)::stage_id=0
        type(Seed_) :: Seed
        type(PlantNode_),allocatable :: NodeSystem(:)
        type(PlantRoot_),allocatable :: RootSystem(:)

        type(Stem_),allocatable :: Stem(:)
        type(Leaf_),allocatable :: Leaf(:)
        type(Root_),allocatable :: Root(:)
        type(Soil_),allocatable :: Soil
        
        ! 節-節点データ構造
        type(Mesh_) :: struct 
        integer(int32),allocatable :: leaf2stem(:,:)
        integer(int32),allocatable :: stem2stem(:,:)
        integer(int32),allocatable :: root2stem(:,:)
        integer(int32),allocatable :: root2root(:,:)
        
        ! 器官オブジェクト配列
        type(FEMDomain_),allocatable :: leaf_list(:)
        type(FEMDomain_),allocatable :: stem_list(:)
        type(FEMDomain_),allocatable :: root_list(:)
        real(real64) :: time
        real(real64) :: seed_length
        real(real64) :: seed_width
        real(real64) :: seed_height
        real(real64),allocatable :: stem_angle(:,:)
        real(real64),allocatable :: root_angle(:,:)
        real(real64),allocatable :: leaf_angle(:,:)

        character(200) :: stemconfig=" "
        character(200) :: rootconfig=" "
        character(200) :: leafconfig=" "
    contains
        procedure,public :: Init => initsoybean
        procedure,public :: new => initsoybean
        procedure,public :: sowing => initsoybean
        procedure,public :: export => exportSoybean

        procedure,public :: grow => growSoybean
        
        procedure,public :: show => showSoybean
        procedure,public :: gmsh => gmshSoybean

        procedure,public :: WaterAbsorption => WaterAbsorptionSoybean
        procedure,public :: move => moveSoybean

        procedure,public :: numleaf => numleafsoybean
        procedure,public :: numstem => numstemsoybean
        procedure,public :: numroot => numrootsoybean

        procedure,public :: laytracing => laytracingsoybean

        !procedure,public :: AddNode => AddNodeSoybean
    end type

    type :: SoybeanCanopy_
        real(real64) :: inter_row, intra_row
        type(soybean_),allocatable :: Canopy(:,:)
    end type

contains



! ########################################
subroutine initsoybean(obj,config,&
    regacy,mass,water_content,radius,location,x,y,z,&
    PlantRoot_diameter_per_seed_radius,max_PlantNode_num,Variety,FileName,&
    max_leaf_num,max_stem_num,max_root_num)
    class(Soybean_),intent(inout) :: obj

    real(real64),optional,intent(in) :: mass,water_content,radius,location(3),x,y,z
    real(real64),optional,intent(in) :: PlantRoot_diameter_per_seed_radius
    character(*),optional,intent(in) :: Variety,FileName,config
    logical,optional,intent(in) :: regacy
    character(200) :: fn,conf,line
    integer(int32),optional,intent(in) :: max_PlantNode_num,max_leaf_num,max_stem_num,max_root_num
    real(real64) :: MaxThickness,Maxwidth,loc(3),vec(3),rot(3),zaxis(3),meshloc(3),meshvec(3)
    integer(int32) :: i,j,k,blcount,id,rmc,n,node_id,node_id2,elemid
    type(IO_) :: soyconf

    
    ! 子葉節、初生葉節、根の第1節まで種子の状態で存在

    ! 節を生成するためのスクリプトを開く
    if(.not.present(config).or. index(config,".json")==0 )then
        ! デフォルトの設定を生成
        print *, "New soybean-configuration >> soyconfig.json"
        call soyconf%open("soyconfig.json")
        write(soyconf%fh,*) '{'
        write(soyconf%fh,*) '   "type": "soybean",'
        write(soyconf%fh,*) '   "stemconfig": "stemconfig.json",'
        write(soyconf%fh,*) '   "rootconfig": "rootconfig.json",'
        write(soyconf%fh,*) '   "leafconfig": "leafconfig.json",'
        write(soyconf%fh,*) '   "stage": 0,'
        write(soyconf%fh,*) '   "length": 0.0090,'
        write(soyconf%fh,*) '   "width" : 0.0081,'
        write(soyconf%fh,*) '   "height": 0.0072,'
        write(soyconf%fh,*) '   "MaxLeafNum": 50,'
        write(soyconf%fh,*) '   "MaxRootNum":200,'
        write(soyconf%fh,*) '   "MaxStemNum": 50'
        write(soyconf%fh,*) '}'
        conf="soyconfig.json"
        call soyconf%close()
    else
        conf = trim(config)
    endif
    
    call soyconf%open(trim(conf))
    blcount=0
    do
        read(soyconf%fh,'(a)') line
        print *, trim(line)
        if( adjustl(trim(line))=="{" )then
            blcount=1
            cycle
        endif
        if( adjustl(trim(line))=="}" )then
            exit
        endif
        
        if(blcount==1)then
            
            if(index(line,"type")/=0 .and. index(line,"soybean")==0 )then
                print *, "ERROR: This config-file is not for soybean"
                return
            endif


            if(index(line,"rootconfig")/=0 )then
                ! 茎の設定ファイル
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) obj%rootconfig
            endif

            if(index(line,"stemconfig")/=0 )then
                ! 茎の設定ファイル
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) obj%stemconfig
            endif

            if(index(line,"leafconfig")/=0 )then
                ! 茎の設定ファイル
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) obj%leafconfig
            endif


            if(index(line,"stage")/=0 )then
                ! 生育ステージ
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) obj%stage_id
            endif


            if(index(line,"MaxLeafNum")/=0 )then
                ! 生育ステージ
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) obj%MaxLeafNum
            endif


            if(index(line,"MaxStemNum")/=0 )then
                ! 生育ステージ
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) obj%MaxStemNum
            endif


            if(index(line,"MaxRootNum")/=0 )then
                ! 生育ステージ
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) obj%MaxRootNum
            endif

            if(index(line,"length")/=0 )then
                ! 種子の長さ
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) obj%seed_length
            endif

            if(index(line,"width")/=0 )then
                ! 種子の長さ
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) obj%seed_width
            endif

            if(index(line,"height")/=0 )then
                ! 種子の長さ
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) obj%seed_height
            endif

            cycle

        endif

    enddo
    call soyconf%close()

    

    if(index(config,".json")==0 )then
        obj%stemconfig=" "
        obj%rootconfig=" "
        obj%leafconfig=" "
    endif



    ! create leaf, root, stem
    allocate(obj%leaf(obj%MaxLeafNum) )
    allocate(obj%root(obj%MaxrootNum) )
    allocate(obj%stem(obj%MaxstemNum) )
    allocate(obj%stem2stem(obj%MaxstemNum,obj%MaxstemNum) )
    allocate(obj%leaf2stem(obj%MaxstemNum,obj%MaxLeafNum) )
    allocate(obj%root2stem(obj%MaxrootNum,obj%MaxstemNum) )
    allocate(obj%root2root(obj%MaxrootNum,obj%MaxrootNum) )
    obj%stem2stem(:,:) = 0
    obj%leaf2stem(:,:) = 0
    obj%root2stem(:,:) = 0
    obj%root2root(:,:) = 0
    !allocate(obj%struct%NodCoord(4,3) )
    !allocate(obj%struct%ElemNod(3,2) )
    !allocate(obj%struct%ElemMat(3) )
    ! 子葉結節部=(0,0,0)
    !obj%struct%NodCoord(1,1:3) = 0.0d0
    call obj%leaf(1)%init(obj%leafconfig)
    call obj%leaf(1)%rotate(x=radian(90.0d0),y=radian(90.0d0),z=radian(10.0d0) )
    call obj%leaf(2)%init(obj%leafconfig)
    call obj%leaf(2)%rotate(x=radian(90.0d0),y=radian(90.0d0),z=radian(-10.0d0) )
    
    call obj%stem(1)%init(obj%stemconfig)
    call obj%stem(1)%rotate(x=radian(40.0d0) )
    
    call obj%stem(2)%init(obj%stemconfig)
    call obj%stem(2)%rotate(x=radian(80.0d0) )

    call obj%root(1)%init(obj%rootconfig)
    call obj%root(1)%fix(x=0.0d0,y=0.0d0,z=0.0d0)
    call obj%root(1)%rotate(x=radian(-60.0d0) )

    call obj%leaf(1)%connect("=>",obj%stem(1))
    obj%leaf2stem(1,1) = 1.0d0 
    
    call obj%leaf(2)%connect("=>",obj%stem(1))
    obj%leaf2stem(2,1) = 1.0d0
    
    call obj%stem(2)%connect("=>",obj%stem(1))
    obj%stem2stem(2,1) = 1.0d0
    
    call obj%root(1)%connect("=>",obj%stem(1))
    obj%root2stem(1,1) = 1.0d0
    
    obj%stage = "VE"
    ! 初生葉結節部
    !obj%struct%NodCoord(2,1) = 0.0d0
    !obj%struct%NodCoord(2,2) = 0.0d0
    !obj%struct%NodCoord(2,3) = 1.0d0/20.0d0*obj%seed_height
    ! 地際部
    !obj%struct%NodCoord(3,1) = 1.0d0/4.0d0*obj%seed_length
    !obj%struct%NodCoord(3,2) = 0.0d0
    !obj%struct%NodCoord(3,3) = -1.0d0/3.0d0*obj%seed_height
    ! 根冠
    !obj%struct%NodCoord(4,1) = 1.0d0/2.0d0*obj%seed_length
    !obj%struct%NodCoord(4,2) = 0.0d0
    !obj%struct%NodCoord(4,3) = -1.0d0/2.0d0*obj%seed_height

    ! 子葉-初生葉節
    !obj%struct%ElemNod(1,1) = 1
    !obj%struct%ElemNod(1,2) = 2
    ! 地際-子葉節
    !obj%struct%ElemNod(2,1) = 3
    !obj%struct%ElemNod(2,2) = 1
    ! 地際-根冠節
    !obj%struct%ElemNod(3,1) = 3
    !obj%struct%ElemNod(3,2) = 4

    ! 子葉-初生葉節 stem: 1
    !obj%struct%ElemMat(1) = 1
    ! 地際-子葉節 stem: 1
    !obj%struct%ElemMat(2) = 1
    ! 地際-根冠節 primary root: -1
    !obj%struct%ElemMat(3) = -1

    ! FEメッシュを生成
    ! 領域を確保
!    n = input(default=80,option=max_leaf_num)
!    allocate(obj%leaf_list(n) )
!    n = input(default=80,option=max_stem_num)
!    allocate(obj%stem_list(n) )
!    n = input(default=80,option=max_root_num)
!    allocate(obj%root_list(n) )
!
!    ! 子葉のメッシュを生成
!    call obj%leaf_list(1)%create(meshtype="Sphere3D",x_num=10,y_num=10,z_num=10,&
!        x_len=obj%seed_length,y_len=obj%seed_width,z_len=obj%seed_height)
!    call obj%leaf_list(1)%move(x=0.0d0,y=-0.50d0*obj%seed_width,z=-0.50d0*obj%seed_height)
!
!    call obj%leaf_list(2)%create(meshtype="Sphere3D",x_num=10,y_num=10,z_num=10,&
!        x_len=obj%seed_length,y_len=obj%seed_width,z_len=obj%seed_height)
!    call obj%leaf_list(2)%rotate(x=radian(180.0d0) )
!    call obj%leaf_list(2)%move(x=0.0d0,y=-0.50d0*obj%seed_width,z=-0.50d0*obj%seed_height)
!
!
!
!    ! 子葉-初生葉節のメッシュを生成
!    rot(:) = 0.0d0
!    call obj%stem_list(1)%create(meshtype="rectangular3D",x_num=5,y_num=5,z_num=10,&
!        x_len=obj%seed_width/6.0d0,y_len=obj%seed_width/6.0d0,z_len=obj%seed_length/4.0d0)
!    ! 節基部の節点ID
!    node_id = obj%struct%ElemNod(1,1)
!    ! 節先端部の節点ID
!    node_id2= obj%struct%ElemNod(1,2)
!    ! 節基部の位置ベクトル
!    loc(:) = obj%struct%NodCoord( node_id  ,:)
!    ! 節先端部までの方向ベクトル
!    vec(:) =  obj%struct%NodCoord( node_id2 ,:) - obj%struct%NodCoord( node_id  ,:)  
!    
!    ! structの構造データにメッシュデータを合わせる。
!    print *, obj%stem_list(1)%Mesh%BottomElemID
!    print *, obj%stem_list(1)%Mesh%TopElemID
!
!    elemid = obj%stem_list(1)%Mesh%BottomElemID
!    node_id = obj%stem_list(1)%Mesh%ElemNod(elemID,1)
!    meshloc(:) = obj%stem_list(1)%Mesh%NodCoord(node_id,:)
!
!    elemid = obj%stem_list(1)%Mesh%TopElemID
!    node_id = obj%stem_list(1)%Mesh%ElemNod(elemID,1)
!    meshvec(:) = obj%stem_list(1)%Mesh%NodCoord(node_id,:)-meshloc(:)

    !print *, "loc",loc
    !print *, "meshloc",meshloc
    !print *, "vec",vec
    !print *, "meshvec",meshvec
    
!    ! 節中央を原点へ
!    call obj%stem_list(1)%move(x=-obj%seed_width/12.0d0,y=-obj%seed_width/12.0d0)
!    
!    print *, "loc",loc
!    print *, "vec",vec
!    print *, "rot",rot
!    zaxis(:)=0.0d0
!    zaxis(3)=obj%seed_length/5.0d0
!    rot(:) = angles(zaxis,vec)
!    call obj%stem_list(1)%move(x=loc(1),y=loc(2),z=loc(3) )
!    call obj%stem_list(1)%rotate(x=0.0d0,y=0.0d0,z=0.0d0 )
!!    
!    
!!    
!
!
!    ! 地際-子葉節のメッシュを生成
!    rot(:) = 0.0d0
!    call obj%stem_list(2)%create(meshtype="rectangular3D",x_num=5,y_num=5,z_num=10,&
!        x_len=obj%seed_width/6.0d0,y_len=obj%seed_width/6.0d0,z_len=obj%seed_length/4.0d0)
!    ! 節基部の節点ID
!    node_id = obj%struct%ElemNod(2,1)
!    ! 節先端部の節点ID
!    node_id2= obj%struct%ElemNod(2,2)
!    ! 節基部の位置ベクトル
!    loc(:) = obj%struct%NodCoord( node_id  ,:)
!    ! 節先端部までの方向ベクトル
!    vec(:) =  obj%struct%NodCoord( node_id2 ,:) - obj%struct%NodCoord( node_id  ,:)  
!    ! 節中央を原点へ
!    call obj%stem_list(2)%move(x=-obj%seed_width/12.0d0,y=-obj%seed_width/12.0d0,&
!        z=-obj%seed_length/8.0d0)
!    zaxis(:)=0.0d0
!    zaxis(3)=obj%seed_length/5.0d0
!    rot(:) = angles(zaxis,vec)
!    print *, "loc",loc
!    print *, "vec",vec
!    print *, "rot",rot
!    !call obj%stem_list(2)%rotate(x=rot(1),y=rot(2),z=rot(3) )
!    call obj%stem_list(2)%move(x=loc(1),y=loc(2),z=loc(3) )
!    
!
!
!    ! 地際-根冠節のメッシュ生成
!    rot(:) = 0.0d0
!    call obj%root_list(1)%create(meshtype="rectangular3D",x_num=5,y_num=5,z_num=10,&
!        x_len=obj%seed_width/6.0d0,y_len=obj%seed_width/6.0d0,z_len=obj%seed_length/4.0d0)
!    ! 節基部の節点ID
!    node_id = obj%struct%ElemNod(3,1)
!    ! 節先端部の節点ID
!    node_id2= obj%struct%ElemNod(3,2)
!    ! 節基部の位置ベクトル
!    loc(:) = obj%struct%NodCoord( node_id  ,:)
!    ! 節先端部までの方向ベクトル
!    vec(:) =  obj%struct%NodCoord( node_id2 ,:) - obj%struct%NodCoord( node_id  ,:)  
!    ! 節基部へ移動
!    call obj%root_list(1)%move(x=-obj%seed_width/12.0d0,y=-obj%seed_width/12.0d0,&
!        z=-obj%seed_length/8.0d0)
!    call obj%root_list(1)%move(x=loc(1),y=loc(2),z=loc(3) )
!    zaxis(:)=0.0d0
!    zaxis(3)=obj%seed_length/5.0d0
!    rot(:) = angles(zaxis,vec)
!    !call obj%root_list(1)%rotate(x=rot(1),y=rot(2),z=rot(3) )
!    print *, "loc",loc
!    print *, "vec",vec
!    print *, "rot",rot


    ! ここからレガシーモード
    if(present(regacy) )then
        if(regacy .eqv. .true.)then
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
                call obj%Seed%init(mass=mass,width1=9.70d0,width2=8.20d0,&
                    width3=7.70d0,&
                    water_content=water_content,radius=radius,location=loc)    
                call obj%Seed%createMesh(FileName=trim(fn)//".stl",&
                ElemType="Tetrahedra")

                call obj%Seed%convertMeshType(Option="TetraToHexa")

            else
                print *, "Variety name :: is not implemented."
                stop
            endif


            ! setup primary node (plumule)
            call obj%NodeSystem(1)%init(Stage=obj%Stage,&
            Plantname="soybean",location=loc)

            ! setup primary node (radicle))
            MaxThickness=input(default=0.20d0,&
            option=PlantRoot_diameter_per_seed_radius)*obj%Seed%radius
            Maxwidth    =input(default=0.20d0,&
            option=PlantRoot_diameter_per_seed_radius)*obj%Seed%radius
            call obj%RootSystem(1)%init(Plantname="soybean",&
            Stage=obj%Stage,MaxThickness=MaxThickness,Maxwidth=Maxwidth,location=loc)

            obj%time=0.0d0
            return
        endif
    endif


end subroutine
! ########################################

! ########################################
subroutine growSoybean(obj,dt,light,air,temp)
    class(Soybean_),intent(inout) :: obj
    type(Light_),optional,intent(inout) :: light
    type(air_),optional,intent(in) :: air
    real(real64),optional,intent(in) :: temp
    real(real64),intent(in) :: dt! time-interval
    real(real64) :: ac_temp ! time-interval
    integer(int32) :: i

    ! 光量子量を計算
    call obj%laytracing(light=light)

    ! 光合成量を計算
    do i=1,size(obj%Leaf)
        if(obj%Leaf(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%leaf(i)%photosynthesis(dt=dt,air=air)
        endif
    enddo

    ! ソースの消耗、拡散を計算
    !call obj%source2sink()

    ! 伸長を計算
    !call obj%extention()

    ! 分化を計算、構造の更新
    !call obj%development()


end subroutine
! ########################################


! ########################################
subroutine WaterAbsorptionSoybean(obj,temp,dt)
    class(Soybean_),intent(inout) :: obj
    real(real64),intent(in) :: temp,dt
    real(real64) :: a,b,c,d,AA,BB,w1max,w2max,w3max,time
    real(real64) :: x_rate,y_rate,z_rate,wx,wy,wz

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
    integer(int32),optional,intent(inout) :: SeedID
    logical,optional,intent(in) :: withSTL,withMesh
    integer(int32) :: i,itr

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
!    integer(int32),optional,intent(in)::Max_Num_of_Node
!    integer(int32) ::n
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
!    real(real64),optional,intent(in)::SizeRatio
!    real(real64) :: magnif
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

! ########################################
subroutine showSoybean(obj,name)
    class(Soybean_),intent(inout) :: obj
    character(*),intent(in)::name

    if( obj%struct%empty() .eqv. .true.)then
        print *, "Error :: showSoybean>> no structure is imported."
        return
    endif

    call obj%struct%export(name=name)

end subroutine
! ########################################



! ########################################
function numleafsoybean(obj) result(ret)
    class(Soybean_),intent(in) :: obj
    integer(int32) :: ret,i

    ret=0
    do i=1,size(obj%leaf_list)
        if(obj%leaf_list(i)%Mesh%empty() .eqv. .false. )then
            ret=ret+1
        endif
    enddo
    
end function
! ########################################

! ########################################
function numstemsoybean(obj) result(ret)
    class(Soybean_),intent(in) :: obj
    integer(int32) :: ret,i

    ret=0
    do i=1,size(obj%stem_list)
        if(obj%stem_list(i)%Mesh%empty() .eqv. .false. )then
            ret=ret+1
        endif
    enddo
    
end function
! ########################################

! ########################################
function numrootsoybean(obj) result(ret)
    class(Soybean_),intent(in) :: obj
    integer(int32) :: ret,i

    ret=0
    do i=1,size(obj%root_list)
        if(obj%root_list(i)%Mesh%empty() .eqv. .false. )then
            ret=ret+1
        endif
    enddo
    
end function
! ########################################


! ########################################
subroutine gmshSoybean(obj,name)
    class(Soybean_),intent(inout) :: obj
    character(*),intent(in) :: name
    integer(int32) :: i

    do i=1,size(obj%stem)
        if(obj%stem(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%stem(i)%gmsh(name=trim(name)//"_stem"//trim(str(i)))
        endif
    enddo

    do i=1,size(obj%root)
        if(obj%root(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%root(i)%gmsh(name=trim(name)//"_root"//trim(str(i)))
        endif
    enddo

    do i=1,size(obj%leaf)
        if(obj%leaf(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%leaf(i)%gmsh(name=trim(name)//"_leaf"//trim(str(i)))
        endif
    enddo

end subroutine
! ########################################

! ########################################
subroutine moveSoybean(obj,x,y,z)
    class(Soybean_),intent(inout) :: obj
    real(real64),optional,intent(in) :: x,y,z
    integer(int32) :: i

    do i=1,size(obj%stem)
        if(obj%stem(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%stem(i)%move(x=x,y=y,z=z)
        endif
    enddo

    do i=1,size(obj%root)
        if(obj%root(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%root(i)%move(x=x,y=y,z=z)
        endif
    enddo

    do i=1,size(obj%leaf)
        if(obj%leaf(i)%femdomain%mesh%empty() .eqv. .false. )then
            call obj%leaf(i)%move(x=x,y=y,z=z)
        endif
    enddo

end subroutine
! ########################################

! ########################################
subroutine laytracingsoybean(obj,light)
    class(Soybean_),intent(inout) :: obj
    type(Light_),intent(in) :: light
    real(real64),allocatable :: stemcenter(:,:),stemradius(:)
    real(real64),allocatable :: leafcenter(:,:),leafradius(:)
    real(real64),allocatable :: elemnodcoord(:,:),x(:),x2(:)
    real(real64) :: max_PPFD,r,rc,r0
    real(real64),parameter :: extinction_ratio = 100.0d0 ! ratio/m
    !real(real64),parameter :: radius_ratio = 0.01d0 ! radius_of_gauss_point/element_length
    type(IO_) :: f
    integer(int32) :: i,j,n,num_particle,k,l,nodeid,m,totcount

    max_PPFD = light%maxPPFD
    ! 総当りで、総遮蔽長を割り出す
    ! 茎は光を通さない、葉は透過率あり、空間は透過率ゼロ
    ! 要素中心から頂点への平均長さを半径に持ち、要素中心を中心とする球
    ! を考え、Layとの公差判定を行う。
    num_particle = 0
    do i=1,size(obj%leaf)
        if(obj%leaf(i)%femdomain%mesh%empty() .eqv. .false. )then
            num_particle=num_particle+size(obj%leaf(i)%femdomain%mesh%ElemNod,1)
        endif
    enddo
    allocate(leafcenter(num_particle,3),leafradius(num_particle) )
    leafcenter(:,:) = 0.0d0
    leafradius(:) = 0.0d0

    num_particle = 0
    do i=1,size(obj%leaf)
        if(obj%stem(i)%femdomain%mesh%empty() .eqv. .false. )then
            num_particle=num_particle+size(obj%stem(i)%femdomain%mesh%ElemNod,1)
        endif
    enddo
    allocate(stemcenter(num_particle,3),stemradius(num_particle) )
    stemcenter(:,:) = 0.0d0
    stemradius(:) = 0.0d0

    num_particle = 0
    
    do i=1,size(obj%leaf)
        if(obj%leaf(i)%femdomain%mesh%empty() .eqv. .false. )then
            n = size(obj%leaf(i)%femdomain%mesh%Elemnod,2)
            m = size(obj%leaf(i)%femdomain%mesh%Nodcoord,2)
            allocate(elemnodcoord(n,m) )
            allocate(x(m) )
            do j=1,size(obj%leaf(i)%femdomain%mesh%elemnod,1)
                do k=1,size(obj%leaf(i)%femdomain%mesh%elemnod,2)
                    nodeid = obj%leaf(i)%femdomain%mesh%elemnod(j,k)
                    elemnodcoord(k,:) = obj%leaf(i)%femdomain%mesh%Nodcoord(nodeid,:)
                enddo
                num_particle = num_particle+1
                do k=1, size(elemnodcoord,1)
                    do l=1, size(elemnodcoord,2)
                        leafcenter(num_particle,l) = &
                        + leafcenter(num_particle,l) &
                        + 1.0d0/dble(size(elemnodcoord,1))*elemnodcoord(k,l)
                    enddo
                enddo
                do k=1, size(elemnodcoord,1)
                    x(:) = elemnodcoord(k,:)
                    x(:) = x(:) - leafcenter(num_particle,:)
                    if(k>=2 .and. leafradius(num_particle) > sqrt(dot_product(x,x))  )then
                        leafradius(num_particle) = sqrt(dot_product(x,x))
                    elseif(k==1)then
                        leafradius(num_particle) = sqrt(dot_product(x,x))    
                    else
                        cycle
                    endif
                enddo
            enddo
            deallocate(elemnodcoord)
            deallocate(x)
        endif
    enddo


    num_particle = 0
    do i=1,size(obj%stem)
        if(obj%stem(i)%femdomain%mesh%empty() .eqv. .false. )then
            n = size(obj%stem(i)%femdomain%mesh%Elemnod,2)
            m = size(obj%stem(i)%femdomain%mesh%Nodcoord,2)
            allocate(elemnodcoord(n,m) )
            allocate(x(m) )
            do j=1,size(obj%stem(i)%femdomain%mesh%elemnod,1)
                do k=1,size(obj%stem(i)%femdomain%mesh%elemnod,2)
                    nodeid = obj%stem(i)%femdomain%mesh%elemnod(j,k)
                    elemnodcoord(k,:) = obj%stem(i)%femdomain%mesh%Nodcoord(nodeid,:)
                enddo
                num_particle = num_particle+1
                do k=1, size(elemnodcoord,1)
                    do l=1, size(elemnodcoord,2)
                        stemcenter(num_particle,l) = &
                        + stemcenter(num_particle,l) &
                        + 1.0d0/dble(size(elemnodcoord,1))*elemnodcoord(k,l)
                    enddo
                enddo
                do k=1, size(elemnodcoord,1)
                    x(:) = elemnodcoord(k,:)
                    x(:) = x(:) - stemcenter(num_particle,:)
                    !最小半径で考える
                    if(k>=2 .and. stemradius(num_particle) > sqrt(dot_product(x,x))  )then
                        stemradius(num_particle) = sqrt(dot_product(x,x))
                    elseif(k==1)then
                        stemradius(num_particle) = sqrt(dot_product(x,x))    
                    else
                        cycle
                    endif
                enddo
            enddo
            deallocate(elemnodcoord)
            deallocate(x)
        endif
    enddo
    

    ! DEBUG
    call f%open("leaf.txt")
    do i=1,size(leafcenter,1)
        write(f%fh,*) leafcenter(i,:)
    enddo
    call f%close()
    
    call f%open("stem.txt")
    do i=1,size(stemcenter,1)
        write(f%fh,*) stemcenter(i,:)
    enddo
    call f%close()
    
    allocate(x(3),x2(3) )
    
    
    num_particle = 0
    totcount = 0
    do i=1,size(obj%leaf)
        if(obj%leaf(i)%femdomain%mesh%empty() .eqv. .false. )then
            ! 葉あり
            obj%leaf(i)%PPFD(:) = max_PPFD
            do j=1,size(obj%leaf(i)%PPFD)
                totcount = totcount + 1
                num_particle = num_particle + 1
                ! それぞれの要素について、遮蔽particleを探索
                ! 茎：全減衰
                ! 葉：半減衰
                ! 簡単のため上からのみ
                ! x-yのみについて見て、上方かつx-y平面距離が半径以内で覆陰判定
                x(:) = leafcenter(num_particle,:)
                r0   = leafradius(num_particle)
                ! 枝による覆陰判定
                
                do k=1, size(stemcenter,1)
                    x2(:) = stemcenter(k,:)
                    r     = stemradius(k)
                    rc    = ( x(1)-x2(1) )**(2.0d0) + ( x(2)-x2(2) )**(2.0d0) 
                    rc    = sqrt(rc)
                    if(rc <= r0 + r .and. x(3) < x2(3) )then
                        ! 茎により覆陰されてる
                        obj%leaf(i)%PPFD(j) = 0.0d0
                        exit
                    endif
                enddo
                if(obj%leaf(i)%PPFD(j) == 0.0d0)then
                    cycle
                endif

                do k=1, size(leafcenter,1)
                    ! もし自信だったら除外
                    if(totcount == k)then
                        cycle
                    endif
                    
                    x2(:) = leafcenter(k,:)
                    r     = leafradius(k)
                    rc    = ( x(1)-x2(1) )**(2.0d0) + ( x(2)-x2(2) )**(2.0d0) 
                    rc    = sqrt(rc)
                    if(rc <= (r0 + r)/2.0d0 .and. x(3) < x2(3) )then
                        ! 茎により覆陰されてる
                        obj%leaf(i)%PPFD(j) = &
                        obj%leaf(i)%PPFD(j)*(1.0d0-extinction_ratio*2.0d0*r)
                        if( obj%leaf(i)%PPFD(j) <= 0.0d0 )then
                            obj%leaf(i)%PPFD(j) = 0.0d0
                        endif
                    endif
                enddo

            enddo
        endif
    enddo
    
    call f%open("PPFD.txt")
    do i=1,size(obj%leaf)
        if(obj%leaf(i)%femdomain%mesh%empty() .eqv. .false. )then
            ! 葉あり
            do j=1,size(obj%leaf(i)%PPFD,1)
                write(f%fh,*) obj%leaf(i)%PPFD(j),"leaf_id: ",str(i),"elem_id: ",str(j)
            enddo
        endif
    enddo
    call f%close()
    


end subroutine
! ########################################
end module