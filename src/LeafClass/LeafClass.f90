module LeafClass
    use, intrinsic :: iso_fortran_env
    use KinematicClass
    use FEMDomainClass
    use PetiClass
    use StemClass
    use LightClass
    use AirClass
    implicit none

    integer(int32) :: PF_SOYBEAN_CV = 100
    type :: Leaf_
        type(FEMDomain_)    ::  FEMDomain
        real(real64),allocatable ::  LeafSurfaceNode2D(:,:)
        real(real64)             ::  ShapeFactor,Thickness,length,width,center(3)
        real(real64)             ::  MaxThickness,Maxlength,Maxwidth
        real(real64)             ::  center_bottom(3),center_top(3)
        real(real64)             ::  outer_normal_bottom(3),outer_normal_top(3)
        real(real64),allocatable ::  source(:), ppfd(:),A(:)
        integer(int32)             ::  Division
        type(leaf_),pointer ::  pleaf
        type(Peti_),pointer ::  pPeti
        real(real64)             ::  rot_x = 0.0d0
        real(real64)             ::  rot_y = 0.0d0
        real(real64)             ::  rot_z = 0.0d0
        real(real64)             ::  disp_x = 0.0d0
        real(real64)             ::  disp_y = 0.0d0
        real(real64)             ::  disp_z = 0.0d0
        real(real64)             ::  shaperatio = 0.30d0
        real(real64)             ::  minwidth,minlength,MinThickness

        ! id in multi-leaf
        integer(int32) :: LeafID = -1
        logical :: already_grown = .false.

        integer(int32),allocatable  :: I_planeNodeID(:)
        integer(int32),allocatable  :: I_planeElementID(:)
        integer(int32),allocatable  :: II_planeNodeID(:)
        integer(int32),allocatable  :: II_planeElementID(:)
        integer(int32)  :: A_PointNodeID
        integer(int32)  :: B_PointNodeID
        integer(int32)  :: C_PointNodeID
        integer(int32)  :: D_PointNodeID
        integer(int32)  :: A_PointElementID
        integer(int32)  :: B_PointElementID
        integer(int32)  :: C_PointElementID
        integer(int32)  :: D_PointElementID
        integer(int32)  :: xnum = 10
        integer(int32)  :: ynum = 10
        integer(int32)  :: znum = 10

        ! phisiological parameters


        real(real64) :: V_cmax = 100.0d0 ! 最大カルボキシル化反応速度, mincro-mol/m-2/s
        real(real64) :: V_omax = 100.0d0 ! 最大酸素化反応速度, mincro-mol/m-2/s, lambdaから推定
        real(real64) :: O2 = 380.0d0! 酸素濃度, ppm
        real(real64) :: CO2=202000.0d0! 二酸化炭素濃度, ppm
        real(real64) :: R_d=1.0d0 ! 暗呼吸速度, mincro-mol/m-2/s
    
        real(real64) :: K_c=272.380d0 ! CO2に対するミカエリス定数
        real(real64) :: K_o=165820.0d0 ! O2に対するミカエリス定数
    
        real(real64) :: J_=0.0d0 ! 電子伝達速度
        real(real64) :: I_=0.0d0 ! 光強度
        real(real64) :: phi=0.0d0 ! I-J曲線の初期勾配
        real(real64) :: J_max=180.0d0 !最大電子伝達速度,mincro-mol/m-2/s
        real(real64) :: theta_r=0.0d0 ! 曲線の凸度
    
        real(real64) :: maxPPFD=1.0d0 ! micro-mol/m^2/s
    
        real(real64) :: Lambda= 37.430d0 ! 暗呼吸速度を無視した時のCO2補償点ppm
        real(real64) :: temp=303.0d0 ! temp


        ! physical parameter
        real(real64),allocatable :: DryDensity(:)  ! element-wise
        real(real64),allocatable :: WaterContent(:)! element-wise

        ! For deformation analysis
        real(real64),allocatable :: YoungModulus(:)! element-wise
        real(real64),allocatable :: PoissonRatio(:)! element-wise
        real(real64),allocatable :: Density(:)     ! element-wise
        real(real64),allocatable :: CarbonDiffusionCoefficient(:)
        real(real64),allocatable :: Stress(:,:,:)     ! Gauss point-wise
        real(real64),allocatable :: Displacement(:,:) ! node-wise, three dimensional
        

        real(real64),allocatable :: BoundaryTractionForce(:,:) ! node-wise, three dimensional
        real(real64),allocatable :: BoundaryDisplacement(:,:) ! node-wise, three dimensional
        
        ! growth parameters
        real(real64)  :: my_time = 0.0d0
        real(real64)  :: initial_width  = 0.0010d0 ! 1.0 mm
        real(real64)  :: initial_length = 0.0010d0 ! 1.0 mm
        real(real64)  :: final_width  = 0.060d0   !  60.0 mm
        real(real64)  :: final_length = 0.100d0   ! 100.0 mm
        real(real64)  :: width_growth_ratio = 1.0d0/4.0d0   ! 
        real(real64)  :: length_growth_ratio = 1.0d0/4.0d0   ! 

        !Sink-source flow parameter
        real(real64) :: default_CarbonDiffusionCoefficient=0.0010d0 ! ソースの拡散係数 mincro-mol/m^2/m/s

    contains
        procedure, public :: Init => initLeaf
        procedure, public :: grow => growLeaf

        procedure, public :: rotate => rotateleaf
        procedure, public :: move => moveleaf
        procedure, public :: curve => curveleaf
        procedure, public :: create => createLeaf
        
        procedure,pass :: connectLeafLeaf => connectLeafLeaf
        procedure,pass :: connectLeafStem => connectLeafStem

        generic :: connect => connectLeafLeaf, connectLeafStem
        
        procedure, public :: photosynthesis => photosynthesisLeaf
        procedure, public :: getPhotosynthesisSpeedPerVolume => getPhotosynthesisSpeedPerVolumeLeaf
        
        procedure, public :: rescale => rescaleleaf
        procedure, public :: adjust => adjustLeaf
        procedure, public :: resize => resizeleaf
        
        ! check condition
        ! is it empty?
        procedure, public :: empty => emptyLeaf
        procedure, public :: getVolume => getVolumeLeaf
        procedure, public :: getLength => getLengthLeaf
        procedure, public :: getBiomass => getBiomassLeaf
        procedure, public :: getCoordinate => getCoordinateleaf
        procedure, public :: getLeafArea => getLeafAreaLeaf
        procedure, public :: getRadius => getRadiusLeaf
        procedure, public :: getCenter => getCenterLeaf
        procedure, public :: getNormalVector => getNormalVectorLeaf

        procedure, public :: FullyExpanded => FullyExpandedLeaf

        procedure, public :: gmsh => gmshleaf
        procedure, public :: msh => mshleaf
        procedure, public :: vtk => vtkleaf
        procedure, public :: stl => stlleaf



        procedure, public :: sync => syncleaf


        procedure,public :: nn => nnLeaf
        procedure,public :: ne => neLeaf

        ! remove
        procedure, public :: remove => removeLeaf
    end type
contains



subroutine createLeaf(obj,SurfacePoints,filename,x_num,y_num,x_len,y_len)
    class(Leaf_),intent(inout) :: obj
    real(real64),optional,intent(in) :: SurfacePoints(:,:),x_len,y_len
    character(*),optional,intent(in) :: filename
    integer(int32),optional,intent(in) :: x_num,y_num

    type(IO_) :: f
    type(FEMDomain_) :: domain
    type(Math_) :: math
    character(:),allocatable :: line
    real(real64) :: x, y, r ,theta,x_sum,y_sum,center(2),max_r,coord(2), ret
    real(real64),allocatable :: r_data(:),theta_data(:),tx(:),tfx(:)
    integer(int32) :: num_ptr, i,id,ids(5),id_n

    if(present(filename) )then
        call f%open(filename,"r")
        ! get brief info
        num_ptr = 0

        x_sum = 0.0d0
        y_sum = 0.0d0

        do 
            line = f%readline()
            if(f%EOF) exit
            num_ptr = num_ptr+1
            ! read x-y
            read(line,*) x, y
            x_sum = x_sum + x
            y_sum = y_sum + y
        enddo
        call f%close()

        center(1) = x_sum/dble(num_ptr)
        center(2) = y_sum/dble(num_ptr)

        r_data = zeros(num_ptr)
        theta_data = zeros(num_ptr)

        ! get detail
        call f%open(filename,"r")
        num_ptr=0
        do 
            line = f%readline()
            if(f%EOF) exit
            ! read x-y
            read(line,*) x, y

            coord(1) = x - center(1)
            coord(2) = y - center(2)
            r = sqrt( dot_product(coord,coord) )
            theta = angles( coord )

            num_ptr = num_ptr + 1

            r_data(num_ptr) = r
            theta_data(num_ptr) = theta 
        enddo
        max_r = maxval(r_data)
        r_data = r_data/max_r
        call f%close()
    elseif(present(SurfacePoints) )then
        num_ptr = size(SurfacePoints,1)
        center(1) = x_sum/dble(num_ptr)
        center(2) = y_sum/dble(num_ptr)

        r_data = zeros(num_ptr)
        theta_data = zeros(num_ptr)

        num_ptr=0
        do i=1,size(SurfacePoints)

            ! read x-y
            x = SurfacePoints(i,1)
            y = SurfacePoints(i,2)

            coord(1) = x - center(1)
            coord(2) = y - center(2)

            r = sqrt( dot_product(coord,coord) )
            theta = angles( coord )

            num_ptr = num_ptr + 1

            r_data(num_ptr) = r
            theta_data(num_ptr) = theta 
        enddo
        max_r = maxval(r_data)
        r_data = r_data/max_r

    else
        print *, "ERROR :: Leaf%create >> Please import SurfacePoints or Filename"
        stop
    endif
    
    call obj%femdomain%create("Cylinder3D",x_num=x_num,y_num=y_num)
    call obj%femdomain%resize(x=2.0d0)
    call obj%femdomain%resize(y=2.0d0)
    call obj%femdomain%resize(z=0.010d0)

    ! ####################################
    ! test interpolate


    !tx = [0.0d0, 1.0d0, 2.0d0, 3.0d0]
    !tfx = [0.0d0, 2.0d0, 4.0d0, 8.0d0]
    !ret = interpolate(x =tx,Fx=tfx,x_value = -0.50d0)
    !print *, ret
    !stop
    ! ####################################

    ! adjust shape
    do i=1,obj%femdomain%nn()
        x = obj%femdomain%mesh%nodcoord(i,1)
        y = obj%femdomain%mesh%nodcoord(i,2)
        r = sqrt(x**2 + y**2)
        coord(1:2) = obj%femdomain%mesh%nodcoord(i,1:2)
        r = norm(coord)
        theta = angles(coord)
        ! find nearest theta
        r = r * interpolate(x=theta_data,Fx=r_data,x_value=theta)
        x = r*x
        y = r*y
        obj%femdomain%mesh%nodcoord(i,1) = x 
        obj%femdomain%mesh%nodcoord(i,2) = y 
    enddo

    obj%A_PointNodeID = randi(obj%femdomain%nn())
    obj%B_PointNodeID = randi(obj%femdomain%nn())
    obj%A_PointElementID = randi(obj%femdomain%nn())
    obj%B_PointElementID = randi(obj%femdomain%nn())


    if(present(x_len) )then
        call obj%femdomain%resize(x=x_len)
    endif

    if(present(y_len) )then
        call obj%femdomain%resize(y=y_len)
    endif


    obj%thickness = maxval(obj%femdomain%mesh%nodcoord(:,3)) &
    - minval(obj%femdomain%mesh%nodcoord(:,3))
!    ! export data
!    call f%open("theta_r_relation.txt","w")
!    do i=1,size(r_data)
!        call f%write(theta_data(i),r_data(i) )
!    enddo
!    call f%close()
!    call f%plot("theta_r_relation.txt","w l")
!    call f%plot(filename,"w l")
    
end subroutine

! ########################################
    subroutine initLeaf(obj,config,regacy,Thickness,length,width,ShapeFactor,&
        MaxThickness,Maxlength,Maxwidth,rotx,roty,rotz,location,species,SoyWidthRatio,&
        curvature,x_num,y_num,z_num)
        class(leaf_),intent(inout) :: obj
        real(real64),optional,intent(in) :: Thickness,length,width,ShapeFactor
        real(real64),optional,intent(in) :: MaxThickness,Maxlength,Maxwidth
        real(real64),optional,intent(in)::  rotx,roty,rotz,location(3),SoyWidthRatio,curvature
        integer(int32),optional,intent(in) :: species, x_num,y_num,z_num
        logical, optional,intent(in) :: regacy
        character(*),optional,intent(in) :: config
        type(IO_) :: leafconf,f
        character(200) :: fn,conf,line
        integer(int32),allocatable :: buf(:)
        integer(int32) :: id,rmc,n,node_id,node_id2,elemid,blcount,i,j

        integer(int32),allocatable :: killElementList(:),killElementIDs(:)
        real(real64) :: loc(3),radius,z,leaf_L
        logical :: debug=.false.

        obj%minlength =  0.005d0
        obj%minwidth =  0.005d0
        obj%minthickness =  0.0001d0
        obj%maxlength =  0.07d0
        obj%maxwidth =  0.045d0
        obj%maxthickness =  0.001d0
        obj%shaperatio =  0.3d0
        !obj%drydensity =  0.0
        !obj%watercontent =  0.0
        obj%xnum =  10
        obj%ynum =  10
        obj%znum =  20

!        ! 節を生成するためのスクリプトを開く
!        if(.not.present(config) .or. index(config,".json")==0 )then
!            ! デフォルトの設定を生成
!            if(debug) print *, "New leaf-configuration >> leafconfig.json"
!            call leafconf%open("leafconfig.json")
!            write(leafconf%fh,*) '{'
!            write(leafconf%fh,*) '   "type": "leaf",'
!            write(leafconf%fh,*) '   "minlength": 0.005,'
!            write(leafconf%fh,*) '   "minwidth": 0.005,'
!            write(leafconf%fh,*) '   "minthickness": 0.0001,'
!            write(leafconf%fh,*) '   "maxlength": 0.07,'
!            write(leafconf%fh,*) '   "maxwidth": 0.045,'
!            write(leafconf%fh,*) '   "maxthickness": 0.001,'
!            write(leafconf%fh,*) '   "shaperatio": 0.3,'
!            write(leafconf%fh,*) '   "drydensity": 0.0,'
!            write(leafconf%fh,*) '   "watercontent": 0.0,'
!            write(leafconf%fh,*) '   "xnum": 10,'
!            write(leafconf%fh,*) '   "ynum": 10,'
!            write(leafconf%fh,*) '   "znum": 20'
!            write(leafconf%fh,*) '}'
!            conf="leafconfig.json"
!            call leafconf%close()
!        else
!            conf = config
!        endif
        
        if(present(config) )then
            conf = config
            call leafconf%open(conf)
            blcount=0
            do
                read(leafconf%fh,'(a)') line
                if(debug) print *, line
                if( adjustl(line)=="{" )then
                    blcount=1
                    cycle
                endif
                if( adjustl(line)=="}" )then
                    exit
                endif

                if(blcount==1)then

                    if(index(line,"type")/=0 .and. index(line,"leaf")==0 )then
                        print *, "ERROR: This config-file is not for leaf"
                        return
                    endif
                
                    if(index(line,"maxlength")/=0 )then
                        ! 生育ステージ
                        rmc=index(line,",")
                        ! カンマがあれば除く
                        if(rmc /= 0)then
                            line(rmc:rmc)=" "
                        endif
                        id = index(line,":")
                        read(line(id+1:),*) obj%maxlength
                    endif
                
                
                    if(index(line,"maxwidth")/=0 )then
                        ! 種子の長さ
                        rmc=index(line,",")
                        ! カンマがあれば除く
                        if(rmc /= 0)then
                            line(rmc:rmc)=" "
                        endif
                        id = index(line,":")
                        read(line(id+1:),*) obj%maxwidth
                    endif

                    if(index(line,"maxthickness")/=0 )then
                        ! 種子の長さ
                        rmc=index(line,",")
                        ! カンマがあれば除く
                        if(rmc /= 0)then
                            line(rmc:rmc)=" "
                        endif
                        id = index(line,":")
                        read(line(id+1:),*) obj%maxthickness
                    endif
                
                
                    if(index(line,"minlength")/=0 )then
                        ! 生育ステージ
                        rmc=index(line,",")
                        ! カンマがあれば除く
                        if(rmc /= 0)then
                            line(rmc:rmc)=" "
                        endif
                        id = index(line,":")
                        read(line(id+1:),*) obj%minlength
                    endif
                
                    if(index(line,"shaperatio")/=0 )then
                        ! 生育ステージ
                        rmc=index(line,",")
                        ! カンマがあれば除く
                        if(rmc /= 0)then
                            line(rmc:rmc)=" "
                        endif
                        id = index(line,":")
                        read(line(id+1:),*) obj%shaperatio
                    endif
                
                    if(index(line,"minwidth")/=0 )then
                        ! 種子の長さ
                        rmc=index(line,",")
                        ! カンマがあれば除く
                        if(rmc /= 0)then
                            line(rmc:rmc)=" "
                        endif
                        id = index(line,":")
                        read(line(id+1:),*) obj%minwidth
                    endif
                
                    if(index(line,"minthickness")/=0 )then
                        ! 種子の長さ
                        rmc=index(line,",")
                        ! カンマがあれば除く
                        if(rmc /= 0)then
                            line(rmc:rmc)=" "
                        endif
                        id = index(line,":")
                        read(line(id+1:),*) obj%minthickness
                    endif
                
                
                
                    if(index(line,"xnum")/=0 )then
                        ! 種子の長さ
                        rmc=index(line,",")
                        ! カンマがあれば除く
                        if(rmc /= 0)then
                            line(rmc:rmc)=" "
                        endif
                        id = index(line,":")
                        read(line(id+1:),*) obj%xnum
                    endif
                
                
                
                    if(index(line,"ynum")/=0 )then
                        ! 種子の長さ
                        rmc=index(line,",")
                        ! カンマがあれば除く
                        if(rmc /= 0)then
                            line(rmc:rmc)=" "
                        endif
                        id = index(line,":")
                        read(line(id+1:),*) obj%ynum
                    endif
                
                
                
                    if(index(line,"znum")/=0 )then
                        ! 種子の長さ
                        rmc=index(line,",")
                        ! カンマがあれば除く
                        if(rmc /= 0)then
                            line(rmc:rmc)=" "
                        endif
                        id = index(line,":")
                        read(line(id+1:),*) obj%znum
                    endif
                    cycle
                
                endif
            
            enddo
            call leafconf%close()
        endif


        obj%xnum = input(default=obj%xnum,option=x_num)
        obj%ynum = input(default=obj%ynum,option=y_num)
        obj%znum = input(default=obj%znum,option=z_num)
        
        ! グラフ構造とメッシュ構造を生成する。
    
        !
        !           D %%%%%%%%%%%%%%%%%%%%%%%%%%%  B
        !         %%                        %   %
        !        %%                    %      %%  
        !      %%                 %          %%    
        !     %%            %              %%      
        !     %%      %                  %%        
        !     %%                       %%          
        !   A   %%                  %%            
        !      <I> %%%%%%%%%%%%%%%% C                              
    
    
    
        ! メッシュを生成
        call obj%FEMdomain%create(meshtype="rectangular3D",x_num=obj%xnum,y_num=obj%ynum,z_num=obj%znum,&
        x_len=obj%minwidth/2.0d0,y_len=obj%minwidth/2.0d0,z_len=obj%minlength,shaperatio=obj%shaperatio)
        

        ! physical parameters
        allocate(obj%A(size(obj%FEMDomain%Mesh%ElemNod,1) ) )
        obj%A(:) = 0.0d0
        allocate(obj%source(size(obj%FEMDomain%Mesh%ElemNod,1) ) )
        obj%source(:) = 0.0d0
        allocate(obj%ppfd(size(obj%FEMDomain%Mesh%ElemNod,1) ) )
        obj%ppfd(:) = 0.0d0

        ! initialize physical parameter
        obj%DryDensity = zeros( obj%FEMDomain%ne() )
        obj%watercontent = zeros(obj%FEMDomain%ne())
        if(present(config) )then
            obj%DryDensity(:) = freal(leafconf%parse(config,key1="drydensity"))
            obj%watercontent(:) = freal(leafconf%parse(config,key1="watercontent"))
        endif
        ! <I>面に属する要素番号、節点番号、要素座標、節点座標のリストを生成
        obj%I_planeNodeID = obj%FEMdomain%mesh%getNodeList(zmax=0.0d0)
        obj%I_planeElementID = obj%FEMdomain%mesh%getElementList(zmax=0.0d0)
        
        ! <I>面に属する要素番号、節点番号、要素座標、節点座標のリストを生成
        obj%II_planeNodeID = obj%FEMdomain%mesh%getNodeList(zmin=obj%minlength)
        obj%II_planeElementID = obj%FEMdomain%mesh%getElementList(zmin=obj%minlength)
        
        buf   = obj%FEMDomain%mesh%getNodeList(&
            xmin=obj%minwidth/2.0d0 - obj%minwidth/dble(obj%xnum)/2.0d0 ,&
            xmax=obj%minwidth/2.0d0 + obj%minwidth/dble(obj%xnum)/2.0d0 ,&
            ymin=obj%minwidth/2.0d0 - obj%minwidth/dble(obj%ynum)/2.0d0 ,&
            ymax=obj%minwidth/2.0d0 + obj%minwidth/dble(obj%ynum)/2.0d0 ,&
            zmax=0.0d0)
        !obj%A_PointNodeID = buf(1)
        obj%A_PointNodeID = median(buf)
    
        buf   = obj%FEMDomain%mesh%getNodeList(&
            xmin=obj%minwidth/2.0d0 - obj%minwidth/dble(obj%xnum)/2.0d0 ,&
            xmax=obj%minwidth/2.0d0 + obj%minwidth/dble(obj%xnum)/2.0d0 ,&
            ymin=obj%minwidth/2.0d0 - obj%minwidth/dble(obj%ynum)/2.0d0 ,&
            ymax=obj%minwidth/2.0d0 + obj%minwidth/dble(obj%ynum)/2.0d0 ,&
            zmin=obj%minlength)
        !obj%B_PointNodeID = buf(1)
        obj%B_PointNodeID = median(buf)



        buf    = obj%FEMDomain%mesh%getElementList(&
            xmin=obj%minwidth/2.0d0 - obj%minwidth/dble(obj%xnum)/2.0d0 ,&
            xmax=obj%minwidth/2.0d0 + obj%minwidth/dble(obj%xnum)/2.0d0 ,&
            ymin=obj%minwidth/2.0d0 - obj%minwidth/dble(obj%ynum)/2.0d0 ,&
            ymax=obj%minwidth/2.0d0 + obj%minwidth/dble(obj%ynum)/2.0d0 ,&
            zmax=0.0d0)

            
        !obj%A_PointElementID = buf(1)
        obj%A_PointElementID = median(buf)
    
        buf    = obj%FEMDomain%mesh%getElementList(&
            xmin=obj%minwidth/2.0d0 - obj%minwidth/dble(obj%xnum)/2.0d0 ,&
            xmax=obj%minwidth/2.0d0 + obj%minwidth/dble(obj%xnum)/2.0d0 ,&
            ymin=obj%minwidth/2.0d0 - obj%minwidth/dble(obj%ynum)/2.0d0 ,&
            ymax=obj%minwidth/2.0d0 + obj%minwidth/dble(obj%ynum)/2.0d0 ,&
            zmin=obj%minlength)
        !obj%B_PointElementID = buf(1)
        obj%B_PointElementID = median(buf)
    
        !print *, obj%C_PointNodeID
        !print *, obj%D_PointNodeID
        !print *, obj%C_PointElementID
        !print *, obj%D_PointElementID
!
        call obj%FEMdomain%remove()
        if(present(species) )then
            if(species==PF_SOYBEAN_CV)then
                call obj%FEMdomain%create(meshtype="Sphere3D",x_num=obj%xnum,y_num=obj%ynum,z_num=obj%xnum,&
                    x_len=10.0d0,y_len=10.0d0,z_len=10.0d0)
                killElementIDs = obj%FEMdomain%getElementList(ymin= obj%FEMdomain%ymax()*1.0d0/2.0d0)
                killElementList = zeros(obj%FEMdomain%ne() )
                killElementList(killElementIDs(:) ) = 1
                call obj%FEMDomain%killElement(blacklist=killElementList,flag=1)
                
                call obj%resize(x=obj%minwidth/2.0d0,y=obj%minthickness/2.0d0,&
                    z=obj%minlength)
                call obj%FEMdomain%move(z=-obj%FEMdomain%zmin() )
                call obj%FEMdomain%move(x=-(obj%FEMdomain%xmax()+obj%FEMdomain%xmin()) )
                call obj%FEMdomain%move(y=-(obj%FEMdomain%ymax()+obj%FEMdomain%ymin()) )
                
                
            else
                call obj%FEMdomain%create(meshtype="Leaf3D",x_num=obj%xnum,y_num=obj%ynum,z_num=obj%znum,&
                x_len=obj%minwidth/2.0d0,y_len=obj%minthickness/2.0d0,&
                z_len=obj%minlength,species=species,SoyWidthRatio=SoyWidthRatio)
            endif
        else
            call obj%FEMdomain%create(meshtype="Leaf3D",x_num=obj%xnum,y_num=obj%ynum,z_num=obj%znum,&
            x_len=obj%minwidth/2.0d0,y_len=obj%minthickness/2.0d0,z_len=obj%minlength,shaperatio=obj%shaperatio)
        endif

        obj%thickness = maxval(obj%femdomain%mesh%nodcoord(:,2)) &
            - minval(obj%femdomain%mesh%nodcoord(:,2))
        
        
        buf   = obj%FEMDomain%mesh%getNodeList(&
            xmin=maxval(obj%FEMdomain%mesh%nodcoord(:,1) ) &
        )
        !obj%A_PointNodeID = buf(1)
        obj%C_PointNodeID = median(buf)
    
        buf   = obj%FEMDomain%mesh%getNodeList(&
            xmax=minval(obj%FEMdomain%mesh%nodcoord(:,1) ) &
        )    !obj%B_PointNodeID = median(buf)
        obj%D_PointNodeID = median(buf)


        
        buf    = obj%FEMDomain%mesh%getElementList(&
            xmin=maxval(obj%FEMdomain%mesh%nodcoord(:,1) )-obj%minwidth/2.0d0/2.0d0/dble(obj%xnum) &
        )
            
        !obj%A_PointElementID = median(buf)
        obj%C_PointElementID = median(buf)
    
        buf    = obj%FEMDomain%mesh%getElementList(&
            xmax=minval(obj%FEMdomain%mesh%nodcoord(:,1) )+obj%minwidth/2.0d0/2.0d0/dble(obj%xnum) &
        )    
        !obj%B_PointElementID = median(buf)
        obj%D_PointElementID = median(buf)
    
        ! Aについて、要素番号、節点番号、要素座標、節点座標のリストを生成
    


        if( present(regacy))then
            if(regacy .eqv. .true.)then
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
        endif
    endif
    obj%CarbonDiffusionCoefficient = obj%default_CarbonDiffusionCoefficient*ones(obj%femdomain%ne() )
    
    end subroutine 
! ########################################

subroutine curveleaf(obj,curvature)
    ! deform by curvature
    class(leaf_),intent(inout) :: obj
    real(real64),intent(in) :: curvature
    real(real64) :: leaf_L,radius,z
    integer(int32) :: i

    if(curvature < dble(1.0e-5))then
        print *, "Caution >> initLeaf >> curvature is too small < 1.0e-5"
        print *, "Then, ignored."
        return
    endif
    radius = 1.0d0/curvature
    leaf_L = maxval(obj%femdomain%mesh%nodcoord(:,3)) - minval(obj%femdomain%mesh%nodcoord(:,3))
    leaf_L = 0.50d0*leaf_L
    do i=1, obj%femdomain%nn()
        z = obj%femdomain%mesh%nodcoord(i,3)
        obj%femdomain%mesh%nodcoord(i,2) = &
            obj%femdomain%mesh%nodcoord(i,2) &
            - sqrt(radius*radius - leaf_L*leaf_L ) &
            + sqrt(radius*radius - (z - leaf_L)*(z - leaf_L)   )
    enddo

end subroutine
    
    
! ########################################
recursive subroutine rotateleaf(obj,x,y,z,reset)
class(leaf_),intent(inout) :: obj
real(real64),optional,intent(in) :: x,y,z
logical,optional,intent(in) :: reset
real(real64),allocatable :: origin1(:),origin2(:),disp(:)

if(present(reset) )then
    if(reset .eqv. .true.)then
        call obj%femdomain%rotate(-obj%rot_x,-obj%rot_y,-obj%rot_z)
        obj%rot_x = 0.0d0
        obj%rot_y = 0.0d0
        obj%rot_z = 0.0d0
    endif
endif

origin1 = obj%getCoordinate("A")
call obj%femdomain%rotate(x,y,z)
obj%rot_x = obj%rot_x + input(default=0.0d0, option=x)
obj%rot_y = obj%rot_y + input(default=0.0d0, option=y)
obj%rot_z = obj%rot_z + input(default=0.0d0, option=z)
origin2 = obj%getCoordinate("A")
disp = origin1
disp(:) = origin1(:) - origin2(:)
call obj%femdomain%move(x=disp(1),y=disp(2),z=disp(3) )


end subroutine
! ########################################


! ########################################
recursive subroutine moveleaf(obj,x,y,z,reset)
class(leaf_),intent(inout) :: obj
real(real64),optional,intent(in) :: x,y,z
logical,optional,intent(in) :: reset
real(real64),allocatable :: origin1(:),origin2(:),disp(:)

if(present(reset) )then
    if(reset .eqv. .true.)then
        call obj%femdomain%move(-obj%disp_x,-obj%disp_y,-obj%disp_z)
        obj%disp_x = 0.0d0
        obj%disp_y = 0.0d0
        obj%disp_z = 0.0d0
    endif
endif

call obj%femdomain%move(x,y,z)
obj%disp_x = obj%disp_x + input(default=0.0d0, option=x)
obj%disp_y = obj%disp_y + input(default=0.0d0, option=y)
obj%disp_z = obj%disp_z + input(default=0.0d0, option=z)

end subroutine
! ########################################

! ########################################
subroutine connectleafleaf(obj,direct,leaf)
    class(leaf_),intent(inout) :: obj    
    class(leaf_),intent(inout) :: leaf
    character(2),intent(in) :: direct
    real(real64),allocatable :: x1(:),x2(:),disp(:)

    !if(present(Stem) )then
    !    if(direct=="->" .or. direct=="=>")then
    !        ! move obj to connect stem (stem is not moved.)
    !        x1 = leaf%getCoordinate("A")
    !        x2 = stem%getCoordinate("B")
    !        disp = x2 - x1
    !        call leaf%move(x=disp(1),y=disp(2),z=disp(3) )
    !    endif
!
!
    !    if(direct=="<-" .or. direct=="<=")then
    !        ! move obj to connect stem (stem is not moved.)
    !        x1 = stem%getCoordinate("A")
    !        x2 = leaf%getCoordinate("B")
    !        disp = x2 - x1
    !        call stem%move(x=disp(1),y=disp(2),z=disp(3) )
    !    endif
    !    return
    !endif

    if(direct=="->" .or. direct=="=>")then
        ! move obj to connect leaf (leaf is not moved.)
        x1 =  obj%getCoordinate("A")
        x2 = leaf%getCoordinate("B")
        disp = x2 - x1
        call obj%move(x=disp(1),y=disp(2),z=disp(3) )
    endif

    if(direct=="<-" .or. direct=="<=")then
        ! move obj to connect leaf (leaf is not moved.)
        x1 = leaf%getCoordinate("A")
        x2 =  obj%getCoordinate("B")
        disp = x2 - x1
        call leaf%move(x=disp(1),y=disp(2),z=disp(3) )
    endif

end subroutine
! ########################################

! ########################################
subroutine connectLeafStem(obj,direct,Stem)
    class(leaf_),intent(inout) :: obj    
    class(Stem_),intent(inout) :: stem
    character(2),intent(in) :: direct
    real(real64),allocatable :: x1(:),x2(:),disp(:)


    if(direct=="->" .or. direct=="=>")then
        ! move obj to connect stem (stem is not moved.)
        x1 = obj%getCoordinate("A")
        x2 = stem%getCoordinate("B")
        disp = x2 - x1
        call obj%move(x=disp(1),y=disp(2),z=disp(3) )
    endif
    
    if(direct=="<-" .or. direct=="<=")then
        ! move obj to connect stem (stem is not moved.)
        x1 = stem%getCoordinate("A")
        x2 = obj%getCoordinate("B")
        disp = x2 - x1
        call stem%move(x=disp(1),y=disp(2),z=disp(3) )
    endif
end subroutine
! ########################################



! ########################################
function getCoordinateleaf(obj,nodetype) result(ret)
class(leaf_),intent(inout) :: obj
character(*),intent(in) :: nodetype
real(real64),allocatable :: ret(:)
integer(int32) :: dimnum,n,i

dimnum = size(obj%femdomain%mesh%nodcoord,2)

allocate(ret(dimnum) )
ret(:) = 0.0d0

if( nodetype=="A" .or. nodetype=="a")then
    n = size(obj%I_planeNodeID )
    do i=1,n
        ret(:) = ret(:) + obj%femdomain%mesh%nodcoord( obj%I_planeNodeID(i),: ) 
    enddo
    ret(:) = 1.0d0/dble(n) * ret(:)

    !ret = obj%femdomain%mesh%nodcoord(obj%A_PointNodeID,:)
endif
if( nodetype=="B" .or. nodetype=="B")then
    !ret = obj%femdomain%mesh%nodcoord(obj%B_PointNodeID,:)
    n = size(obj%II_planeNodeID )
    do i=1,n
        ret(:) = ret(:) + obj%femdomain%mesh%nodcoord( obj%II_planeNodeID(i),: ) 
    enddo
    ret(:) = 1.0d0/dble(n) * ret(:)
endif

end function
! ########################################



! ########################################
subroutine gmshleaf(obj,name)
    class(leaf_),intent(inout) :: obj
    character(*),intent(in) ::name
    if(obj%femdomain%mesh%empty() )then
        return
    endif
    
    call obj%femdomain%gmsh(Name=name)
    ! PPFD を出力
    call obj%femdomain%gmsh(Name=name//"_PPFD_",field=obj%PPFD)
    ! ソース量 を出力
    call obj%femdomain%gmsh(Name=name//"_SOURCE_",field=obj%source)
    ! 光合成速度 を出力
    call obj%femdomain%gmsh(Name=name//"_A_",field=obj%A)


end subroutine
! ########################################

! ########################################
subroutine mshleaf(obj,name)
    class(leaf_),intent(inout) :: obj
    character(*),intent(in) ::name
    if(obj%femdomain%mesh%empty() )then
        return
    endif
    
    call obj%femdomain%msh(Name=name)
    ! PPFD を出力
    !call obj%femdomain%msh(Name=name//"_PPFD_",field=obj%PPFD)
    ! ソース量 を出力
    !call obj%femdomain%msh(Name=name//"_SOURCE_",field=obj%source)
    ! 光合成速度 を出力
    !call obj%femdomain%msh(Name=name//"_A_",field=obj%A)


end subroutine
! ########################################


! ########################################
subroutine vtkleaf(obj,name,field_name)
    class(leaf_),intent(inout) :: obj
    character(*),intent(in) ::name
    character(*),optional,intent(in) ::field_name
    if(obj%femdomain%mesh%empty() )then
        return
    endif
    
    call obj%femdomain%vtk(Name=name,field=field_name)
    ! PPFD を出力
    !call obj%femdomain%msh(Name=name//"_PPFD_",field=obj%PPFD)
    ! ソース量 を出力
    !call obj%femdomain%msh(Name=name//"_SOURCE_",field=obj%source)
    ! 光合成速度 を出力
    !call obj%femdomain%msh(Name=name//"_A_",field=obj%A)


end subroutine
! ########################################


! ########################################
subroutine stlleaf(obj,name)
    class(leaf_),intent(inout) :: obj
    character(*),intent(in) ::name
    if(obj%femdomain%mesh%empty() )then
        return
    endif
    
    call obj%femdomain%stl(Name=name)
    ! PPFD を出力
    !call obj%femdomain%msh(Name=name//"_PPFD_",field=obj%PPFD)
    ! ソース量 を出力
    !call obj%femdomain%msh(Name=name//"_SOURCE_",field=obj%source)
    ! 光合成速度 を出力
    !call obj%femdomain%msh(Name=name//"_A_",field=obj%A)


end subroutine
! ########################################

! ########################################
subroutine resizeleaf(obj,x,y,z,Length,Width)
    class(Leaf_),optional,intent(inout) :: obj
    real(real64),optional,intent(in) :: x,y,z,Length,Width
    real(real64),allocatable :: origin1(:), origin2(:),disp(:),total_rot(:)

    if(present(Length).or. present(Width) )then

        total_rot = zeros(3)
        total_rot(1) = obj%rot_x
        total_rot(2) = obj%rot_y
        total_rot(3) = obj%rot_z
        call obj%rotate(z=-total_rot(3) )
        call obj%rotate(y=-total_rot(2) )
        call obj%rotate(x=-total_rot(1) )

        if(present(Length) )then
            call obj%femdomain%resize(z=Length)
        endif

        if(present(Width) )then
            call obj%femdomain%resize(x=Width)
        endif

        call obj%rotate(x=total_rot(1) )
        call obj%rotate(y=total_rot(2) )
        call obj%rotate(z=total_rot(3) )

    else
        origin1 = obj%getCoordinate("A")
        call obj%femdomain%resize(x_len=x,y_len=y,z_len=z)
        origin2 = obj%getCoordinate("A")
        disp = origin1 - origin2
        call obj%move(x=disp(1),y=disp(2),z=disp(3) )
    endif
end subroutine
! ########################################


! ########################################
function getLengthLeaf(obj) result(Length)
    class(Leaf_),optional,intent(inout) :: obj
    real(real64) :: Length
    real(real64),allocatable :: origin1(:), origin2(:),disp(:),total_rot(:)


    total_rot = zeros(3)
    total_rot(1) = obj%rot_x
    total_rot(2) = obj%rot_y
    total_rot(3) = obj%rot_z
    call obj%rotate(z=-total_rot(3) )
    call obj%rotate(y=-total_rot(2) )
    call obj%rotate(x=-total_rot(1) )

    Length = obj%femdomain%zmax() - obj%femdomain%zmin()
    
    call obj%rotate(x=total_rot(1) )
    call obj%rotate(y=total_rot(2) )
    call obj%rotate(z=total_rot(3) )


end function
! ########################################

! ########################################
function FullyExpandedLeaf(obj,threshold) result(ret_expanded)
    class(Leaf_),optional,intent(inout) :: obj
    real(real64),intent(in) :: threshold
    logical :: ret_expanded
    real(real64) :: length, full_length

    if(obj%getLength()/obj%final_length > threshold)then
        ret_expanded = .true.
    else
        ret_expanded = .false.
    endif

end function
! ########################################



! ########################################
subroutine rescaleleaf(obj,x,y,z)
    class(Leaf_),optional,intent(inout) :: obj
    real(real64),optional,intent(in) :: x,y,z
    real(real64),allocatable :: origin1(:), origin2(:),disp(:)

    origin1 = obj%getCoordinate("A")
    call obj%femdomain%resize(x_rate=x,y_rate=y,z_rate=z)
    origin2 = obj%getCoordinate("A")
    disp = origin1 - origin2
    call obj%move(x=disp(1),y=disp(2),z=disp(3) )
end subroutine
! ########################################

! ########################################
!subroutine LayTracingLeaf(obj,maxPPFD,light,)
!    class(Leaf_),intent(inout) :: obj
!    class(Light_),intent(in) :: light
!    real(real64),intent(in) :: maxPPFD
!    integer(int32) :: i,j,n,m,node_id
!    real(real64) :: lx(3)
!    real(real64),allocatable :: Elem_x(:,:)
!    ! PPFDを計算する。
!    ! Photosynthetic photon flux density (PPFD)
!    ! micro-mol/m^2/s
!
!    ! 反射、屈折は無視、直線のみ
!
!    n=size(obj%FEMDomain%Mesh%ElemNod,2)
!    m=size(obj%FEMDomain%Mesh%NodCoord,2)
!
!    allocate(Elem_x(n,m) )
!    ! 要素ごと
!    do i=1, size(obj%FEMDomain%Mesh%ElemNod,1)
!        do j=1,size(obj%FEMDomain%Mesh%ElemNod,2)
!            node_id = obj%FEMDomain%Mesh%ElemNod(i,j)
!            Elem_x(j,:) = obj%FEMDomain%Mesh%NodCoord(node_id,:)
!        enddo
!        ! 要素座標 >> Elem_x(:,:)
!        ! 光源座標 >> lx(:)
!
!    enddo
!
!
!
!end subroutine
! ########################################

! ########################################
subroutine photosynthesisLeaf(obj,dt,air)

    ! https://eprints.lib.hokudai.ac.jp/dspace/bitstream/2115/39102/1/67-013.pdf

    class(Leaf_),intent(inout) :: obj
    type(Air_),intent(in) :: air
    type(IO_) :: f
    real(real64),intent(in) :: dt
    ! Farquhar modelのパラメータ
    real(real64) :: A   ! CO2吸収速度
    real(real64) :: V_c ! カルボキシル化反応速度
    real(real64) :: V_o ! 酸素化反応速度

    real(real64) :: W_c! RuBPが飽和している場合のCO2吸収速度
    real(real64) :: W_j! RuBP供給が律速している場合のCO2吸収速度

    real(real64) :: V_cmax ! 最大カルボキシル化反応速度
    real(real64) :: V_omax ! 最大酸素化反応速度
    real(real64) :: O2 ! 酸素濃度
    real(real64) :: CO2 ! 二酸化炭素濃度
    real(real64) :: R_d ! なんだっけ

    real(real64) :: K_c ! CO2に対するミカエリス定数
    real(real64) :: K_o ! O2に対するミカエリス定数

    real(real64) :: J_ ! 電子伝達速度
    real(real64) :: I_ ! 光強度
    real(real64) :: phi ! I-J曲線の初期勾配
    real(real64) :: J_max !最大電子伝達速度
    real(real64) :: theta_r ! 曲線の凸度

    real(real64) :: pfd

    real(real64) :: Lambda, volume,gs,Ci,Ca


    integer(int32) :: i, element_id

    obj%temp=air%temp
    obj%CO2 = air%CO2
    obj%O2 = air%O2

    ! TT-model
    do i=1,size(obj%source)
        ! 要素ごとに電子伝達速度を求める
        element_id = i
        pfd = obj%ppfd(element_id)
        obj%J_ = 0.240d0*pfd/(sqrt(1.0d0 + (0.240d0*0.240d0)*pfd*pfd)/obj%J_max/obj%J_max)
        
        ! lambdaからV_omaxを推定
        obj%V_omax = obj%Lambda*( 2.0d0 * obj%V_cmax*obj%K_o )/(obj%K_c*O2)

        ! CO2固定速度の計算
        V_c = (obj%V_cmax*obj%CO2)/(obj%CO2 +obj% K_o * (1.0d0+ obj%O2/obj%K_o) )
        V_o = (obj%V_omax*obj%O2 )/(obj%O2 + obj%K_o * (1.0d0 + obj%CO2/obj%K_c) )

        ! RuBPが飽和している場合のCO2吸収速度
        Ca  = obj%CO2

        W_c = (obj%V_cmax*(Ca - obj%Lambda))/(Ca + obj%K_c*(1.0d0 + obj%O2/obj%K_o))

        ! RuBP供給が律速している場合のCO2吸収速度
        W_j = obj%J_ * (Ca - obj%Lambda)/(4.0d0 * Ca + 8.0d0 * obj%Lambda ) - obj%R_d


        if(W_j >= W_c )then
            A = W_c
        else
            A = W_j
        endif

        !A = gs(Ca - Ci)
        !A = gs*Ca - gs*Ci
        !A = gs*Ca - gs*Ci
        !gs_max = 0.6
        

        ! 要素体積を求める, m^3
        obj%A(element_id) = A
        volume = obj%femdomain%getVolume(elem=element_id)

        ! CO2拡散が考慮されていない，表皮がない．コンダクタンス∞
        ! コンダクタンスを光で．
        ! 水分の関数をあとでいれる

        !CO2固定量　mincro-mol/m-2/s
        ! ここ、体積あたりにする必要がある
        ! 一応、通常の葉の厚さを2mmとして、
        ! 1 micro-mol/m^2/sを、 1 micro-mol/ 0.0002m^3/s= 5000micro-mol/m^3/sとして計算
        ! また、ソース量はC6H12O6の質量gramとして換算する。
        ! CO2の分子量44.01g/mol
        ! C6H12O6の分子量180.16g/mol
        ! 6CO2 + 12H2O => C6H12O6 + 6H2O + 6O2
        ! よって、生成されるソース量は
        !               {CO2固定量,mol     }× {1/6 してグルコースmol}×グルコース分子量
        obj%source(i) =obj%source(i)+ A*dt*5000.0d0*volume * 1.0d0/6.0d0 * 180.160d0
        
    enddo
!    ! For each elements, estimate photosynthesis by Farquhar model
!    do i=1,size(obj%source)
!
!        ! 光合成量の計算
!        ! Farquhar model
!        V_c = (V_cmax*CO2)/(CO2 + K_o * (1.0d0 + O2/K_o) )
!        V_o = (V_omax*O2 )/(O2 + K_o * (1.0d0 + CO2/K_c) )
!        
!        Lambda = (V_omax*K_c*O2)/( 2.0d0 * V_cmax*K_o )
!    
!        W_c = (V_cmax*(CO2 - Lambda))/(CO2 + K_c*(1.0d0 + O2/K_o)  )
!    
!        J_ = (phi*I_ + J_max - &
!        sqrt( (phi*I_ + J_max)**(2.0d0) - 4.0d0*phi*I_*theta_r*J_max)&
!        /(2.0d0 * theta_r) )
!        W_j = J_ * (CO2 - Lambda)/(4.0d0 * CO2 + 8.0d0 * Lambda ) - R_d
!        ! CO2吸収速度
!        A = V_c + 0.50d0*V_o - R_d
!    
!        if(W_j >= W_c )then
!            A = W_c
!        else
!            A = W_j
!        endif
!        
!
!    enddo
!
!
end subroutine

! ####################################################################

! ########################################
function getPhotosynthesisSpeedPerVolumeLeaf(obj,dt,air) result(Speed_PV)

    ! https://eprints.lib.hokudai.ac.jp/dspace/bitstream/2115/39102/1/67-013.pdf

    class(Leaf_),intent(inout) :: obj
    type(Air_),intent(in) :: air
    type(IO_) :: f
    real(real64),intent(in) :: dt
    ! Farquhar modelのパラメータ
    real(real64) :: A   ! CO2吸収速度
    real(real64) :: V_c ! カルボキシル化反応速度
    real(real64) :: V_o ! 酸素化反応速度

    real(real64) :: W_c! RuBPが飽和している場合のCO2吸収速度
    real(real64) :: W_j! RuBP供給が律速している場合のCO2吸収速度

    real(real64) :: V_cmax ! 最大カルボキシル化反応速度
    real(real64) :: V_omax ! 最大酸素化反応速度
    real(real64) :: O2 ! 酸素濃度
    real(real64) :: CO2 ! 二酸化炭素濃度
    real(real64) :: R_d ! なんだっけ

    real(real64) :: K_c ! CO2に対するミカエリス定数
    real(real64) :: K_o ! O2に対するミカエリス定数

    real(real64) :: J_ ! 電子伝達速度
    real(real64) :: I_ ! 光強度
    real(real64) :: phi ! I-J曲線の初期勾配
    real(real64) :: J_max !最大電子伝達速度
    real(real64) :: theta_r ! 曲線の凸度

    real(real64) :: pfd

    real(real64) :: Lambda, volume
    real(real128) :: buf
    real(real64),allocatable :: Speed_PV(:)

    integer(int32) :: i, element_id

    obj%temp=air%temp
    obj%CO2 = air%CO2
    obj%O2 = air%O2

    Speed_PV = zeros(obj%femdomain%ne() )
    ! TT-model
    do i=1,size(Speed_PV)
        ! 要素ごとに電子伝達速度を求める
        element_id = i
        pfd = obj%ppfd(element_id)
        obj%J_ = 0.240d0*pfd/(sqrt(1.0d0 + (0.240d0*0.240d0)*pfd*pfd)/obj%J_max/obj%J_max)
        
        ! lambdaからV_omaxを推定
        !obj%V_omax = obj%Lambda* 2.0d0 * obj%V_cmax*obj%K_o/(obj%K_c*O2) 
        buf = obj%Lambda* 2.0d0 * obj%V_cmax*obj%K_o
        obj%V_omax = dble(buf/obj%K_c/obj%O2) 

        ! CO2固定速度の計算
        V_c = (obj%V_cmax*obj%CO2)/(obj%CO2 +obj% K_o * (1.0d0+ obj%O2/obj%K_o) )
        V_o = (obj%V_omax*obj%O2 )/(obj%O2 + obj%K_o * (1.0d0 + obj%CO2/obj%K_c) )

        ! RuBPが飽和している場合のCO2吸収速度
        W_c = (obj%V_cmax*(obj%CO2 - obj%Lambda))/(obj%CO2 + obj%K_c*(1.0d0 + obj%O2/obj%K_o))

        ! RuBP供給が律速している場合のCO2吸収速度
        W_j = obj%J_ * (obj%CO2 - obj%Lambda)/(4.0d0 * obj%CO2 + 8.0d0 * obj%Lambda ) - obj%R_d


        if(W_j >= W_c )then
            A = W_c
        else
            A = W_j
        endif
        ! 要素体積を求める, m^3
        obj%A(element_id) = A
        volume = obj%femdomain%getVolume(elem=element_id)

        !CO2固定量　mincro-mol/m^2/s
        ! ここ、体積あたりにする必要がある
        ! 一応、通常の葉の厚さを0.2mmとして、
        ! 1 micro-mol/m^2/sを、 1 micro-mol/ 0.002m^3/s= 500micro-mol/m^3/sとして計算
        ! また、ソース量はC6H12O6の質量gramとして換算する。
        ! CO2の分子量44.01g/mol
        ! C6H12O6の分子量180.16g/mol
        ! 6CO2 + 12H2O => C6H12O6 + 6H2O + 6O2
        ! よって、生成されるソース量(micro-gram)は
        !               {CO2固定量,mol     }× {1/6 してグルコースmol}×グルコース分子量
        Speed_PV(i) =A*dt*5000.0d0 * 1.0d0/6.0d0 * 180.160d0
        
    enddo
end function

! ####################################################################
subroutine adjustLeaf(obj,width)
    class(Leaf_),intent(inout) :: obj
    real(real64),intent(in) :: width(:,:)


end subroutine



function getVolumeLeaf(obj) result(ret)
    class(Leaf_),intent(in) :: obj
    real(real64) :: ret
    integer(int32) :: i,j
    
    ret =0.0d0

    if(obj%femdomain%mesh%empty() ) then
        return
    endif
    
    do i=1,obj%femdomain%ne()
        ret = ret + obj%femdomain%getVolume(elem=i)
    enddo

end function
! ########################################

! ########################################
function getBiomassLeaf(obj) result(ret)
    class(Leaf_),intent(in) :: obj
    real(real64) :: ret
    integer(int32) :: i,j

    ret =0.0d0

    if(obj%femdomain%mesh%empty() ) then
        return
    endif
    

    do i=1,obj%femdomain%ne()
        ret = ret + obj%femdomain%getVolume(elem=i)*obj%drydensity(i)
    enddo

end function
! ########################################


! ########################################
function emptyLeaf(obj) result(leaf_is_empty)
    class(Leaf_),intent(in) :: obj
    logical :: leaf_is_empty

    leaf_is_empty = obj%femdomain%empty()

end function
! ########################################

function getLeafAreaLeaf(obj) result(LeafArea)
    class(Leaf_),intent(in) :: obj
    real(real64) :: LeafArea

    LeafArea = obj%getVolume()/obj%thickness    

end function
! ########################################


! ################################################################
pure function getRadiusLeaf(obj)  result(radius)
    class(Leaf_),intent(in) :: obj
    real(real64),allocatable :: Points(:,:)
    real(real64) :: radius,center(3)
    
    Points = obj%femdomain%mesh%nodcoord

    ! search Intersect leaf
    center(1) = sum(Points(:,1) )/dble(size(Points,1))
    center(2) = sum(Points(:,2) )/dble(size(Points,1))
    center(3) = sum(Points(:,3) )/dble(size(Points,1))

    Points(:,1) = Points(:,1) - center(1)
    Points(:,2) = Points(:,2) - center(2)

    radius = maxval(Points(:,1)*Points(:,1) +Points(:,2)*Points(:,2)   )
    radius = sqrt(radius)

end function
! ################################################################

! ################################################################
pure function getCenterLeaf(obj)  result(Center)
    class(Leaf_),intent(in) :: obj
    real(real64),allocatable :: Points(:,:)
    real(real64) :: center(3)
    
    Points = obj%femdomain%mesh%nodcoord

    ! search Intersect leaf
    center(1) = sum(Points(:,1) )/dble(size(Points,1))
    center(2) = sum(Points(:,2) )/dble(size(Points,1))
    center(3) = sum(Points(:,3) )/dble(size(Points,1))

end function
! ################################################################


subroutine syncleaf(obj,from, mpid)
    class(Leaf_),intent(inout) :: obj
    integer(int32),intent(in) :: from
    type(MPI_),intent(inout) :: mpid

    call obj%FEMDomain%sync(from=from, mpid=mpid)
    call mpid%bcast(from=from,val=obj%  LeafSurfaceNode2D)
    call mpid%bcast(from=from,val=obj% ShapeFactor) !
    call mpid%bcast(from=from,val=obj% Thickness) !
    call mpid%bcast(from=from,val=obj% length) !
    call mpid%bcast(from=from,val=obj% width) !
    call mpid%BcastMPIRealVecFixedSize(from=from,val=obj% center ) !(3)
    call mpid%bcast(from=from,val=obj% MaxThickness) !
    call mpid%bcast(from=from,val=obj% Maxlength) !
    call mpid%bcast(from=from,val=obj% Maxwidth) !
    call mpid%BcastMPIRealVecFixedSize(from=from,val=obj% center_bottom) !(3)
    call mpid%BcastMPIRealVecFixedSize(from=from,val=obj% center_top) !(3)
    call mpid%BcastMPIRealVecFixedSize(from=from,val=obj% outer_normal_bottom) !(3)
    call mpid%BcastMPIRealVecFixedSize(from=from,val=obj% outer_normal_top) !(3)
    call mpid%bcast(from=from,val=obj%  source)
    call mpid%bcast(from=from,val=obj%  ppfd)
    call mpid%bcast(from=from,val=obj%  A)
    call mpid%bcast(from=from,val=obj%Division)
    !type(leaf_),pointer ::  pleaf
    !type(Peti_),pointer ::  pPeti
    call mpid%bcast(from=from,val=obj%  rot_x) ! = 0.0d0
    call mpid%bcast(from=from,val=obj%  rot_y) ! = 0.0d0
    call mpid%bcast(from=from,val=obj%  rot_z) ! = 0.0d0
    call mpid%bcast(from=from,val=obj%  disp_x) ! = 0.0d0
    call mpid%bcast(from=from,val=obj%  disp_y) ! = 0.0d0
    call mpid%bcast(from=from,val=obj%  disp_z) ! = 0.0d0
    call mpid%bcast(from=from,val=obj%  shaperatio) ! = 0.30d0
    call mpid%bcast(from=from,val=obj%  minwidth) !
    call mpid%bcast(from=from,val=obj% minlength) !
    call mpid%bcast(from=from,val=obj% MinThickness) !

    call mpid%bcast(from=from,val=obj%I_planeNodeID) !(:)
    call mpid%bcast(from=from,val=obj%I_planeElementID) !(:)
    call mpid%bcast(from=from,val=obj%II_planeNodeID) !(:)
    call mpid%bcast(from=from,val=obj%II_planeElementID) !(:)
    call mpid%bcast(from=from,val=obj%A_PointNodeID)!
    call mpid%bcast(from=from,val=obj%B_PointNodeID)!
    call mpid%bcast(from=from,val=obj%A_PointElementID)!
    call mpid%bcast(from=from,val=obj%B_PointElementID)!
    call mpid%bcast(from=from,val=obj%xnum)! = 10
    call mpid%bcast(from=from,val=obj%ynum)! = 10
    call mpid%bcast(from=from,val=obj%znum)! = 10

    ! phisiological parameters


    call mpid%bcast(from=from,val=obj% V_cmax) ! = 100.0d0 ! 最大カルボキシル化反応速度, mincro-mol/m-2/s
    call mpid%bcast(from=from,val=obj% V_omax) ! = 100.0d0 ! 最大酸素化反応速度, mincro-mol/m-2/s, lambdaから推定
    call mpid%bcast(from=from,val=obj% O2) ! = 380.0d0! 酸素濃度, ppm
    call mpid%bcast(from=from,val=obj% CO2) !=202000.0d0! 二酸化炭素濃度, ppm
    call mpid%bcast(from=from,val=obj% R_d) !=1.0d0 ! 暗呼吸速度, mincro-mol/m-2/s

    call mpid%bcast(from=from,val=obj% K_c) !=272.380d0 ! CO2に対するミカエリス定数
    call mpid%bcast(from=from,val=obj% K_o) !=165820.0d0 ! O2に対するミカエリス定数

    call mpid%bcast(from=from,val=obj% J_) !=0.0d0 ! 電子伝達速度
    call mpid%bcast(from=from,val=obj% I_) !=0.0d0 ! 光強度
    call mpid%bcast(from=from,val=obj% phi) !=0.0d0 ! I-J曲線の初期勾配
    call mpid%bcast(from=from,val=obj% J_max) !=180.0d0 !最大電子伝達速度,mincro-mol/m-2/s
    call mpid%bcast(from=from,val=obj% theta_r) !=0.0d0 ! 曲線の凸度

    call mpid%bcast(from=from,val=obj% maxPPFD) !=1.0d0 ! micro-mol/m^2/s

    call mpid%bcast(from=from,val=obj% Lambda) != 37.430d0 ! 暗呼吸速度を無視した時のCO2補償点ppm
    call mpid%bcast(from=from,val=obj% temp) !=303.0d0 ! temp


    ! physical parameter
    call mpid%bcast(from=from,val=obj% DryDensity)
    call mpid%bcast(from=from,val=obj% WaterContent)

    ! For deformation analysis
    call mpid%bcast(from=from,val=obj% YoungModulus)
    
    call mpid%bcast(from=from,val=obj% PoissonRatio)
    call mpid%bcast(from=from,val=obj% CarbonDiffusionCoefficient)
    call mpid%bcast(from=from,val=obj% Density)
    call mpid%bcast(from=from,val=obj% Stress)
    call mpid%bcast(from=from,val=obj% Displacement)
    

    call mpid%bcast(from=from,val=obj% BoundaryTractionForce)
    call mpid%bcast(from=from,val=obj% BoundaryDisplacement)
    

end subroutine


subroutine syncLeafVector(obj,from,mpid)
	type(Leaf_),allocatable,intent(inout) :: obj(:)
	integer(int32),intent(in) :: from
	type(MPI_),intent(inout) :: mpid
	integer(int32) :: vec_size, i

	vec_size=0
	if(mpid%myrank==from)then
		if(.not.allocated(obj) )then
			vec_size = -1
        else
            vec_size = size(obj)
		endif
	endif
	call mpid%bcast(from=from,val=vec_size)
	if(vec_size<1)then
		return
	endif

	if(from /= mpid%myrank)then
		if(allocated(obj) )then
			deallocate(obj)
		endif
		allocate(obj(vec_size) )
	endif

	do i=1,vec_size
		call obj(i)%sync(from=from, mpid=mpid)
	enddo

end subroutine
! ##################################################################
function getNormalVectorLeaf(obj,ElementID) result(ret)
    class(Leaf_),intent(inout) :: obj
    integer(int32),intent(in) :: ElementID
    real(real64),allocatable :: ret(:),x_A(:),x_B(:),x_C(:),x_D(:),x(:),&
        n_AC(:),n_CB(:),n_BD(:),n_DA(:)
    real(real64) :: min_norm
    integer(int32) :: num_effective_norms,nd

    nd = obj%femdomain%nd()
    allocate(ret(nd) )
    x_A = obj%femdomain%centerPosition(ElementID=obj%A_PointElementID)
    x_B = obj%femdomain%centerPosition(ElementID=obj%B_PointElementID)
    x_C = obj%femdomain%centerPosition(ElementID=obj%C_PointElementID)
    x_D = obj%femdomain%centerPosition(ElementID=obj%D_PointElementID)
    x   = obj%femdomain%centerPosition(ElementID=ElementID)
    
    n_AC = cross_product(x_C -x_A,x - x_C )
    n_CB = cross_product(x_B -x_C,x - x_B )
    n_BD = cross_product(x_D -x_B,x - x_D )
    n_DA = cross_product(x_A -x_D,x - x_A )
    
    num_effective_norms = 0
    min_norm = dble(1.0e-6)
    if(norm(n_AC)< min_norm)then
        n_AC = zeros(nd)
    else 
        num_effective_norms = num_effective_norms + 1
        n_AC = n_AC(:)/norm(n_AC)   
    endif

    if(norm(n_CB)< min_norm)then
        n_CB = zeros(nd)
    else 
        num_effective_norms = num_effective_norms + 1
        n_CB = n_CB(:)/norm(n_CB)   
    endif

    if(norm(n_BD)< min_norm)then
        n_BD = zeros(nd)
    else 
        num_effective_norms = num_effective_norms + 1
        n_BD = n_BD(:)/norm(n_BD)   
    endif

    if(norm(n_DA)< dble(1.0e-6))then
        n_DA = zeros(nd)
    else 
        num_effective_norms = num_effective_norms + 1
        n_DA = n_DA(:)/norm(n_DA)   
    endif

    if(num_effective_norms==0)then
        print *, "ERROR :: getNormalVectorLeaf >> no valid normal vector is found"
        stop 
    endif
    ret = (n_AC + n_CB + n_BD + n_DA)/dble(num_effective_norms)
    ret = ret/norm(ret)*(-1.0d0)
    ! get outer nomal of the element 


    
        !
        !           D %%%%%%%%%%%%%%%%%%%%%%%%%%%  B
        !         %%                        %   %
        !        %%                    %      %%  
        !      %%                 %          %%    
        !     %%            %              %%      
        !     %%      %                  %%        
        !     %%                       %%          
        !   A   %%                  %%            
        !      <I> %%%%%%%%%%%%%%%% C    


end function
! #################################################################
subroutine growLeaf(this,dt)
    class(Leaf_),intent(inout)::this
    real(real64),intent(in) :: dt
    real(real64) :: Length,Width


    if(this%already_grown)then
        ! ignore growth for this
        return
    endif

    if(this%femdomain%empty() ) then
        return
    endif

    ! logistic curve
    ! automatic growth
    this%my_time = this%my_time + dt
    ! growth curve: logistic function
    Length = this%final_length &
        /(1.0d0 +&
            (this%final_length/this%initial_length - 1.0d0)&
                *exp(-this%length_growth_ratio*this%my_time) )

    Width = this%final_Width &
        /(1.0d0 + &
            (this%final_Width/this%initial_Width - 1.0d0)&
                *exp(-this%Width_growth_ratio*this%my_time) )
    
    call this%resize(Length=Length,Width=Width)
    return

end subroutine

! ###############################################################
subroutine removeLeaf(this)
    class(Leaf_),intent(inout) :: this

    call this%FEMDomain%remove()
        if(allocated(this%  LeafSurfaceNode2D)) deallocate( this%  LeafSurfaceNode2D)
        this%ShapeFactor = 0.0d0
        this%Thickness = 0.0d0
        this%length = 0.0d0
        this%width = 0.0d0
        this%center = 0.0d0
        this%MaxThickness = 0.0d0
        this%Maxlength = 0.0d0
        this%Maxwidth = 0.0d0
        this%center_bottom = 0.0d0
        this%center_top = 0.0d0
        this%outer_normal_bottom = 0.0d0
        this%outer_normal_top = 0.0d0
        if(allocated(this%  source)) deallocate( this%  source)

        this % Division = 0
        if(associated(this%pleaf)) nullify(this%pleaf)
        if(associated(this%pPeti)) nullify(this%pPeti)

        this % rot_x = 0.0d0
        this % rot_y = 0.0d0
        this % rot_z = 0.0d0
        this % disp_x = 0.0d0
        this % disp_y = 0.0d0
        this % disp_z = 0.0d0
        this % shaperatio = 0.30d0
        this % minwidth = 0.0d0
        this % minlength = 0.0d0
        this % MinThickness = 0.0d0

        ! id in multi-leaf
        this % LeafID = -1
        this % already_grown = .false.

        if(allocated(this% I_planeNodeID)) deallocate( this% I_planeNodeID)
        if(allocated(this% I_planeElementID)) deallocate( this% I_planeElementID)
        if(allocated(this% II_planeNodeID)) deallocate( this% II_planeNodeID)
        if(allocated(this% II_planeElementID)) deallocate( this% II_planeElementID)
        this % A_PointNodeID = 0
        this % B_PointNodeID = 0
        this % C_PointNodeID = 0
        this % D_PointNodeID = 0
        this % A_PointElementID = 0
        this % B_PointElementID = 0
        this % C_PointElementID = 0
        this % D_PointElementID = 0
        this % xnum = 10
        this % ynum = 10
        this % znum = 10

        ! phisiological parameters


        this % V_cmax = 100.0d0 ! 最大カルボキシル化反応速度, mincro-mol/m-2/s
        this % V_omax = 100.0d0 ! 最大酸素化反応速度, mincro-mol/m-2/s, lambdaから推定
        this % O2 = 380.0d0! 酸素濃度, ppm
        this % CO2=202000.0d0! 二酸化炭素濃度, ppm
        this % R_d=1.0d0 ! 暗呼吸速度, mincro-mol/m-2/s
    
        this % K_c=272.380d0 ! CO2に対するミカエリス定数
        this % K_o=165820.0d0 ! O2に対するミカエリス定数
    
        this % J_=0.0d0 ! 電子伝達速度
        this % I_=0.0d0 ! 光強度
        this % phi=0.0d0 ! I-J曲線の初期勾配
        this % J_max=180.0d0 !最大電子伝達速度,mincro-mol/m-2/s
        this % theta_r=0.0d0 ! 曲線の凸度
    
        this % maxPPFD=1.0d0 ! micro-mol/m^2/s
    
        this % Lambda= 37.430d0 ! 暗呼吸速度を無視した時のCO2補償点ppm
        this % temp=303.0d0 ! temp


        ! physical parameter
        if(allocated(this% DryDensity)) deallocate( this% DryDensity)
        if(allocated(this% WaterContent)) deallocate( this% WaterContent)

        ! For deformation analysis
        if(allocated(this% YoungModulus)) deallocate( this% YoungModulus)
        if(allocated(this% PoissonRatio)) deallocate( this% PoissonRatio)
        if(allocated(this% Density)) deallocate( this% Density)
        if(allocated(this% CarbonDiffusionCoefficient)) deallocate( this% CarbonDiffusionCoefficient)
        if(allocated(this% Stress)) deallocate( this% Stress)
        if(allocated(this% Displacement)) deallocate( this% Displacement)
        

        if(allocated(this% BoundaryTractionForce)) deallocate( this% BoundaryTractionForce)
        if(allocated(this% BoundaryDisplacement)) deallocate( this% BoundaryDisplacement)
        
        ! growth parameters
        this % my_time = 0.0d0
        this % initial_width  = 0.0010d0 ! 1.0 mm
        this % initial_length = 0.0010d0 ! 1.0 mm
        this % final_width  = 0.060d0   !  60.0 mm
        this % final_length = 0.100d0   ! 100.0 mm
        this % width_growth_ratio = 1.0d0/4.0d0   ! 
        this % length_growth_ratio = 1.0d0/4.0d0   ! 

end subroutine



function nnLeaf(this) result(ret)
    class(Leaf_),intent(in) :: this
    integer(int32) :: ret

    ret = this%femdomain%nn()
end function



function neLeaf(this) result(ret)
    class(Leaf_),intent(in) :: this
    integer(int32) :: ret

    ret = this%femdomain%ne()
end function

end module 