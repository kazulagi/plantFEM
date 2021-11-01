module LeafClass
    use, intrinsic :: iso_fortran_env
    use KinematicClass
    use FEMDomainClass
    use PetiClass
    use StemClass
    use LightClass
    use AirClass
    implicit none

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

        integer(int32),allocatable  :: I_planeNodeID(:)
        integer(int32),allocatable  :: I_planeElementID(:)
        integer(int32),allocatable  :: II_planeNodeID(:)
        integer(int32),allocatable  :: II_planeElementID(:)
        integer(int32)  :: A_PointNodeID
        integer(int32)  :: B_PointNodeID
        integer(int32)  :: A_PointElementID
        integer(int32)  :: B_PointElementID
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

        real(real64),allocatable :: DryDensity(:)
        real(real64),allocatable :: WaterContent(:)
    

    contains
        procedure, public :: Init => initLeaf
        procedure, public :: rotate => rotateleaf
        procedure, public :: move => moveleaf
        procedure, public :: curve => curveleaf
        procedure, public :: create => createLeaf
        
        procedure,pass :: connectLeafLeaf => connectLeafLeaf
        procedure,pass :: connectLeafStem => connectLeafStem

        generic :: connect => connectLeafLeaf, connectLeafStem
        
        procedure, public :: photosynthesis => photosynthesisLeaf
        
        procedure, public :: rescale => rescaleleaf
        procedure, public :: adjust => adjustLeaf
        procedure, public :: resize => resizeleaf
        
        procedure, public :: getVolume => getVolumeLeaf
        procedure, public :: getBiomass => getBiomassLeaf
        procedure, public :: getCoordinate => getCoordinateleaf


        procedure, public :: gmsh => gmshleaf
        procedure, public :: msh => mshleaf
        procedure, public :: vtk => vtkleaf
        procedure, public :: stl => stlleaf
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
        curvature)
        class(leaf_),intent(inout) :: obj
        real(real64),optional,intent(in) :: Thickness,length,width,ShapeFactor
        real(real64),optional,intent(in) :: MaxThickness,Maxlength,Maxwidth
        real(real64),optional,intent(in)::  rotx,roty,rotz,location(3),SoyWidthRatio,curvature
        integer(int32),optional,intent(in) :: species
        logical, optional,intent(in) :: regacy
        character(*),optional,intent(in) :: config
        type(IO_) :: leafconf,f
        character(200) :: fn,conf,line
        integer(int32),allocatable :: buf(:)
        integer(int32) :: id,rmc,n,node_id,node_id2,elemid,blcount,i,j
        real(real64) :: loc(3),radius,z,leaf_L
        logical :: debug=.false.

        ! 節を生成するためのスクリプトを開く
        if(.not.present(config) .or. index(config,".json")==0 )then
            ! デフォルトの設定を生成
            if(debug) print *, "New leaf-configuration >> leafconfig.json"
            call leafconf%open("leafconfig.json")
            write(leafconf%fh,*) '{'
            write(leafconf%fh,*) '   "type": "leaf",'
            write(leafconf%fh,*) '   "minlength": 0.005,'
            write(leafconf%fh,*) '   "minwidth": 0.005,'
            write(leafconf%fh,*) '   "minthickness": 0.0001,'
            write(leafconf%fh,*) '   "maxlength": 0.07,'
            write(leafconf%fh,*) '   "maxwidth": 0.045,'
            write(leafconf%fh,*) '   "maxthickness": 0.001,'
            write(leafconf%fh,*) '   "shaperatio": 0.3,'
            write(leafconf%fh,*) '   "drydensity": 0.0,'
            write(leafconf%fh,*) '   "watercontent": 0.0,'
            write(leafconf%fh,*) '   "xnum": 10,'
            write(leafconf%fh,*) '   "ynum": 10,'
            write(leafconf%fh,*) '   "znum": 20'
            write(leafconf%fh,*) '}'
            conf="leafconfig.json"
            call leafconf%close()
        else
            conf = trim(config)
        endif
        
        call leafconf%open(trim(conf))
        blcount=0
        do
            read(leafconf%fh,'(a)') line
            if(debug) print *, trim(line)
            if( adjustl(trim(line))=="{" )then
                blcount=1
                cycle
            endif
            if( adjustl(trim(line))=="}" )then
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
    
            
    
        ! グラフ構造とメッシュ構造を生成する。
    
        !
        !           %%%%%%%%%%%%%%%%%%%%%%%%%%%%%  B
        !         %%                        %   %
        !        %%                    %      %%  
        !      %%                 %          %%    
        !     %%            %              %%      
        !     %%      %                  %%        
        !     %%                       %%          
        !   A   %%                  %%            
        !      <I> %%%%%%%%%%%%%%%%                               
    
    
    
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
        obj%DryDensity(:) = freal(leafconf%parse(conf,key1="drydensity"))
        obj%watercontent(:) = freal(leafconf%parse(conf,key1="watercontent"))
        
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
        obj%A_PointNodeID = buf(1)
    
        buf   = obj%FEMDomain%mesh%getNodeList(&
            xmin=obj%minwidth/2.0d0 - obj%minwidth/dble(obj%xnum)/2.0d0 ,&
            xmax=obj%minwidth/2.0d0 + obj%minwidth/dble(obj%xnum)/2.0d0 ,&
            ymin=obj%minwidth/2.0d0 - obj%minwidth/dble(obj%ynum)/2.0d0 ,&
            ymax=obj%minwidth/2.0d0 + obj%minwidth/dble(obj%ynum)/2.0d0 ,&
            zmin=obj%minlength)
        obj%B_PointNodeID = buf(1)
        buf    = obj%FEMDomain%mesh%getElementList(&
            xmin=obj%minwidth/2.0d0 - obj%minwidth/dble(obj%xnum)/2.0d0 ,&
            xmax=obj%minwidth/2.0d0 + obj%minwidth/dble(obj%xnum)/2.0d0 ,&
            ymin=obj%minwidth/2.0d0 - obj%minwidth/dble(obj%ynum)/2.0d0 ,&
            ymax=obj%minwidth/2.0d0 + obj%minwidth/dble(obj%ynum)/2.0d0 ,&
            zmax=0.0d0)
        obj%A_PointElementID = buf(1)
    
        buf    = obj%FEMDomain%mesh%getElementList(&
            xmin=obj%minwidth/2.0d0 - obj%minwidth/dble(obj%xnum)/2.0d0 ,&
            xmax=obj%minwidth/2.0d0 + obj%minwidth/dble(obj%xnum)/2.0d0 ,&
            ymin=obj%minwidth/2.0d0 - obj%minwidth/dble(obj%ynum)/2.0d0 ,&
            ymax=obj%minwidth/2.0d0 + obj%minwidth/dble(obj%ynum)/2.0d0 ,&
            zmin=obj%minlength)
        obj%B_PointElementID = buf(1)
    
        !print *, obj%A_PointNodeID
        !print *, obj%B_PointNodeID
        !print *, obj%A_PointElementID
        !print *, obj%B_PointElementID
!
        call obj%FEMdomain%remove()
        if(present(species) )then
            call obj%FEMdomain%create(meshtype="Leaf3D",x_num=obj%xnum,y_num=obj%ynum,z_num=obj%znum,&
            x_len=obj%minwidth/2.0d0,y_len=obj%minthickness/2.0d0,z_len=obj%minlength,species=species,SoyWidthRatio=SoyWidthRatio)
        else
            call obj%FEMdomain%create(meshtype="Leaf3D",x_num=obj%xnum,y_num=obj%ynum,z_num=obj%znum,&
            x_len=obj%minwidth/2.0d0,y_len=obj%minthickness/2.0d0,z_len=obj%minlength,shaperatio=obj%shaperatio)
        endif
    ! デバッグ用
    !    call f%open("I_phaseNodeID.txt")
    !    do i=1,size(obj%I_planeNodeID)
    !        write(f%fh,*) obj%femdomain%mesh%NodCoord( obj%I_planeNodeID(i) ,:)
    !    enddo
    !    call f%close()
    !
    !    call f%open("II_phaseNodeID.txt")
    !    do i=1,size(obj%II_planeNodeID)
    !        write(f%fh,*) obj%femdomain%mesh%NodCoord( obj%II_planeNodeID(i) ,:)
    !    enddo
    !    call f%close()
    !
    !    call f%open("I_phaseElementID.txt")
    !    do i=1,size(obj%I_planeElementID)
    !        do j=1,size(obj%femdomain%mesh%elemnod,2)
    !            write(f%fh,*) obj%femdomain%mesh%NodCoord( &
    !            obj%femdomain%mesh%elemnod(obj%I_planeElementID(i),j),:)
    !        enddo
    !    enddo
    !    call f%close()
    !
    !    call f%open("II_phaseElementID.txt")
    !    do i=1,size(obj%II_planeElementID)
    !        do j=1,size(obj%femdomain%mesh%elemnod,2)
    !            write(f%fh,*) obj%femdomain%mesh%NodCoord( &
    !            obj%femdomain%mesh%elemnod(obj%II_planeElementID(i),j),:)
    !        enddo
    !    enddo
    !    call f%close()
    !    return
    
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
integer(int32) :: dimnum

dimnum = size(obj%femdomain%mesh%nodcoord,2)
allocate(ret(dimnum) )
if( trim(nodetype)=="A" .or. trim(nodetype)=="a")then
    ret = obj%femdomain%mesh%nodcoord(obj%A_PointNodeID,:)
endif
if( trim(nodetype)=="B" .or. trim(nodetype)=="B")then
    ret = obj%femdomain%mesh%nodcoord(obj%B_PointNodeID,:)
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
subroutine vtkleaf(obj,name)
    class(leaf_),intent(inout) :: obj
    character(*),intent(in) ::name
    if(obj%femdomain%mesh%empty() )then
        return
    endif
    
    call obj%femdomain%vtk(Name=name)
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
subroutine resizeleaf(obj,x,y,z)
    class(Leaf_),optional,intent(inout) :: obj
    real(real64),optional,intent(in) :: x,y,z
    real(real64),allocatable :: origin1(:), origin2(:),disp(:)

    origin1 = obj%getCoordinate("A")
    call obj%femdomain%resize(x_len=x,y_len=y,z_len=z)
    origin2 = obj%getCoordinate("A")
    disp = origin1 - origin2
    call obj%move(x=disp(1),y=disp(2),z=disp(3) )
end subroutine
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

    real(real64) :: Lambda, volume


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

        !CO2固定量　mincro-mol/m-2/s
        ! ここ、体積あたりにする必要がある
        ! 一応、通常の葉の厚さを2mmとして、
        ! 1 micro-mol/m^2/sを、 1 micro-mol/ 0.002m^3/s= 500micro-mol/m^3/sとして計算
        ! また、ソース量はC6H12O6の質量gramとして換算する。
        ! CO2の分子量44.01g/mol
        ! C6H12O6の分子量180.16g/mol
        ! 6CO2 + 12H2O => C6H12O6 + 6H2O + 6O2
        ! よって、生成されるソース量は
        !               {CO2固定量,mol     }× {1/6 してグルコースmol}×グルコース分子量
        obj%source(i) =obj%source(i)+ A*dt/500.0d0*volume * 1.0d0/6.0d0 * 180.160d0
        
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



end module 