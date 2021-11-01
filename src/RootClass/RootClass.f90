module RootClass
    use, intrinsic :: iso_fortran_env
    use KinematicClass
    use FEMDomainClass
    use StemClass
    implicit none
    
    type :: Root_
        type(FEMDomain_)    ::  FEMDomain
        real(real64)             ::  Thickness,length,width
        real(real64)             ::  MaxThickness,Maxlength,Maxwidth
        real(real64)             ::  center_bottom(3),center_top(3)
        real(real64)             ::  radius_bottom(3),radius_top(3)
        real(real64)             ::  outer_normal_bottom(3),outer_normal_top(3)
        real(real64)             ::  rot_x = 0.0d0
        real(real64)             ::  rot_y = 0.0d0
        real(real64)             ::  rot_z = 0.0d0
        real(real64)             ::  disp_x = 0.0d0
        real(real64)             ::  disp_y = 0.0d0
        real(real64)             ::  disp_z = 0.0d0
        integer(int32)           ::  EdgeNodeID(4)
        integer(int32)           ::  EdgeElemID(4)
        real(real64)             ::  maxdiameter,mindiameter,minlength
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

        ! density
        real(real64),allocatable :: DryDensity(:)
        real(real64),allocatable :: WaterContent(:)

        integer(int32)             ::  Division
        type(Root_),pointer ::  pRoot
    contains
        procedure, public :: Init => initRoot
        procedure, public :: rotate => rotateRoot
        
        procedure, public :: move => moveRoot
                
        procedure,pass :: connectRootRoot => connectRootRoot
        procedure,pass :: connectRootStem => connectRootStem

        generic :: connect => connectRootRoot, connectRootStem

        procedure, public :: rescale => rescaleRoot
        procedure, public :: resize => resizeRoot
        procedure, public :: fix => fixRoot
        
        procedure, public :: getCoordinate => getCoordinateRoot

        procedure, public :: getVolume => getVolumeRoot
        procedure, public :: getBiomass => getBiomassRoot

        procedure, public :: gmsh => gmshRoot
        procedure, public :: vtk => vtkRoot
        procedure, public :: msh => mshRoot
        procedure, public :: stl => stlRoot
        procedure, public :: export => exportRoot
    end type
contains



! ########################################
subroutine initRoot(obj,config,regacy,Thickness,length,width,MaxThickness,Maxlength,Maxwidth,rotx,roty,rotz,location)
    class(Root_),intent(inout) :: obj
    real(real64),optional,intent(in)::  Thickness,length,width
    real(real64),optional,intent(in)::  MaxThickness,Maxlength,MaxWidth
    real(real64),optional,intent(in)::  rotx,roty,rotz,location(3)
    logical, optional,intent(in) :: regacy
    character(*),optional,intent(in) :: config
    type(IO_) :: Rootconf,f
    character(200) :: fn,conf,line
    integer(int32),allocatable :: buf(:)
    integer(int32) :: id,rmc,n,node_id,node_id2,elemid,blcount,i,j
    real(real64) :: loc(3)
    logical :: debug=.false.

    ! 節を生成するためのスクリプトを開く
    if(.not.present(config) .or. index(config,".json")==0 )then
        ! デフォルトの設定を生成
        if(debug) print *, "New Root-configuration >> Rootconfig.json"
        call Rootconf%open("rootconfig.json")
        write(Rootconf%fh,*) '{'
        write(Rootconf%fh,*) '   "type": "root",'
        write(Rootconf%fh,*) '   "minlength": 0.002,'
        write(Rootconf%fh,*) '   "mindiameter": 0.001,'
        write(Rootconf%fh,*) '   "maxlength": 0.07,'
        write(Rootconf%fh,*) '   "maxdiameter": 0.01,'
        write(Rootconf%fh,*) '   "xnum": 10,'
        write(Rootconf%fh,*) '   "ynum": 10,'
        write(Rootconf%fh,*) '   "znum": 10'
        write(Rootconf%fh,*) '}'
        conf="rootconfig.json"
        call Rootconf%close()
    else
        conf = trim(config)
    endif
    
    call Rootconf%open(trim(conf))
    blcount=0
    do
        read(Rootconf%fh,'(a)') line
        if(debug) print *, trim(line)
        if( adjustl(trim(line))=="{" )then
            blcount=1
            cycle
        endif
        if( adjustl(trim(line))=="}" )then
            exit
        endif
        
        if(blcount==1)then
            
            if(index(line,"type")/=0 .and. index(line,"root")==0 )then
                print *, "ERROR: This config-file is not for Root"
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


            if(index(line,"maxdiameter")/=0 )then
                ! 種子の長さ
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) obj%maxdiameter
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


            if(index(line,"mindiameter")/=0 )then
                ! 種子の長さ
                rmc=index(line,",")
                ! カンマがあれば除く
                if(rmc /= 0)then
                    line(rmc:rmc)=" "
                endif
                id = index(line,":")
                read(line(id+1:),*) obj%mindiameter
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
    call Rootconf%close()

    ! グラフ構造とメッシュ構造を生成する。



    !
    !                                    <II>        
    !                     # # #           
    !                 #   # B       # #           
    !               #     #       #   #           
    !              # #     #           
    !              #      #    #      #           
    !              #      #    #      #           
    !              #      #    #      #           
    !              #      #    #      #           
    !              #      #           #           
    !              #      # D #           
    !              #    #            #             
    !              #  C     A  #   #   <I>           
    !              # #         # #                 
    !              # # # # # # #                        
    !                                             

    ! メッシュを生成
    call obj%FEMdomain%create(meshtype="rectangular3D",x_num=obj%xnum,y_num=obj%ynum,z_num=obj%znum,&
    x_len=obj%mindiameter/2.0d0,y_len=obj%mindiameter/2.0d0,z_len=obj%minlength )
    
    ! initialize physical parameter
    obj%DryDensity = zeros( obj%FEMDomain%ne() )
    obj%watercontent = zeros( obj%FEMDomain%ne() )
    
    obj%DryDensity(:) = freal(rootconf%parse(config,key1="drydensity"))
    obj%watercontent(:) = freal(rootconf%parse(config,key1="watercontent"))

    ! <I>面に属する要素番号、節点番号、要素座標、節点座標のリストを生成
    obj%I_planeNodeID = obj%FEMdomain%mesh%getNodeList(zmax=0.0d0)
    obj%I_planeElementID = obj%FEMdomain%mesh%getElementList(zmax=0.0d0)
    
    ! <I>面に属する要素番号、節点番号、要素座標、節点座標のリストを生成
    obj%II_planeNodeID = obj%FEMdomain%mesh%getNodeList(zmin=obj%minlength)
    obj%II_planeElementID = obj%FEMdomain%mesh%getElementList(zmin=obj%minlength)
    
    buf   = obj%FEMDomain%mesh%getNodeList(&
        xmin=obj%mindiameter/2.0d0 - obj%mindiameter/dble(obj%xnum)/2.0d0 ,&
        xmax=obj%mindiameter/2.0d0 + obj%mindiameter/dble(obj%xnum)/2.0d0 ,&
        ymin=obj%mindiameter/2.0d0 - obj%mindiameter/dble(obj%ynum)/2.0d0 ,&
        ymax=obj%mindiameter/2.0d0 + obj%mindiameter/dble(obj%ynum)/2.0d0 ,&
        zmax=0.0d0)
    obj%A_PointNodeID = buf(1)

    buf   = obj%FEMDomain%mesh%getNodeList(&
        xmin=obj%mindiameter/2.0d0 - obj%mindiameter/dble(obj%xnum)/2.0d0 ,&
        xmax=obj%mindiameter/2.0d0 + obj%mindiameter/dble(obj%xnum)/2.0d0 ,&
        ymin=obj%mindiameter/2.0d0 - obj%mindiameter/dble(obj%ynum)/2.0d0 ,&
        ymax=obj%mindiameter/2.0d0 + obj%mindiameter/dble(obj%ynum)/2.0d0 ,&
        zmin=obj%minlength)
    obj%B_PointNodeID = buf(1)
    buf    = obj%FEMDomain%mesh%getElementList(&
        xmin=obj%mindiameter/2.0d0 - obj%mindiameter/dble(obj%xnum)/2.0d0 ,&
        xmax=obj%mindiameter/2.0d0 + obj%mindiameter/dble(obj%xnum)/2.0d0 ,&
        ymin=obj%mindiameter/2.0d0 - obj%mindiameter/dble(obj%ynum)/2.0d0 ,&
        ymax=obj%mindiameter/2.0d0 + obj%mindiameter/dble(obj%ynum)/2.0d0 ,&
        zmax=0.0d0)
    obj%A_PointElementID = buf(1)

    buf    = obj%FEMDomain%mesh%getElementList(&
        xmin=obj%mindiameter/2.0d0 - obj%mindiameter/dble(obj%xnum)/2.0d0 ,&
        xmax=obj%mindiameter/2.0d0 + obj%mindiameter/dble(obj%xnum)/2.0d0 ,&
        ymin=obj%mindiameter/2.0d0 - obj%mindiameter/dble(obj%ynum)/2.0d0 ,&
        ymax=obj%mindiameter/2.0d0 + obj%mindiameter/dble(obj%ynum)/2.0d0 ,&
        zmin=obj%minlength)
    obj%B_PointElementID = buf(1)

    call obj%FEMdomain%rotate(x=radian(180.0d0) )
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
        
            obj%Thickness   = input(default=0.010d0,option= Thickness     )
            obj%length      = input(default=0.050d0,option= length      )
            obj%width       = input(default=0.010d0,option= width)
        
            obj%MaxThickness   = input(default=0.50d0  ,option=MaxThickness      )
            obj%Maxlength      = input(default=10.0d0  ,option=Maxlength       )
            obj%Maxwidth       = input(default=0.50d0  ,option=Maxwidth )
        
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


! ########################################
subroutine exportRoot(obj,FileName,RootID)
    class(Root_),intent(in)::obj
    character(*),intent(in) :: FileName
    integer(int32),optional,intent(inout) :: RootID
    real(real64) :: radius

    
    radius=0.50d0*obj%width+0.50d0*obj%Thickness
    
    open(13,file=FileName)
    write(13,'(A)') "//+"
    write(13,'(A)') 'SetFactory("OpenCASCADE");'
    write(13,*) "Cylinder(",input(default=1,option=RootID),") = {",&
    obj%center_bottom(1),",", obj%center_bottom(2),",", obj%center_bottom(3),",",&
    obj%center_top(1)-obj%center_bottom(1),",", obj%center_top(2)-obj%center_bottom(2),",",&
     obj%center_top(3)-obj%center_bottom(3),",",&
    radius,", 2*Pi};"
    close(13)
    RootID=RootID+1

end subroutine
! ########################################


! ########################################
recursive subroutine rotateRoot(obj,x,y,z,reset)
    class(Root_),intent(inout) :: obj
    real(real64),optional,intent(in) :: x,y,z
    logical,optional,intent(in) :: reset
    real(real64),allocatable :: origin1(:),origin2(:),disp(:)

    if(present(reset) )then
        if(reset .eqv. .true.)then
            call obj%femdomain%rotate(-obj%rot_x,-obj%rot_y,-obj%rot_z)
            obj%rot_x = 0.0d0
            obj%rot_y = 0.0d0
            obj%rot_z = radian(180.0d0)
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
recursive subroutine moveRoot(obj,x,y,z,reset)
    class(Root_),intent(inout) :: obj
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
subroutine connectRootRoot(obj,direct,Root)
    class(Root_),intent(inout) :: obj
    class(Root_),intent(inout) :: Root
    character(2),intent(in) :: direct
    real(real64),allocatable :: x1(:),x2(:),disp(:)

    if(direct=="->" .or. direct=="=>")then
        ! move obj to connect Root (Root is not moved.)
        x1 =  obj%getCoordinate("A")
        x2 = Root%getCoordinate("B")
        disp = x2 - x1
        call obj%move(x=disp(1),y=disp(2),z=disp(3) )
    endif
    if(direct=="<-" .or. direct=="<=")then
        ! move obj to connect Root (Root is not moved.)
        x1 = Root%getCoordinate("A")
        x2 =  obj%getCoordinate("B")
        disp = x2 - x1
        call Root%move(x=disp(1),y=disp(2),z=disp(3) )
    endif

end subroutine
! ########################################


! ########################################
subroutine connectRootStem(obj,direct,stem)
    class(Root_),intent(inout) :: obj
    class(Stem_),intent(inout) :: Stem
    character(2),intent(in) :: direct
    real(real64),allocatable :: x1(:),x2(:),disp(:)

    if(direct=="->" .or. direct=="=>")then
        ! move obj to connect Stem (Stem is not moved.)
        x1 =  obj%getCoordinate("A")
        x2 = Stem%getCoordinate("A") ! 注意！stemのAにつなぐ
        disp = x2 - x1
        call obj%move(x=disp(1),y=disp(2),z=disp(3) )
    endif


    if(direct=="<-" .or. direct=="<=")then
        ! move obj to connect Stem (Stem is not moved.)
        x1 = Stem%getCoordinate("A")
        x2 =  obj%getCoordinate("A") ! 注意！rootのAにつなぐ
        disp = x2 - x1
        call Stem%move(x=disp(1),y=disp(2),z=disp(3) )
    endif

end subroutine
! ########################################

! ########################################
function getCoordinateRoot(obj,nodetype) result(ret)
    class(Root_),intent(inout) :: obj
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

subroutine gmshRoot(obj,name)
    class(Root_),intent(inout) :: obj
    character(*),intent(in) ::name
    if(obj%femdomain%mesh%empty() )then
        return
    endif
    
    call obj%femdomain%gmsh(Name=name)
end subroutine
! ########################################

subroutine mshRoot(obj,name)
    class(Root_),intent(inout) :: obj
    character(*),intent(in) ::name
    if(obj%femdomain%mesh%empty() )then
        return
    endif
    
    call obj%femdomain%msh(Name=name)
end subroutine
! ########################################

subroutine vtkRoot(obj,name)
    class(Root_),intent(inout) :: obj
    character(*),intent(in) ::name
    if(obj%femdomain%mesh%empty() )then
        return
    endif
    
    call obj%femdomain%vtk(Name=name)
end subroutine
! ########################################

subroutine stlRoot(obj,name)
    class(Root_),intent(inout) :: obj
    character(*),intent(in) ::name
    if(obj%femdomain%mesh%empty() )then
        return
    endif
    
    call obj%femdomain%stl(Name=name)
end subroutine


! ########################################
subroutine resizeRoot(obj,x,y,z)
    class(Root_),optional,intent(inout) :: obj
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
subroutine rescaleRoot(obj,x,y,z)
    class(Root_),optional,intent(inout) :: obj
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
subroutine fixRoot(obj,x,y,z)
    class(Root_),optional,intent(inout) :: obj
    real(real64),optional,intent(in) :: x,y,z
    real(real64),allocatable :: origin1(:), origin2(:),disp(:)

    origin1 = obj%getCoordinate("A")
    call obj%move(x=-origin1(1),y=-origin1(2),z=-origin1(3) )
    call obj%move(x=x,y=y,z=z )
end subroutine
! ########################################




function getVolumeRoot(obj) result(ret)
    class(Root_),intent(in) :: obj
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


function getBiomassRoot(obj) result(ret)
    class(Root_),intent(in) :: obj
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