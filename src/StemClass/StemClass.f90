module StemClass
    use, intrinsic :: iso_fortran_env
    use KinematicClass
    use FEMDomainClass
    implicit none
    
    type :: Stem_
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
        integer(int32)  :: C_PointNodeID
        integer(int32)  :: D_PointNodeID
        
        integer(int32)  :: A_PointElementID
        integer(int32)  :: B_PointElementID
        integer(int32)  :: xnum = 10
        integer(int32)  :: ynum = 10
        integer(int32)  :: znum = 10

        ! position in a whole structure (single plant)
        integer(int32) :: StemID = -1
        integer(int32) :: InterNodeID = -1
        logical :: already_grown = .false.
        

        ! physical parameter
        real(real64),allocatable :: DryDensity(:)  ! element-wise
        real(real64),allocatable :: WaterContent(:)! element-wise

        ! For deformation analysis
        real(real64),allocatable :: YoungModulus(:)! element-wise
        real(real64),allocatable :: PoissonRatio(:)! element-wise
        real(real64),allocatable :: Density(:)     ! element-wise
        real(real64),allocatable :: CarbonDiffusionCoefficient(:) ! element-wise
        real(real64),allocatable :: Stress(:,:,:)     ! Gauss point-wise
        real(real64),allocatable :: Displacement(:,:) ! node-wise, three dimensional


        real(real64),allocatable :: BoundaryTractionForce(:,:) ! node-wise, three dimensional
        real(real64),allocatable :: BoundaryDisplacement(:,:) ! node-wise, three dimensional
        


        integer(int32)             ::  Division

        ! growth parameters
        real(real64)  :: my_time = 0.0d0
        real(real64)  :: initial_width  = 0.0010d0 ! 1.0 mm
        real(real64)  :: initial_length = 0.0010d0 ! 1.0 mm
        real(real64)  :: final_width  = 0.0040d0   ! 4.0 mm
        real(real64)  :: final_length = 0.040d0   ! 40.0 mm
        real(real64)  :: width_growth_ratio = 1.0d0/4.0d0   ! 
        real(real64)  :: length_growth_ratio = 1.0d0/4.0d0   ! 

        type(Stem_),pointer ::  pStem


        ! physiological factor
        real(real64) :: R_d=1.0d0 ! 暗呼吸速度, mincro-mol/m-2/s
        real(real64) :: default_CarbonDiffusionCoefficient=0.0010d0 ! ソースの拡散係数 mincro-mol/m^2/m/s
    contains
        procedure, public :: Init => initStem
        procedure, public :: rotate => rotateStem
        procedure, public :: grow => growStem
        procedure, public :: change_length_or_width => growStem
        procedure, public :: resize => resizeStem
        procedure, public :: move => moveStem
        procedure, public :: connect => connectStem
        
        ! check condition
        ! is it empty?
        procedure, public :: empty => emptyStem
        procedure, public :: getCoordinate => getCoordinateStem
        procedure, public :: getLength => getLengthStem
        procedure, public :: getWidth => getWidthStem
        procedure, public :: FullyExpanded => FullyExpandedStem


        procedure, public :: gmsh => gmshStem
        procedure, public :: msh => mshStem
        procedure, public :: vtk => vtkStem
        procedure, public :: stl => stlStem
        procedure, public :: ply => plyStem
        procedure, public :: export => exportStem
        procedure, public :: getVolume => getVolumeStem
        procedure, public :: getBiomass => getBiomassStem

        procedure,public :: sync => syncStem

        procedure,public :: nn => nnStem
        procedure,public :: ne => neStem

        procedure,public :: remove => removeStem
    end type


    interface operator(//)
        module procedure append_stem_object_vector
    end interface

contains



! ########################################
subroutine initStem(obj,config,regacy,Thickness,length,width,MaxThickness,&
    Maxlength,Maxwidth,rotx,roty,rotz,location, x_num,y_num,z_num)
    class(Stem_),intent(inout) :: obj
    real(real64),optional,intent(in)::  Thickness,length,width
    real(real64),optional,intent(in)::  MaxThickness,Maxlength,MaxWidth
    real(real64),optional,intent(in)::  rotx,roty,rotz,location(3)
    logical, optional,intent(in) :: regacy
    character(*),optional,intent(in) :: config
    integer(int32),optional,intent(in) :: x_num,y_num,z_num
    type(IO_) :: stemconf,f
    character(200) :: fn,conf,line
    integer(int32),allocatable :: buf(:)
    real(real64) :: center_coord(1:3),dist_val
    integer(int32) :: id,rmc,n,node_id,node_id2,elemid,blcount,i,j
    real(real64) :: loc(3)
    logical :: debug=.false.

    obj%my_time = 0.0d0

    ! default value
    obj%minlength= 0.001
    obj%mindiameter= 0.001
    obj%maxlength= 0.07
    obj%maxdiameter= 0.01
    obj%xnum= 10
    obj%ynum= 10
    obj%znum= 10
    
    if(present(config))then
    
        conf = config
        call stemconf%open(conf,"r")
        blcount=0
        do
            read(stemconf%fh,'(a)') line
            if(debug) print *, line
            if( adjustl(line)=="{" )then
                blcount=1
                cycle
            endif
            if( adjustl(line)=="}" )then
                exit
            endif

            if(blcount==1)then

                if(index(line,"type")/=0 .and. index(line,"stem")==0 )then
                    print *, "ERROR: This config-file is not for stem"
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
        call stemconf%close()
    endif
    
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
    
        
    obj%xnum = input(default=obj%xnum,option=x_num)
    obj%ynum = input(default=obj%ynum,option=y_num)
    obj%znum = input(default=obj%znum,option=z_num)
    
    call obj%FEMdomain%create(meshtype="Cube",x_num=obj%xnum,y_num=obj%ynum,z_num=obj%znum,&
    x_len=obj%mindiameter/2.0d0,y_len=obj%mindiameter/2.0d0,z_len=obj%minlength )

    ! initialize physical parameters
    obj%DryDensity = zeros( obj%FEMDomain%ne() )
    obj%watercontent = zeros( obj%FEMDomain%ne() )
    
    if(present(config) )then
        obj%DryDensity(:) = freal(stemconf%parse(conf,key1="drydensity"))
        obj%watercontent(:) = freal(stemconf%parse(conf,key1="watercontent"))
    endif
    ! <I>面に属する要素番号、節点番号、要素座標、節点座標のリストを生成
    obj%I_planeNodeID = obj%FEMdomain%mesh%getNodeList(zmax=0.0d0)
    obj%I_planeElementID = obj%FEMdomain%mesh%getElementList(zmax=0.0d0)
    
    ! <I>面に属する要素番号、節点番号、要素座標、節点座標のリストを生成
    obj%II_planeNodeID = obj%FEMdomain%mesh%getNodeList(zmin=obj%minlength)
    obj%II_planeElementID = obj%FEMdomain%mesh%getElementList(zmin=obj%minlength)
    
    !buf   = obj%FEMDomain%mesh%getNodeList(&
    !    xmin=obj%mindiameter/2.0d0 - obj%mindiameter/dble(obj%xnum)/2.0d0 ,&
    !    xmax=obj%mindiameter/2.0d0 + obj%mindiameter/dble(obj%xnum)/2.0d0 ,&
    !    ymin=obj%mindiameter/2.0d0 - obj%mindiameter/dble(obj%ynum)/2.0d0 ,&
    !    ymax=obj%mindiameter/2.0d0 + obj%mindiameter/dble(obj%ynum)/2.0d0 ,&
    !    zmax=0.0d0)
    
    center_coord(1) = sum(obj%FEMDomain%mesh%nodcoord(obj%I_planeNodeID(:),1) )&
        /size(obj%I_planeNodeID)
    center_coord(2) = sum(obj%FEMDomain%mesh%nodcoord(obj%I_planeNodeID(:),2) )&
        /size(obj%I_planeNodeID)
    center_coord(3) = sum(obj%FEMDomain%mesh%nodcoord(obj%I_planeNodeID(:),3) )&
        /size(obj%I_planeNodeID)

    dist_val = norm(obj%FEMDomain%mesh%nodcoord(obj%I_planeNodeID(1),:)-center_coord)
    obj%A_PointNodeID = obj%I_planeNodeID(1)
    
    do i=2, size(obj%I_planeNodeID)
        if(  norm(obj%FEMDomain%mesh%nodcoord(obj%I_planeNodeID(i),:)-center_coord) < dist_val  )then
            obj%A_PointNodeID = obj%I_planeNodeID(i)
            dist_val = norm(obj%FEMDomain%mesh%nodcoord(obj%I_planeNodeID(i),:)-center_coord)
        endif
    enddo
    
    !obj%A_PointNodeID = median(buf)
    
!    buf   = obj%FEMDomain%mesh%getNodeList(&
!        xmin=obj%mindiameter/2.0d0 - obj%mindiameter/dble(obj%xnum)/2.0d0 ,&
!        xmax=obj%mindiameter/2.0d0 + obj%mindiameter/dble(obj%xnum)/2.0d0 ,&
!        ymin=obj%mindiameter/2.0d0 - obj%mindiameter/dble(obj%ynum)/2.0d0 ,&
!        ymax=obj%mindiameter/2.0d0 + obj%mindiameter/dble(obj%ynum)/2.0d0 ,&
!        zmin=obj%minlength)
    
    !obj%B_PointNodeID = buf(1)

    !obj%B_PointNodeID = median(buf)

    center_coord(1) = sum(obj%FEMDomain%mesh%nodcoord(obj%II_planeNodeID(:),1) )&
        /size(obj%I_planeNodeID)
    center_coord(2) = sum(obj%FEMDomain%mesh%nodcoord(obj%II_planeNodeID(:),2) )&
        /size(obj%I_planeNodeID)
    center_coord(3) = sum(obj%FEMDomain%mesh%nodcoord(obj%II_planeNodeID(:),3) )&
        /size(obj%I_planeNodeID)

    dist_val = norm(obj%FEMDomain%mesh%nodcoord(obj%II_planeNodeID(1),:)-center_coord)
    obj%B_PointNodeID = obj%II_planeNodeID(1)
    
    do i=2, size(obj%II_planeNodeID)
        if(  norm(obj%FEMDomain%mesh%nodcoord(obj%II_planeNodeID(i),:)-center_coord) < dist_val  )then
            obj%B_PointNodeID = obj%II_planeNodeID(i)
            dist_val = norm(obj%FEMDomain%mesh%nodcoord(obj%II_planeNodeID(i),:)-center_coord)
        endif
    enddo
    
    

    center_coord(1) = maxval(obj%FEMDomain%mesh%nodcoord(obj%I_planeNodeID(:),1) )

    center_coord(2) = sum(obj%FEMDomain%mesh%nodcoord(obj%I_planeNodeID(:),2) )&
        /size(obj%I_planeNodeID)

    center_coord(3) = sum(obj%FEMDomain%mesh%nodcoord(obj%I_planeNodeID(:),3) )&
        /size(obj%I_planeNodeID)

    dist_val = norm(obj%FEMDomain%mesh%nodcoord(obj%I_planeNodeID(1),:)-center_coord)
    obj%C_PointNodeID = obj%I_planeNodeID(1)
    
    do i=2, size(obj%I_planeNodeID)
        if(  norm(obj%FEMDomain%mesh%nodcoord(obj%I_planeNodeID(i),:)-center_coord) < dist_val  )then
            obj%C_PointNodeID = obj%I_planeNodeID(i)
            dist_val = norm(obj%FEMDomain%mesh%nodcoord(obj%I_planeNodeID(i),:)-center_coord)
        endif
    enddo
    
    

    !buf = obj%FEMDomain%mesh%getNodeList(&
    !xmin=obj%FEMDomain%xmax() ,&
    !ymin=obj%mindiameter/2.0d0 - obj%mindiameter/dble(obj%ynum)/2.0d0 ,&
    !ymax=obj%mindiameter/2.0d0 + obj%mindiameter/dble(obj%ynum)/2.0d0 ,&
    !zmax=obj%FEMDomain%zmin() )
    !
    !obj%C_PointNodeID = median(buf)
    

    center_coord(1) = sum(obj%FEMDomain%mesh%nodcoord(obj%II_planeNodeID(:),1) )&
        /size(obj%II_planeNodeID)

    center_coord(2) = maxval(obj%FEMDomain%mesh%nodcoord(obj%II_planeNodeID(:),2) )


    center_coord(3) = sum(obj%FEMDomain%mesh%nodcoord(obj%II_planeNodeID(:),3) )&
        /size(obj%II_planeNodeID)

    dist_val = norm(obj%FEMDomain%mesh%nodcoord(obj%II_planeNodeID(1),:)-center_coord)
    obj%D_PointNodeID = obj%II_planeNodeID(1)
    
    do i=2, size(obj%II_planeNodeID)
        if(  norm(obj%FEMDomain%mesh%nodcoord(obj%II_planeNodeID(i),:)-center_coord) < dist_val  )then
            obj%D_PointNodeID = obj%II_planeNodeID(i)
            dist_val = norm(obj%FEMDomain%mesh%nodcoord(obj%II_planeNodeID(i),:)-center_coord)
        endif
    enddo

    !buf = obj%FEMDomain%mesh%getNodeList(&
    !ymin=obj%FEMDomain%ymax() ,&
    !xmin=obj%mindiameter/2.0d0 - obj%mindiameter/dble(obj%ynum)/2.0d0 ,&
    !xmax=obj%mindiameter/2.0d0 + obj%mindiameter/dble(obj%ynum)/2.0d0 ,&
    !zmax=obj%FEMDomain%zmin() )
    !
    !obj%D_PointNodeID = median(buf)
    

    buf    = obj%FEMDomain%mesh%getElementList(&
        xmin=obj%mindiameter/2.0d0 - obj%mindiameter/dble(obj%xnum)/2.0d0 ,&
        xmax=obj%mindiameter/2.0d0 + obj%mindiameter/dble(obj%xnum)/2.0d0 ,&
        ymin=obj%mindiameter/2.0d0 - obj%mindiameter/dble(obj%ynum)/2.0d0 ,&
        ymax=obj%mindiameter/2.0d0 + obj%mindiameter/dble(obj%ynum)/2.0d0 ,&
        zmax=0.0d0)
    !obj%A_PointElementID = buf(1)
    obj%A_PointElementID = median(buf)
    
    
    buf    = obj%FEMDomain%mesh%getElementList(&
        xmin=obj%mindiameter/2.0d0 - obj%mindiameter/dble(obj%xnum)/2.0d0 ,&
        xmax=obj%mindiameter/2.0d0 + obj%mindiameter/dble(obj%xnum)/2.0d0 ,&
        ymin=obj%mindiameter/2.0d0 - obj%mindiameter/dble(obj%ynum)/2.0d0 ,&
        ymax=obj%mindiameter/2.0d0 + obj%mindiameter/dble(obj%ynum)/2.0d0 ,&
        zmin=obj%minlength)

    !obj%B_PointElementID = buf(1)
    obj%B_PointElementID = median(buf)

    if(debug) print *, obj%A_PointNodeID
    if(debug) print *, obj%B_PointNodeID
    if(debug) print *, obj%A_PointElementID
    if(debug) print *, obj%B_PointElementID
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




    !
    !           %%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
    !         %%                        %   %
    !        %%                    %      %%  
    !      %%                 %          %%    
    !     %%            %              %%      
    !     %%      %                  %%        
    !     %%                       %%          
    !       %%                  %%            
    !         %%%%%%%%%%%%%%%%%                               














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


    obj%CarbonDiffusionCoefficient = obj%default_CarbonDiffusionCoefficient*ones(obj%femdomain%ne() )
end subroutine 
! ########################################

subroutine resize(obj,x,y,z)
    class(Stem_),intent(inout) :: obj
    real(real64),optional,intent(in) :: x,y,z

    call obj%femdomain%resize(x,y,z)

end subroutine

! ########################################
subroutine exportStem(obj,FileName,StemID)
    class(Stem_),intent(in)::obj
    character(*),intent(in) :: FileName
    integer(int32),optional,intent(inout) :: StemID
    real(real64) :: radius

    
    radius=0.50d0*obj%width+0.50d0*obj%Thickness
    
    open(13,file=FileName)
    write(13,'(A)') "//+"
    write(13,'(A)') 'SetFactory("OpenCASCADE");'
    write(13,*) "Cylinder(",input(default=1,option=StemID),") = {",&
    obj%center_bottom(1),",", obj%center_bottom(2),",", obj%center_bottom(3),",",&
    obj%center_top(1)-obj%center_bottom(1),",", obj%center_top(2)-obj%center_bottom(2),",",&
     obj%center_top(3)-obj%center_bottom(3),",",&
    radius,", 2*Pi};"
    close(13)
    StemID=StemID+1

end subroutine
! ########################################


! ########################################
recursive subroutine rotateStem(obj,x,y,z,reset)
    class(Stem_),intent(inout) :: obj
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
recursive subroutine moveStem(obj,x,y,z,reset)
    class(Stem_),intent(inout) :: obj
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

subroutine connectStem(obj,direct,stem)
    class(Stem_),intent(inout) :: obj,stem
    character(2),intent(in) :: direct
    real(real64),allocatable :: x1(:),x2(:),disp(:)

    if(direct=="->" .or. direct=="=>")then
        ! move obj to connect stem (stem is not moved.)
        x1 =  obj%getCoordinate("A")
        x2 = stem%getCoordinate("B")
        disp = x2 - x1
        call obj%move(x=disp(1),y=disp(2),z=disp(3) )
    endif


    if(direct=="<-" .or. direct=="<=")then
        ! move obj to connect stem (stem is not moved.)
        x1 = stem%getCoordinate("A")
        x2 =  obj%getCoordinate("B")
        disp = x2 - x1
        call stem%move(x=disp(1),y=disp(2),z=disp(3) )
    endif
end subroutine


! ########################################
function getCoordinateStem(obj,nodetype) result(ret)
    class(Stem_),intent(in) :: obj
    character(*),intent(in) :: nodetype
    real(real64),allocatable :: ret(:)
    integer(int32) :: dimnum,n,i

    dimnum = size(obj%femdomain%mesh%nodcoord,2)
    
    allocate(ret(dimnum) )
    ret(:) = 0.0d0
    if( nodetype=="A" .or. nodetype=="a")then
        
        
        ! 20220701 this may be correct
        ret = obj%femdomain%mesh%nodcoord( obj%A_PointNodeID,: ) 
        return



        n = size(obj%I_planeNodeID )
        if(n==0)then
            print *, "ERROR >> getCoordinateStem >> size(obj%I_planeNodeID) = 0"
        endif
        if(.not.allocated(obj%I_planeNodeID))then
            
            print *, "ERROR >> getCoordinateStem >> .not. allocated(obj%I_planeNodeID) "
            
        endif
        do i=1,n
            ret(:) = ret(:) + obj%femdomain%mesh%nodcoord( obj%I_planeNodeID(i),: ) 
        enddo
        ret(:) = 1.0d0/dble(n) * ret(:)

        !ret = obj%femdomain%mesh%nodcoord(obj%A_PointNodeID,:)
    endif

    if( nodetype=="B" .or. nodetype=="b")then
        !ret = obj%femdomain%mesh%nodcoord(obj%B_PointNodeID,:)


        ! 20220701 this may be correct
        ret = obj%femdomain%mesh%nodcoord( obj%B_PointNodeID,: ) 
        return

        n = size(obj%II_planeNodeID )
        if(n==0)then
            print *, "ERROR >> getCoordinateStem >> size(obj%II_planeNodeID) = 0"
        endif
        if(.not.allocated(obj%I_planeNodeID))then
            
            print *, "ERROR >> getCoordinateStem >> .not. allocated(obj%II_planeNodeID) "
            
        endif
        do i=1,n
            ret(:) = ret(:) + obj%femdomain%mesh%nodcoord( obj%II_planeNodeID(i),: ) 
        enddo
        ret(:) = 1.0d0/dble(n) * ret(:)
    endif

end function
! ########################################

subroutine gmshStem(obj,name)
    class(Stem_),intent(inout) :: obj
    character(*),intent(in) ::name
    if(obj%femdomain%mesh%empty() )then
        return
    endif
    
    call obj%femdomain%gmsh(Name=name)
end subroutine

subroutine mshStem(obj,name)
    class(Stem_),intent(inout) :: obj
    character(*),intent(in) ::name
    if(obj%femdomain%mesh%empty() )then
        return
    endif
    
    call obj%femdomain%msh(Name=name)
end subroutine
! ##############################################

subroutine vtkStem(obj,name,field_name)
    class(Stem_),intent(inout) :: obj
    character(*),intent(in) ::name
    character(*),optional,intent(in) ::field_name
    
    if(obj%femdomain%mesh%empty() )then
        return
    endif
    
    call obj%femdomain%vtk(Name=name,field=field_name)
end subroutine
! ##############################################

subroutine stlStem(obj,name)
    class(Stem_),intent(inout) :: obj
    character(*),intent(in) ::name
    if(obj%femdomain%mesh%empty() )then
        return
    endif
    
    call obj%femdomain%ply(Name=name)
end subroutine

! ########################################
subroutine plyStem(obj,name)
    class(Stem_),intent(inout) :: obj
    character(*),intent(in) ::name
    if(obj%femdomain%mesh%empty() )then
        return
    endif
    
    call obj%femdomain%ply(Name=name)
end subroutine

! ########################################
subroutine resizeStem(obj,x,y,z)
    class(Stem_),optional,intent(inout) :: obj
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
recursive subroutine growStem(obj,length,length_rate,Width,width_rate,dt)
    class(Stem_),intent(inout) :: obj 
    real(real64),optional,intent(in) :: length,length_rate,width_rate,Width,dt
    real(real64) :: new_width,new_length
    real(real64) :: length_r,width_r,l_0,w_0,clength
    real(real64),allocatable :: origin(:),top(:),n1(:),coord(:),center(:),vert(:)
    integer(int32) :: i


    if(obj%already_grown)then
        ! ignore growth for this
        return
    endif

    if(present(dt) )then
        ! logistic curve
        ! automatic growth
        if(obj%femdomain%empty() ) then
            return
        endif
        obj%my_time = obj%my_time + dt
        ! growth curve: logistic function
        new_Length = obj%final_length&
            /(1.0d0 +&
                (obj%final_length/obj%initial_length - 1.0d0)&
                    *exp(-obj%length_growth_ratio*obj%my_time) )
    
        new_Width = obj%final_Width&
            /(1.0d0 +&
                (obj%final_Width/obj%initial_Width - 1.0d0)&
                    *exp(-obj%Width_growth_ratio*obj%my_time) )
        call obj%grow(Length=new_Length,Width=new_Width)
        return
    endif

    origin = obj%getCoordinate("A")
    top    = obj%getCoordinate("B")
    l_0    = sqrt(dot_product(top-origin, top-origin) ) 
    n1     = origin
    n1     = top - origin
    n1     = 1.0d0/norm(n1)*n1
    coord  = origin
    
    ! length-ratio = new length / old length
    if(present(length) )then
        length_r = length/l_0
    elseif(present(length_rate) )then
        length_r = length_rate
    else
        length_r = 1.0d0
    endif

    if(present(Width) )then
        width_r = Width/obj%getWidth()
    else
        width_r = input(default=1.0d0, option=width_rate)
    endif

    ! enlong & fatten
    do i=1,obj%femdomain%nn()
        coord(:) = obj%femdomain%mesh%nodcoord(i,:) - origin(:)
        center   = coord
        clength   = dot_product(coord, n1)
        center(:)  = clength*n1(:)
        vert = coord - center
        ! origin -> center -> current coordinate
        coord(:) = length_r*center(:) + width_r*vert(:)
        obj%femdomain%mesh%nodcoord(i,:) = origin(:) + coord(:)
    enddo

    

end subroutine
! ########################################

! ########################################
subroutine rescaleStem(obj,x,y,z)
    class(Stem_),optional,intent(inout) :: obj
    real(real64),optional,intent(in) :: x,y,z
    real(real64),allocatable :: origin1(:), origin2(:),disp(:)

    origin1 = obj%getCoordinate("A")
    call obj%femdomain%resize(x_rate=x,y_rate=y,z_rate=z)
    origin2 = obj%getCoordinate("A")
    disp = origin1 - origin2
    call obj%move(x=disp(1),y=disp(2),z=disp(3) )
end subroutine
! ########################################


function getLengthStem(obj) result(ret)
        class(Stem_),intent(in) :: obj
    real(real64) :: ret

    if(obj%femdomain%mesh%empty() )then
        ret = 0.0d0
    else
        ret = norm(&
            obj%femdomain%mesh%nodcoord(obj%A_PointNodeID,: ) &
            - obj%femdomain%mesh%nodcoord(obj%B_PointNodeID,:) )
    endif

end function


function getWidthStem(obj) result(ret)
    class(Stem_),intent(in) :: obj
real(real64) :: ret

if(obj%femdomain%mesh%empty() )then
    ret = 0.0d0
else
    ret = 2.0d0*norm(&
        obj%femdomain%mesh%nodcoord(obj%C_PointNodeID,: ) &
        - obj%femdomain%mesh%nodcoord(obj%A_PointNodeID,:) )
endif

end function


function getVolumeStem(obj) result(ret)
    class(Stem_),intent(in) :: obj
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
! ####################################################################


! ####################################################################
function getBiomassStem(obj) result(ret)
    class(Stem_),intent(in) :: obj
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
! ####################################################################


! ########################################
function emptyStem(obj) result(Stem_is_empty)
    class(Stem_),intent(in) :: obj
    logical :: Stem_is_empty

    Stem_is_empty = obj%femdomain%empty()

end function
! ########################################

subroutine syncStem(obj,from,mpid)
    class(Stem_),intent(inout) :: obj
    integer(int32),intent(in) :: from
    type(MPI_),intent(inout) :: mpid


    call obj%FEMDomain%sync(from=from,mpid=mpid)
    call mpid%bcast(from=from,val=obj% Thickness) !
    call mpid%bcast(from=from,val=obj%length) !
    call mpid%bcast(from=from,val=obj%width) !
    call mpid%bcast(from=from,val=obj% MaxThickness) !
    call mpid%bcast(from=from,val=obj%Maxlength) !
    call mpid%bcast(from=from,val=obj%Maxwidth) !
    call mpid%bcast(from=from,val=obj% maxdiameter) !
    call mpid%bcast(from=from,val=obj%mindiameter) !
    call mpid%bcast(from=from,val=obj%minlength) !
    call mpid%bcast(from=from,val=obj% rot_x) ! = 0.0d0
    call mpid%bcast(from=from,val=obj% rot_y) ! = 0.0d0
    call mpid%bcast(from=from,val=obj% rot_z) ! = 0.0d0
    call mpid%bcast(from=from,val=obj% disp_x) ! = 0.0d0
    call mpid%bcast(from=from,val=obj% disp_y) ! = 0.0d0
    call mpid%bcast(from=from,val=obj% disp_z) ! = 0.0d0
    call mpid%BcastMPIRealVecFixedSize(from=from,val=obj% center_bottom) !(3)
    call mpid%BcastMPIRealVecFixedSize(from=from,val=obj%center_top) !(3)
    call mpid%BcastMPIRealVecFixedSize(from=from,val=obj% radius_bottom) !(3)
    call mpid%BcastMPIRealVecFixedSize(from=from,val=obj%radius_top) !(3)
    call mpid%BcastMPIRealVecFixedSize(from=from,val=obj% outer_normal_bottom) !(3)
    call mpid%BcastMPIRealVecFixedSize(from=from,val=obj%outer_normal_top) !(3)

    call mpid%BcastMPIIntVecFixedSize(from=from,val=obj% EdgeNodeID)!(4)
    call mpid%BcastMPIIntVecFixedSize(from=from,val=obj% EdgeElemID)!(4)
    call mpid%bcast(from=from,val=obj%I_planeNodeID)!(:)
    call mpid%bcast(from=from,val=obj%I_planeElementID)!(:)
    call mpid%bcast(from=from,val=obj%II_planeNodeID)!(:)
    call mpid%bcast(from=from,val=obj%II_planeElementID)!(:)
    call mpid%bcast(from=from,val=obj%A_PointNodeID)!
    call mpid%bcast(from=from,val=obj%B_PointNodeID)!
    call mpid%bcast(from=from,val=obj%A_PointElementID)!
    call mpid%bcast(from=from,val=obj%B_PointElementID)!
    call mpid%bcast(from=from,val=obj%xnum)! = 10
    call mpid%bcast(from=from,val=obj%ynum)! = 10
    call mpid%bcast(from=from,val=obj%znum)! = 10

    ! physical parameter
    call mpid%bcast(from=from,val=obj%DryDensity)!(:)  ! element-wise
    call mpid%bcast(from=from,val=obj%WaterContent)!(:)! element-wise

    ! For deformation analysis
    call mpid%bcast(from=from,val=obj%YoungModulus)!(:)! element-wise
    call mpid%bcast(from=from,val=obj%CarbonDiffusionCoefficient)
    call mpid%bcast(from=from,val=obj%PoissonRatio)!(:)! element-wise
    call mpid%bcast(from=from,val=obj%Density)!(:)     ! element-wise
    call mpid%bcast(from=from,val=obj%Stress)!(:,:,:)     ! Gauss point-wise
    call mpid%bcast(from=from,val=obj%Displacement)!(:,:) ! node-wise, three dimensional


    call mpid%bcast(from=from,val=obj%BoundaryTractionForce)!(:,:) ! node-wise, three dimensional
    call mpid%bcast(from=from,val=obj%BoundaryDisplacement)!(:,:) ! node-wise, three dimensional
    


    call mpid%bcast(from=from,val=obj%Division)

    !type(Stem_),pointer ::  pStem

end subroutine
! #########################################################


subroutine syncStemVector(obj,from,mpid)
	type(Stem_),allocatable,intent(inout) :: obj(:)
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


! ########################################
function FullyExpandedStem(obj,threshold) result(ret_expanded)
    class(Stem_),optional,intent(inout) :: obj
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

subroutine removeStem(this)
    class(Stem_),intent(inout) :: this


    call this%FEMDomain%remove()
    this%  Thickness = 0.0d0
    this%  length = 0.0d0
    this%  width = 0.0d0
    this%  MaxThickness = 0.0d0
    this%  Maxlength = 0.0d0
    this%  Maxwidth = 0.0d0
    this%  center_bottom = 0.0d0
    this%  center_top = 0.0d0
    this%  radius_bottom = 0.0d0
    this%  radius_top = 0.0d0
    this%  outer_normal_bottom = 0.0d0
    this%  outer_normal_top = 0.0d0
    this%  rot_x = 0.0d0
    this%  rot_y = 0.0d0
    this%  rot_z = 0.0d0
    this%  disp_x = 0.0d0
    this%  disp_y = 0.0d0
    this%  disp_z = 0.0d0

    this % EdgeNodeID = 0
    this % EdgeElemID = 0
    this%  maxdiameter = 0.0d0
    this % mindiameter = 0.0d0
    this % minlength = 0.0d0
    if(allocated(this% I_planeNodeID) )deallocate(this% I_planeNodeID)! (:)
    if(allocated(this% I_planeElementID) )deallocate(this% I_planeElementID)! (:)
    if(allocated(this% II_planeNodeID) )deallocate(this% II_planeNodeID)! (:)
    if(allocated(this% II_planeElementID) )deallocate(this% II_planeElementID)! (:)
    this %A_PointNodeID =0
    this %B_PointNodeID=0
    this %C_PointNodeID=0
    this %D_PointNodeID=0
    
    this %A_PointElementID = 0
    this %B_PointElementID = 0
    this %xnum = 10
    this %ynum = 10
    this %znum = 10

    ! position in a whole structure (single plant)
    this % StemID = -1
    this % InterNodeID = -1
    this % already_grown = .false.
    

    ! physical parameter
    if(allocated(this% DryDensity) )deallocate(this% DryDensity)! (:)  ! element-wise
    if(allocated(this% WaterContent) )deallocate(this% WaterContent)! (:)! element-wise

    ! For deformation analysis
    if(allocated(this% YoungModulus) )deallocate(this% YoungModulus)! (:)! element-wise
    if(allocated(this% PoissonRatio) )deallocate(this% PoissonRatio)! (:)! element-wise
    if(allocated(this% Density) )deallocate(this% Density)! (:)     ! element-wise
    if(allocated(this% CarbonDiffusionCoefficient) )deallocate(this% CarbonDiffusionCoefficient)! (:)     ! element-wise
    
    if(allocated(this% Stress) )deallocate(this% Stress)! (:,:,:)     ! Gauss point-wise
    if(allocated(this% Displacement) )deallocate(this% Displacement)! (:,:) ! node-wise, three dimensional


    if(allocated(this% BoundaryTractionForce) )deallocate(this% BoundaryTractionForce)! (:,:) ! node-wise, three dimensional
    if(allocated(this% BoundaryDisplacement) )deallocate(this% BoundaryDisplacement)! (:,:) ! node-wise, three dimensional
    


    this % Division = 0

    ! growth parameters
    this % my_time = 0.0d0
    this % initial_width  = 0.0010d0 ! 1.0 mm
    this % initial_length = 0.0010d0 ! 1.0 mm
    this % final_width  = 0.0040d0   ! 4.0 mm
    this % final_length = 0.040d0   ! 40.0 mm
    this % width_growth_ratio = 1.0d0/4.0d0   ! 
    this % length_growth_ratio = 1.0d0/4.0d0   ! 

    if(associated(this%pStem) ) nullify(this%pStem)

end subroutine


function nnStem(this) result(ret)
    class(Stem_),intent(in) :: this
    integer(int32) :: ret

    ret = this%femdomain%nn()
end function



function neStem(this) result(ret)
    class(Stem_),intent(in) :: this
    integer(int32) :: ret

    ret = this%femdomain%ne()
end function


! ############################################################
function append_stem_object_vector(arg1,arg2) result(ret)
    type(Stem_),allocatable,intent(in) :: arg1(:),arg2(:)
    type(Stem_),allocatable :: ret(:)

    if(.not. allocated(arg1) )then
        if(.not. allocated(arg2) )then
            return 
        else
            ret = arg2
        endif    
    else
        if(.not. allocated(arg2) )then
            ret = arg1
            return 
        else
            allocate(ret(size(arg1)+size(arg2) ) )
            ret(1:size(arg1) ) = arg1(:)
            ret(size(arg1)+1: ) = arg2(:)
        endif    
    endif

end function
! ############################################################

end module