module RootClass
   use, intrinsic :: iso_fortran_env
   use KinematicClass
   use FEMDomainClass
   use StemClass
   implicit none

   type :: Root_
      type(FEMDomain_)    ::  FEMDomain
      real(real64)             ::  Thickness, length, width
      real(real64)             ::  MaxThickness, Maxlength, Maxwidth
      real(real64)             ::  center_bottom(3), center_top(3)
      real(real64)             ::  radius_bottom(3), radius_top(3)
      real(real64)             ::  outer_normal_bottom(3), outer_normal_top(3)
      real(real64)             ::  rot_x = 0.0d0
      real(real64)             ::  rot_y = 0.0d0
      real(real64)             ::  rot_z = 0.0d0
      real(real64)             ::  disp_x = 0.0d0
      real(real64)             ::  disp_y = 0.0d0
      real(real64)             ::  disp_z = 0.0d0
      integer(int32)           ::  EdgeNodeID(4)
      integer(int32)           ::  EdgeElemID(4)
      real(real64)             ::  maxdiameter, mindiameter, minlength
      integer(int32), allocatable  :: I_planeNodeID(:)
      integer(int32), allocatable  :: I_planeElementID(:)
      integer(int32), allocatable  :: II_planeNodeID(:)
      integer(int32), allocatable  :: II_planeElementID(:)
      integer(int32)  :: A_PointNodeID
      integer(int32)  :: B_PointNodeID
      integer(int32)  :: A_PointElementID
      integer(int32)  :: B_PointElementID
      integer(int32)  :: xnum = 10
      integer(int32)  :: ynum = 10
      integer(int32)  :: znum = 10

      ! physical parameter
      real(real64), allocatable :: DryDensity(:)  ! element-wise
      real(real64), allocatable :: WaterContent(:)! element-wise

      ! For deformation analysis
      real(real64), allocatable :: YoungModulus(:)! element-wise
      real(real64), allocatable :: PoissonRatio(:)! element-wise
      real(real64), allocatable :: Density(:)     ! element-wise
      real(real64), allocatable :: CarbonDiffusionCoefficient(:)
      real(real64), allocatable :: Stress(:, :, :)     ! Gauss point-wise
      real(real64), allocatable :: Displacement(:, :) ! node-wise, three dimensional

      real(real64), allocatable :: BoundaryTractionForce(:, :) ! node-wise, three dimensional
      real(real64), allocatable :: BoundaryDisplacement(:, :) ! node-wise, three dimensional

      ! physiological factor
      real(real64) :: R_d = 1.0d0 ! 暗呼吸速度, mincro-mol/m-2/s
      real(real64) :: default_CarbonDiffusionCoefficient = 0.0010d0 ! ソースの拡散係数 mincro-mol/m^2/m/s

      ! for growth simulation
      logical :: already_grown = .false.

      integer(int32)             ::  Division
      type(Root_), pointer ::  pRoot
   contains
      procedure, public :: Init => initRoot
      procedure, public :: rotate => rotateRoot

      procedure, public :: move => moveRoot

      procedure, pass :: connectRootRoot => connectRootRoot
      procedure, pass :: connectRootStem => connectRootStem

      generic :: connect => connectRootRoot, connectRootStem

      procedure, public :: rescale => rescaleRoot
      procedure, public :: resize => resizeRoot
      procedure, public :: fix => fixRoot

      ! check condition
      ! is it empty?
      procedure, public :: empty => emptyRoot
      procedure, public :: getCoordinate => getCoordinateRoot
      procedure, public :: getVolume => getVolumeRoot
      procedure, public :: getBiomass => getBiomassRoot

      procedure, public :: gmsh => gmshRoot
      procedure, public :: vtk => vtkRoot
      procedure, public :: msh => mshRoot
      procedure, public :: stl => stlRoot
      procedure, public :: ply => plyRoot
      procedure, public :: export => exportRoot

      ! MPI
      procedure, public :: sync => syncRoot

      procedure, public :: nn => nnRoot
      procedure, public :: ne => neRoot

      procedure, public :: remove => removeRoot
   end type

   interface operator(//)
      module procedure append_root_object_vector
   end interface

contains

! ########################################
   subroutine initRoot(obj, config, regacy, Thickness, length, width, MaxThickness, &
                       Maxlength, Maxwidth, rotx, roty, rotz, location, x_num, y_num, z_num)
      class(Root_), intent(inout) :: obj
      real(real64), optional, intent(in)::  Thickness, length, width
      real(real64), optional, intent(in)::  MaxThickness, Maxlength, MaxWidth
      real(real64), optional, intent(in)::  rotx, roty, rotz, location(3)
      logical, optional, intent(in) :: regacy
      character(*), optional, intent(in) :: config
      integer(int32), optional, intent(in) :: x_num, y_num, z_num
      type(IO_) :: Rootconf, f
      character(200) :: fn, conf, line
      integer(int32), allocatable :: buf(:)
      integer(int32) :: id, rmc, n, node_id, node_id2, elemid, blcount, i, j
      real(real64) :: loc(3)
      logical :: debug = .false.

      obj%minlength = 0.002d0
      obj%mindiameter = 0.001d0
      obj%maxlength = 0.07d0
      obj%maxdiameter = 0.01d0
      obj%xnum = 10
      obj%ynum = 10
      obj%znum = 10

      obj%xnum = input(default=obj%xnum, option=x_num)
      obj%ynum = input(default=obj%ynum, option=y_num)
      obj%znum = input(default=obj%znum, option=z_num)

      ! 節を生成するためのスクリプトを開く
!    if(.not.present(config) .or. index(config,".json")==0 )then
!        ! デフォルトの設定を生成
!        if(debug) print *, "New Root-configuration >> Rootconfig.json"
!        call Rootconf%open("rootconfig.json")
!        write(Rootconf%fh,*) '{'
!        write(Rootconf%fh,*) '   "type": "root",'
!        write(Rootconf%fh,*) '   "minlength": 0.002,'
!        write(Rootconf%fh,*) '   "mindiameter": 0.001,'
!        write(Rootconf%fh,*) '   "maxlength": 0.07,'
!        write(Rootconf%fh,*) '   "maxdiameter": 0.01,'
!        write(Rootconf%fh,*) '   "xnum": 10,'
!        write(Rootconf%fh,*) '   "ynum": 10,'
!        write(Rootconf%fh,*) '   "znum": 10'
!        write(Rootconf%fh,*) '}'
!        conf="rootconfig.json"
!        call Rootconf%close()
!    else
!        conf = config
!    endif
      if (present(config)) then
         conf = config
         call Rootconf%open(conf)
         blcount = 0
         do
            read (Rootconf%fh, '(a)') line
            if (debug) print *, line
            if (adjustl(line) == "{") then
               blcount = 1
               cycle
            end if
            if (adjustl(line) == "}") then
               exit
            end if

            if (blcount == 1) then

               if (index(line, "type") /= 0 .and. index(line, "root") == 0) then
                  print *, "ERROR: This config-file is not for Root"
                  return
               end if

               if (index(line, "maxlength") /= 0) then
                  ! 生育ステージ
                  rmc = index(line, ",")
                  ! カンマがあれば除く
                  if (rmc /= 0) then
                     line(rmc:rmc) = " "
                  end if
                  id = index(line, ":")
                  read (line(id + 1:), *) obj%maxlength
               end if

               if (index(line, "maxdiameter") /= 0) then
                  ! 種子の長さ
                  rmc = index(line, ",")
                  ! カンマがあれば除く
                  if (rmc /= 0) then
                     line(rmc:rmc) = " "
                  end if
                  id = index(line, ":")
                  read (line(id + 1:), *) obj%maxdiameter
               end if

               if (index(line, "minlength") /= 0) then
                  ! 生育ステージ
                  rmc = index(line, ",")
                  ! カンマがあれば除く
                  if (rmc /= 0) then
                     line(rmc:rmc) = " "
                  end if
                  id = index(line, ":")
                  read (line(id + 1:), *) obj%minlength
               end if

               if (index(line, "mindiameter") /= 0) then
                  ! 種子の長さ
                  rmc = index(line, ",")
                  ! カンマがあれば除く
                  if (rmc /= 0) then
                     line(rmc:rmc) = " "
                  end if
                  id = index(line, ":")
                  read (line(id + 1:), *) obj%mindiameter
               end if

               if (index(line, "xnum") /= 0) then
                  ! 種子の長さ
                  rmc = index(line, ",")
                  ! カンマがあれば除く
                  if (rmc /= 0) then
                     line(rmc:rmc) = " "
                  end if
                  id = index(line, ":")
                  read (line(id + 1:), *) obj%xnum
               end if

               if (index(line, "ynum") /= 0) then
                  ! 種子の長さ
                  rmc = index(line, ",")
                  ! カンマがあれば除く
                  if (rmc /= 0) then
                     line(rmc:rmc) = " "
                  end if
                  id = index(line, ":")
                  read (line(id + 1:), *) obj%ynum
               end if

               if (index(line, "znum") /= 0) then
                  ! 種子の長さ
                  rmc = index(line, ",")
                  ! カンマがあれば除く
                  if (rmc /= 0) then
                     line(rmc:rmc) = " "
                  end if
                  id = index(line, ":")
                  read (line(id + 1:), *) obj%znum
               end if
               cycle

            end if

         end do
         call Rootconf%close()
      end if

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
      call obj%FEMdomain%create(meshtype="rectangular3D", x_num=obj%xnum, y_num=obj%ynum, z_num=obj%znum, &
                                x_len=obj%mindiameter/2.0d0, y_len=obj%mindiameter/2.0d0, z_len=obj%minlength)

      ! initialize physical parameter
      obj%DryDensity = zeros(obj%FEMDomain%ne())
      obj%watercontent = zeros(obj%FEMDomain%ne())
      if (present(config)) then
         obj%DryDensity(:) = freal(rootconf%parse(config, key1="drydensity"))
         obj%watercontent(:) = freal(rootconf%parse(config, key1="watercontent"))
      end if
      ! <I>面に属する要素番号、節点番号、要素座標、節点座標のリストを生成
      obj%I_planeNodeID = obj%FEMdomain%mesh%getNodeList(zmax=0.0d0)
      obj%I_planeElementID = obj%FEMdomain%mesh%getElementList(zmax=0.0d0)

      ! <I>面に属する要素番号、節点番号、要素座標、節点座標のリストを生成
      obj%II_planeNodeID = obj%FEMdomain%mesh%getNodeList(zmin=obj%minlength)
      obj%II_planeElementID = obj%FEMdomain%mesh%getElementList(zmin=obj%minlength)

      buf = obj%FEMDomain%mesh%getNodeList( &
            xmin=obj%mindiameter/2.0d0 - obj%mindiameter/dble(obj%xnum)/2.0d0, &
            xmax=obj%mindiameter/2.0d0 + obj%mindiameter/dble(obj%xnum)/2.0d0, &
            ymin=obj%mindiameter/2.0d0 - obj%mindiameter/dble(obj%ynum)/2.0d0, &
            ymax=obj%mindiameter/2.0d0 + obj%mindiameter/dble(obj%ynum)/2.0d0, &
            zmax=0.0d0)
      !obj%A_PointNodeID = buf(1)
      obj%A_PointNodeID = median(buf)

      buf = obj%FEMDomain%mesh%getNodeList( &
            xmin=obj%mindiameter/2.0d0 - obj%mindiameter/dble(obj%xnum)/2.0d0, &
            xmax=obj%mindiameter/2.0d0 + obj%mindiameter/dble(obj%xnum)/2.0d0, &
            ymin=obj%mindiameter/2.0d0 - obj%mindiameter/dble(obj%ynum)/2.0d0, &
            ymax=obj%mindiameter/2.0d0 + obj%mindiameter/dble(obj%ynum)/2.0d0, &
            zmin=obj%minlength)
      !obj%B_PointNodeID = buf(1)
      obj%B_PointNodeID = median(buf)

      buf = obj%FEMDomain%mesh%getElementList( &
            xmin=obj%mindiameter/2.0d0 - obj%mindiameter/dble(obj%xnum)/2.0d0, &
            xmax=obj%mindiameter/2.0d0 + obj%mindiameter/dble(obj%xnum)/2.0d0, &
            ymin=obj%mindiameter/2.0d0 - obj%mindiameter/dble(obj%ynum)/2.0d0, &
            ymax=obj%mindiameter/2.0d0 + obj%mindiameter/dble(obj%ynum)/2.0d0, &
            zmax=0.0d0)
      !obj%A_PointElementID = buf(1)
      obj%A_PointElementID = median(buf)

      buf = obj%FEMDomain%mesh%getElementList( &
            xmin=obj%mindiameter/2.0d0 - obj%mindiameter/dble(obj%xnum)/2.0d0, &
            xmax=obj%mindiameter/2.0d0 + obj%mindiameter/dble(obj%xnum)/2.0d0, &
            ymin=obj%mindiameter/2.0d0 - obj%mindiameter/dble(obj%ynum)/2.0d0, &
            ymax=obj%mindiameter/2.0d0 + obj%mindiameter/dble(obj%ynum)/2.0d0, &
            zmin=obj%minlength)
      !obj%B_PointElementID = buf(1)
      obj%B_PointElementID = median(buf)

      call obj%FEMdomain%rotate(x=radian(180.0d0))
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

      if (present(regacy)) then
         if (regacy .eqv. .true.) then
            loc(:) = 0.0d0
            if (present(location)) then
               loc(:) = location(:)
            end if

            obj%Thickness = input(default=0.010d0, option=Thickness)
            obj%length = input(default=0.050d0, option=length)
            obj%width = input(default=0.010d0, option=width)

            obj%MaxThickness = input(default=0.50d0, option=MaxThickness)
            obj%Maxlength = input(default=10.0d0, option=Maxlength)
            obj%Maxwidth = input(default=0.50d0, option=Maxwidth)

            obj%outer_normal_bottom(:) = 0.0d0
            obj%outer_normal_bottom(1) = 1.0d0
            obj%outer_normal_top(:) = 0.0d0
            obj%outer_normal_top(1) = 1.0d0
            ! rotate
            obj%outer_normal_Bottom(:) = Rotation3D(vector=obj%outer_normal_bottom, rotx=rotx, roty=roty, rotz=rotz)
            obj%outer_normal_top(:) = Rotation3D(vector=obj%outer_normal_top, rotx=rotx, roty=roty, rotz=rotz)

            obj%center_bottom(:) = loc(:)
            obj%center_top(:) = obj%center_bottom(:) + obj%length*obj%outer_normal_bottom(:)
         end if
      end if

      obj%CarbonDiffusionCoefficient = obj%default_CarbonDiffusionCoefficient*ones(obj%femdomain%ne())

   end subroutine
! ########################################

! ########################################
   subroutine exportRoot(obj, FileName, RootID)
      class(Root_), intent(in)::obj
      character(*), intent(in) :: FileName
      integer(int32), optional, intent(inout) :: RootID
      real(real64) :: radius

      radius = 0.50d0*obj%width + 0.50d0*obj%Thickness

      open (13, file=FileName)
      write (13, '(A)') "//+"
      write (13, '(A)') 'SetFactory("OpenCASCADE");'
      write (13, *) "Cylinder(", input(default=1, option=RootID), ") = {", &
         obj%center_bottom(1), ",", obj%center_bottom(2), ",", obj%center_bottom(3), ",", &
         obj%center_top(1) - obj%center_bottom(1), ",", obj%center_top(2) - obj%center_bottom(2), ",", &
         obj%center_top(3) - obj%center_bottom(3), ",", &
         radius, ", 2*Pi};"
      close (13)
      RootID = RootID + 1

   end subroutine
! ########################################

! ########################################
   recursive subroutine rotateRoot(obj, x, y, z, reset)
      class(Root_), intent(inout) :: obj
      real(real64), optional, intent(in) :: x, y, z
      logical, optional, intent(in) :: reset
      real(real64), allocatable :: origin1(:), origin2(:), disp(:)

      if (present(reset)) then
         if (reset .eqv. .true.) then
            call obj%femdomain%rotate(-obj%rot_x, -obj%rot_y, -obj%rot_z)
            obj%rot_x = 0.0d0
            obj%rot_y = 0.0d0
            obj%rot_z = radian(180.0d0)
         end if
      end if

      origin1 = obj%getCoordinate("A")
      call obj%femdomain%rotate(x, y, z)
      obj%rot_x = obj%rot_x + input(default=0.0d0, option=x)
      obj%rot_y = obj%rot_y + input(default=0.0d0, option=y)
      obj%rot_z = obj%rot_z + input(default=0.0d0, option=z)
      origin2 = obj%getCoordinate("A")
      disp = origin1
      disp(:) = origin1(:) - origin2(:)
      call obj%femdomain%move(x=disp(1), y=disp(2), z=disp(3))

   end subroutine
! ########################################

! ########################################
   recursive subroutine moveRoot(obj, x, y, z, reset)
      class(Root_), intent(inout) :: obj
      real(real64), optional, intent(in) :: x, y, z
      logical, optional, intent(in) :: reset
      real(real64), allocatable :: origin1(:), origin2(:), disp(:)

      if (present(reset)) then
         if (reset .eqv. .true.) then
            call obj%femdomain%move(-obj%disp_x, -obj%disp_y, -obj%disp_z)
            obj%disp_x = 0.0d0
            obj%disp_y = 0.0d0
            obj%disp_z = 0.0d0
         end if
      end if

      call obj%femdomain%move(x, y, z)
      obj%disp_x = obj%disp_x + input(default=0.0d0, option=x)
      obj%disp_y = obj%disp_y + input(default=0.0d0, option=y)
      obj%disp_z = obj%disp_z + input(default=0.0d0, option=z)

   end subroutine
! ########################################

! ########################################
   subroutine connectRootRoot(obj, direct, Root)
      class(Root_), intent(inout) :: obj
      class(Root_), intent(inout) :: Root
      character(2), intent(in) :: direct
      real(real64), allocatable :: x1(:), x2(:), disp(:)

      if (direct == "->" .or. direct == "=>") then
         ! move obj to connect Root (Root is not moved.)
         x1 = obj%getCoordinate("A")
         x2 = Root%getCoordinate("B")
         disp = x2 - x1
         call obj%move(x=disp(1), y=disp(2), z=disp(3))
      end if
      if (direct == "<-" .or. direct == "<=") then
         ! move obj to connect Root (Root is not moved.)
         x1 = Root%getCoordinate("A")
         x2 = obj%getCoordinate("B")
         disp = x2 - x1
         call Root%move(x=disp(1), y=disp(2), z=disp(3))
      end if

   end subroutine
! ########################################

! ########################################
   subroutine connectRootStem(obj, direct, stem)
      class(Root_), intent(inout) :: obj
      class(Stem_), intent(inout) :: Stem
      character(2), intent(in) :: direct
      real(real64), allocatable :: x1(:), x2(:), disp(:)

      if (direct == "->" .or. direct == "=>") then
         ! move obj to connect Stem (Stem is not moved.)
         x1 = obj%getCoordinate("A")
         x2 = Stem%getCoordinate("A") ! 注意！stemのAにつなぐ
         disp = x2 - x1
         call obj%move(x=disp(1), y=disp(2), z=disp(3))
      end if

      if (direct == "<-" .or. direct == "<=") then
         ! move obj to connect Stem (Stem is not moved.)
         x1 = Stem%getCoordinate("A")
         x2 = obj%getCoordinate("A") ! 注意！rootのAにつなぐ
         disp = x2 - x1
         call Stem%move(x=disp(1), y=disp(2), z=disp(3))
      end if

   end subroutine
! ########################################

! ########################################
   function getCoordinateRoot(obj, nodetype) result(ret)
      class(Root_), intent(in) :: obj
      character(*), intent(in) :: nodetype
      real(real64), allocatable :: ret(:)
      integer(int32) :: dimnum, n, i

      dimnum = size(obj%femdomain%mesh%nodcoord, 2)

      allocate (ret(dimnum))
      ret(:) = 0.0d0
      if (nodetype == "A" .or. nodetype == "a") then
         n = size(obj%I_planeNodeID)
         do i = 1, n
            ret(:) = ret(:) + obj%femdomain%mesh%nodcoord(obj%I_planeNodeID(i), :)
         end do
         ret(:) = 1.0d0/dble(n)*ret(:)

         !ret = obj%femdomain%mesh%nodcoord(obj%A_PointNodeID,:)
      end if

      if (nodetype == "B" .or. nodetype == "b") then
         !ret = obj%femdomain%mesh%nodcoord(obj%B_PointNodeID,:)
         n = size(obj%II_planeNodeID)
         do i = 1, n
            ret(:) = ret(:) + obj%femdomain%mesh%nodcoord(obj%II_planeNodeID(i), :)
         end do
         ret(:) = 1.0d0/dble(n)*ret(:)
      end if
!    integer(int32) :: dimnum
!
!    dimnum = size(obj%femdomain%mesh%nodcoord,2)
!    allocate(ret(dimnum) )
!    if( nodetype=="A" .or. nodetype=="a")then
!        ret = obj%femdomain%mesh%nodcoord(obj%A_PointNodeID,:)
!    endif
!    if( nodetype=="B" .or. nodetype=="B")then
!        ret = obj%femdomain%mesh%nodcoord(obj%B_PointNodeID,:)
!    endif

   end function
! ########################################

   subroutine gmshRoot(obj, name)
      class(Root_), intent(inout) :: obj
      character(*), intent(in) ::name
      if (obj%femdomain%mesh%empty()) then
         return
      end if

      call obj%femdomain%gmsh(Name=name)
   end subroutine
! ########################################

   subroutine mshRoot(obj, name)
      class(Root_), intent(inout) :: obj
      character(*), intent(in) ::name
      if (obj%femdomain%mesh%empty()) then
         return
      end if

      call obj%femdomain%msh(Name=name)
   end subroutine
! ########################################

   subroutine vtkRoot(obj, name, field_name)
      class(Root_), intent(inout) :: obj
      character(*), intent(in) ::name
      character(*), optional, intent(in) ::field_name
      if (obj%femdomain%mesh%empty()) then
         return
      end if

      call obj%femdomain%vtk(Name=name, field=field_name)
   end subroutine
! ########################################

   subroutine stlRoot(obj, name)
      class(Root_), intent(inout) :: obj
      character(*), intent(in) ::name
      if (obj%femdomain%mesh%empty()) then
         return
      end if

      call obj%femdomain%stl(Name=name)
   end subroutine
! ########################################

   subroutine plyRoot(obj, name)
      class(Root_), intent(inout) :: obj
      character(*), intent(in) ::name
      if (obj%femdomain%mesh%empty()) then
         return
      end if

      call obj%femdomain%ply(Name=name)
   end subroutine

! ########################################
   subroutine resizeRoot(obj, x, y, z)
      class(Root_), optional, intent(inout) :: obj
      real(real64), optional, intent(in) :: x, y, z
      real(real64), allocatable :: origin1(:), origin2(:), disp(:)

      origin1 = obj%getCoordinate("A")
      call obj%femdomain%resize(x_len=x, y_len=y, z_len=z)
      origin2 = obj%getCoordinate("A")
      disp = origin1 - origin2
      call obj%move(x=disp(1), y=disp(2), z=disp(3))
   end subroutine
! ########################################

! ########################################
   subroutine rescaleRoot(obj, x, y, z)
      class(Root_), optional, intent(inout) :: obj
      real(real64), optional, intent(in) :: x, y, z
      real(real64), allocatable :: origin1(:), origin2(:), disp(:)

      origin1 = obj%getCoordinate("A")
      call obj%femdomain%resize(x_rate=x, y_rate=y, z_rate=z)
      origin2 = obj%getCoordinate("A")
      disp = origin1 - origin2
      call obj%move(x=disp(1), y=disp(2), z=disp(3))
   end subroutine
! ########################################

! ########################################
   subroutine fixRoot(obj, x, y, z)
      class(Root_), optional, intent(inout) :: obj
      real(real64), optional, intent(in) :: x, y, z
      real(real64), allocatable :: origin1(:), origin2(:), disp(:)

      origin1 = obj%getCoordinate("A")
      call obj%move(x=-origin1(1), y=-origin1(2), z=-origin1(3))
      call obj%move(x=x, y=y, z=z)
   end subroutine
! ########################################

   function getVolumeRoot(obj) result(ret)
      class(Root_), intent(in) :: obj
      real(real64) :: ret
      integer(int32) :: i, j

      ret = 0.0d0

      if (obj%femdomain%mesh%empty()) then
         return
      end if

      do i = 1, obj%femdomain%ne()
         ret = ret + obj%femdomain%getVolume(elem=i)
      end do

   end function

   function getBiomassRoot(obj) result(ret)
      class(Root_), intent(in) :: obj
      real(real64) :: ret
      integer(int32) :: i, j

      ret = 0.0d0

      if (obj%femdomain%mesh%empty()) then
         return
      end if

      do i = 1, obj%femdomain%ne()
         ret = ret + obj%femdomain%getVolume(elem=i)*obj%drydensity(i)
      end do

   end function
! ########################################
   function emptyRoot(obj) result(Root_is_empty)
      class(Root_), intent(in) :: obj
      logical :: Root_is_empty

      Root_is_empty = obj%femdomain%empty()

   end function
! ########################################

   subroutine syncRoot(obj, from, mpid)
      class(Root_), intent(inout) :: obj
      integer(int32), intent(in) :: from
      type(MPI_), intent(inout) :: mpid

      call obj%FEMDomain%sync(from=from, mpid=mpid)
      call mpid%bcast(from=from, val=obj%Thickness) !
      call mpid%bcast(from=from, val=obj%length) !
      call mpid%bcast(from=from, val=obj%width) !
      call mpid%bcast(from=from, val=obj%MaxThickness) !
      call mpid%bcast(from=from, val=obj%Maxlength) !
      call mpid%bcast(from=from, val=obj%Maxwidth) !
      call mpid%bcast(from=from, val=obj%maxdiameter) !
      call mpid%bcast(from=from, val=obj%mindiameter) !
      call mpid%bcast(from=from, val=obj%minlength) !
      call mpid%bcast(from=from, val=obj%rot_x) ! = 0.0d0
      call mpid%bcast(from=from, val=obj%rot_y) ! = 0.0d0
      call mpid%bcast(from=from, val=obj%rot_z) ! = 0.0d0
      call mpid%bcast(from=from, val=obj%disp_x) ! = 0.0d0
      call mpid%bcast(from=from, val=obj%disp_y) ! = 0.0d0
      call mpid%bcast(from=from, val=obj%disp_z) ! = 0.0d0
      call mpid%BcastMPIRealVecFixedSize(from=from, val=obj%center_bottom) !(3)
      call mpid%BcastMPIRealVecFixedSize(from=from, val=obj%center_top) !(3)
      call mpid%BcastMPIRealVecFixedSize(from=from, val=obj%radius_bottom) !(3)
      call mpid%BcastMPIRealVecFixedSize(from=from, val=obj%radius_top) !(3)
      call mpid%BcastMPIRealVecFixedSize(from=from, val=obj%outer_normal_bottom) !(3)
      call mpid%BcastMPIRealVecFixedSize(from=from, val=obj%outer_normal_top) !(3)

      call mpid%BcastMPIIntVecFixedSize(from=from, val=obj%EdgeNodeID)!(4)
      call mpid%BcastMPIIntVecFixedSize(from=from, val=obj%EdgeElemID)!(4)
      call mpid%bcast(from=from, val=obj%I_planeNodeID)!(:)
      call mpid%bcast(from=from, val=obj%I_planeElementID)!(:)
      call mpid%bcast(from=from, val=obj%II_planeNodeID)!(:)
      call mpid%bcast(from=from, val=obj%II_planeElementID)!(:)
      call mpid%bcast(from=from, val=obj%A_PointNodeID)!
      call mpid%bcast(from=from, val=obj%B_PointNodeID)!
      call mpid%bcast(from=from, val=obj%A_PointElementID)!
      call mpid%bcast(from=from, val=obj%B_PointElementID)!
      call mpid%bcast(from=from, val=obj%xnum)! = 10
      call mpid%bcast(from=from, val=obj%ynum)! = 10
      call mpid%bcast(from=from, val=obj%znum)! = 10

      ! physical parameter
      call mpid%bcast(from=from, val=obj%DryDensity)!(:)  ! element-wise
      call mpid%bcast(from=from, val=obj%WaterContent)!(:)! element-wise

      ! For deformation analysis
      call mpid%bcast(from=from, val=obj%YoungModulus)!(:)! element-wise
      call mpid%bcast(from=from, val=obj%PoissonRatio)!(:)! element-wise
      call mpid%bcast(from=from, val=obj%Density)!(:)     ! element-wise
      call mpid%bcast(from=from, val=obj%CarbonDiffusionCoefficient)!(:)     ! element-wise

      call mpid%bcast(from=from, val=obj%Stress)!(:,:,:)     ! Gauss point-wise
      call mpid%bcast(from=from, val=obj%Displacement)!(:,:) ! node-wise, three dimensional

      call mpid%bcast(from=from, val=obj%BoundaryTractionForce)!(:,:) ! node-wise, three dimensional
      call mpid%bcast(from=from, val=obj%BoundaryDisplacement)!(:,:) ! node-wise, three dimensional

      call mpid%bcast(from=from, val=obj%Division)

   end subroutine

! ######################################################################
   subroutine syncRootVector(obj, from, mpid)
      type(Root_), allocatable, intent(inout) :: obj(:)
      integer(int32), intent(in) :: from
      type(MPI_), intent(inout) :: mpid
      integer(int32) :: vec_size, i

      vec_size = 0
      if (mpid%myrank == from) then
         if (.not. allocated(obj)) then
            vec_size = -1
         else
            vec_size = size(obj)
         end if
      end if
      call mpid%bcast(from=from, val=vec_size)
      if (vec_size < 1) then
         return
      end if

      if (from /= mpid%myrank) then
         if (allocated(obj)) then
            deallocate (obj)
         end if
         allocate (obj(vec_size))
      end if

      do i = 1, vec_size
         call obj(i)%sync(from=from, mpid=mpid)
      end do

   end subroutine

! #######################################################
   subroutine removeRoot(this)
      class(Root_), intent(inout) :: this

      call this%FEMDomain%remove()
      this%Thickness = 0.0d0
      this%length = 0.0d0
      this%width = 0.0d0
      this%MaxThickness = 0.0d0
      this%Maxlength = 0.0d0
      this%Maxwidth = 0.0d0
      this%center_bottom(1:3) = 0.0d0
      this%center_top(1:3) = 0.0d0
      this%radius_bottom(1:3) = 0.0d0
      this%radius_top(1:3) = 0.0d0
      this%outer_normal_bottom(1:3) = 0.0d0
      this%outer_normal_top(1:3) = 0.0d0
      this%rot_x = 0.0d0
      this%rot_y = 0.0d0
      this%rot_z = 0.0d0
      this%disp_x = 0.0d0
      this%disp_y = 0.0d0
      this%disp_z = 0.0d0
      this%EdgeNodeID(1:4) = 0
      this%EdgeElemID(1:4) = 0
      this%maxdiameter = 0.0d0
      this%mindiameter = 0.0d0
      this%minlength = 0.0d0
      if (allocated(this%I_planeNodeID)) deallocate (this%I_planeNodeID)! (:)
      if (allocated(this%I_planeElementID)) deallocate (this%I_planeElementID)! (:)
      if (allocated(this%II_planeNodeID)) deallocate (this%II_planeNodeID)! (:)
      if (allocated(this%II_planeElementID)) deallocate (this%II_planeElementID)! (:)
      this%A_PointNodeID = 0
      this%B_PointNodeID = 0
      this%A_PointElementID = 0
      this%B_PointElementID = 0
      this%xnum = 10
      this%ynum = 10
      this%znum = 10

      ! physical parameter
      if (allocated(this%DryDensity)) deallocate (this%DryDensity)! (:)  ! element-wise
      if (allocated(this%WaterContent)) deallocate (this%WaterContent)! (:)! element-wise

      ! For deformation analysis
      if (allocated(this%YoungModulus)) deallocate (this%YoungModulus)! (:)! element-wise
      if (allocated(this%PoissonRatio)) deallocate (this%PoissonRatio)! (:)! element-wise
      if (allocated(this%Density)) deallocate (this%Density)! (:)     ! element-wise
      if (allocated(this%CarbonDiffusionCoefficient)) deallocate (this%CarbonDiffusionCoefficient)! (:)     ! element-wise
      if (allocated(this%Stress)) deallocate (this%Stress)! (:,:,:)     ! Gauss point-wise
      if (allocated(this%Displacement)) deallocate (this%Displacement)! (:,:) ! node-wise, three dimensional

      if (allocated(this%BoundaryTractionForce)) deallocate (this%BoundaryTractionForce)! (:,:) ! node-wise, three dimensional
      if (allocated(this%BoundaryDisplacement)) deallocate (this%BoundaryDisplacement)! (:,:) ! node-wise, three dimensional

      ! for growth simulation
      this%already_grown = .false.

      this%Division = 1
      if (associated(this%pRoot)) nullify (this%pRoot)

   end subroutine

   function nnRoot(this) result(ret)
      class(Root_), intent(in) :: this
      integer(int32) :: ret

      ret = this%femdomain%nn()
   end function

   function neRoot(this) result(ret)
      class(Root_), intent(in) :: this
      integer(int32) :: ret

      ret = this%femdomain%ne()
   end function

! ############################################################
   function append_root_object_vector(arg1, arg2) result(ret)
      type(root_), allocatable, intent(in) :: arg1(:), arg2(:)
      type(root_), allocatable :: ret(:)

      if (.not. allocated(arg1)) then
         if (.not. allocated(arg2)) then
            return
         else
            ret = arg2
         end if
      else
         if (.not. allocated(arg2)) then
            ret = arg1
            return
         else
            allocate (ret(size(arg1) + size(arg2)))
            ret(1:size(arg1)) = arg1(:)
            ret(size(arg1) + 1:) = arg2(:)
         end if
      end if

   end function
! ############################################################

end module
