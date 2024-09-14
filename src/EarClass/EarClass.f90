module EarClass
   use, intrinsic :: iso_fortran_env
   use KinematicClass
   use StemClass
   use FEMDomainClass

   implicit none

   type :: Ear_
      type(FEMDomain_)    ::  FEMDomain
      real(real64)        :: Length, Width, Angle
      type(Stem_), pointer ::  pStem
      integer(int32) :: division(1:3) = [6, 6, 30]

      integer(int32), allocatable  :: I_planeNodeID(:)
      integer(int32), allocatable  :: I_planeElementID(:)
      integer(int32), allocatable  :: II_planeNodeID(:)
      integer(int32), allocatable  :: II_planeElementID(:)
      integer(int32)  :: A_PointNodeID
      integer(int32)  :: B_PointNodeID
      integer(int32)  :: C_PointNodeID
      integer(int32)  :: D_PointNodeID

      integer(int32)  :: A_PointElementID
      integer(int32)  :: B_PointElementID

      real(real64) :: disp_x
      real(real64) :: disp_y
      real(real64) :: disp_z

      ! For deformation analysis
      real(real64), allocatable :: YoungModulus(:)! element-wise
      real(real64), allocatable :: PoissonRatio(:)! element-wise
      real(real64), allocatable :: Density(:)     ! element-wise
      real(real64), allocatable :: Stress(:, :, :)     ! Gauss point-wise
      real(real64), allocatable :: Displacement(:, :) ! node-wise, three dimensional

   contains
      procedure, public :: Init => initEar
      procedure, public :: move => moveEar
      procedure, public :: rotate => rotateEar
      procedure, public :: getCoordinate => getCoordinateEar
      procedure, public :: connect => connectEar
      procedure, public :: vtk => vtkEar
      procedure, public :: stl => stlEar
   end type

contains

! #####################################################
   subroutine initEar(this, Length, Width, Angle, debug, x_num, y_num, z_num, Leaf_angle_z)
      class(Ear_), intent(inout) :: this
      real(real64), intent(in) :: Length, width, Angle
      real(real64), optional, intent(in) ::  Leaf_angle_z
      integer(int32), optional, intent(in) :: x_num, y_num, z_num
      type(Math_) :: math
      type(Random_) :: random
      real(real64) :: x, y, z, r, theta, alpha, dist_val
      integer(int32) :: i
      integer(int32), allocatable :: buf(:)
      logical, optional, intent(in) :: debug

      real(real64) :: center_coord(1:3)

      this%Length = length
      this%Width = Width
      this%Angle = Angle
      this%division(1) = input(default=this%division(1), option=x_num)
      this%division(2) = input(default=this%division(2), option=y_num)
      this%division(3) = input(default=this%division(3), option=z_num)

      call this%FEMDomain%create("Cube3D", &
                                 x_num=this%division(1), y_num=this%division(2), z_num=this%division(3))
      call this%FEMDomain%resize(x=Width, y=width, z=Length)

      x = 0.50d0*(Width)
      y = 0.50d0*(Width)
      z = this%FEMDomain%zmin()

      call this%FEMDomain%move(x=-x, y=-y, z=-z)

      do i = 1, this%FEMDomain%nn()
         z = this%FEMDomain%mesh%nodcoord(i, 3)
         if (z < Length*1.0d0/3.0d0) then
            theta = z/(Length*1.0d0/3.0d0)
            alpha = 0.30d0 + (1.0d0 - 0.30d0)*theta !0.3 at theta=0.0, 1.0 at theta = 1.0
            this%FEMDomain%mesh%nodcoord(i, 1:2) = this%FEMDomain%mesh%nodcoord(i, 1:2)*alpha
         else
            theta = (z - Length*1.0d0/3.0d0)/(Length*2.0d0/3.0d0)
            alpha = 1.00d0 + (0.30d0 - 1.0d0)*theta !1.0 at theta=0.0, 0.30 at theta = 1.0
            this%FEMDomain%mesh%nodcoord(i, 1:2) = this%FEMDomain%mesh%nodcoord(i, 1:2)*alpha
         end if
      end do

!    ! <I>面に属する要素番号、節点番号、要素座標、節点座標のリストを生成
!    this%I_planeNodeID = this%FEMdomain%mesh%getNodeList(zmax=0.0d0)
!    this%I_planeElementID = this%FEMdomain%mesh%getElementList(zmax=0.0d0)
!
!    ! <I>面に属する要素番号、節点番号、要素座標、節点座標のリストを生成
!    this%II_planeNodeID = this%FEMdomain%mesh%getNodeList(zmin=this%length)
!    this%II_planeElementID = this%FEMdomain%mesh%getElementList(zmin=this%length)
!
!
!
!    center_coord(1) = sum(this%FEMDomain%mesh%nodcoord(this%I_planeNodeID(:),1) )&
!        /size(this%I_planeNodeID)
!    center_coord(2) = sum(this%FEMDomain%mesh%nodcoord(this%I_planeNodeID(:),2) )&
!        /size(this%I_planeNodeID)
!    center_coord(3) = sum(this%FEMDomain%mesh%nodcoord(this%I_planeNodeID(:),3) )&
!        /size(this%I_planeNodeID)
!
!    dist_val = norm(this%FEMDomain%mesh%nodcoord(this%I_planeNodeID(1),:)-center_coord)
!    this%A_PointNodeID = this%I_planeNodeID(1)
!
!    do i=2, size(this%I_planeNodeID)
!        if(  norm(this%FEMDomain%mesh%nodcoord(this%I_planeNodeID(i),:)-center_coord) < dist_val  )then
!            this%A_PointNodeID = this%I_planeNodeID(i)
!            dist_val = norm(this%FEMDomain%mesh%nodcoord(this%I_planeNodeID(i),:)-center_coord)
!        endif
!    enddo
!
!
!    center_coord(1) = sum(this%FEMDomain%mesh%nodcoord(this%II_planeNodeID(:),1) )&
!        /size(this%I_planeNodeID)
!    center_coord(2) = sum(this%FEMDomain%mesh%nodcoord(this%II_planeNodeID(:),2) )&
!        /size(this%I_planeNodeID)
!    center_coord(3) = sum(this%FEMDomain%mesh%nodcoord(this%II_planeNodeID(:),3) )&
!        /size(this%I_planeNodeID)
!
!    dist_val = norm(this%FEMDomain%mesh%nodcoord(this%II_planeNodeID(1),:)-center_coord)
!    this%B_PointNodeID = this%II_planeNodeID(1)
!
!    do i=2, size(this%II_planeNodeID)
!        if(  norm(this%FEMDomain%mesh%nodcoord(this%II_planeNodeID(i),:)-center_coord) < dist_val  )then
!            this%B_PointNodeID = this%II_planeNodeID(i)
!            dist_val = norm(this%FEMDomain%mesh%nodcoord(this%II_planeNodeID(i),:)-center_coord)
!        endif
!    enddo
!
!
!
!    center_coord(1) = maxval(this%FEMDomain%mesh%nodcoord(this%I_planeNodeID(:),1) )
!
!    center_coord(2) = sum(this%FEMDomain%mesh%nodcoord(this%I_planeNodeID(:),2) )&
!        /size(this%I_planeNodeID)
!
!    center_coord(3) = sum(this%FEMDomain%mesh%nodcoord(this%I_planeNodeID(:),3) )&
!        /size(this%I_planeNodeID)
!
!    dist_val = norm(this%FEMDomain%mesh%nodcoord(this%I_planeNodeID(1),:)-center_coord)
!    this%C_PointNodeID = this%I_planeNodeID(1)
!
!    do i=2, size(this%I_planeNodeID)
!        if(  norm(this%FEMDomain%mesh%nodcoord(this%I_planeNodeID(i),:)-center_coord) < dist_val  )then
!            this%C_PointNodeID = this%I_planeNodeID(i)
!            dist_val = norm(this%FEMDomain%mesh%nodcoord(this%I_planeNodeID(i),:)-center_coord)
!        endif
!    enddo
!
!
!
!    center_coord(1) = sum(this%FEMDomain%mesh%nodcoord(this%II_planeNodeID(:),1) )&
!        /size(this%II_planeNodeID)
!
!    center_coord(2) = maxval(this%FEMDomain%mesh%nodcoord(this%II_planeNodeID(:),2) )
!
!
!    center_coord(3) = sum(this%FEMDomain%mesh%nodcoord(this%II_planeNodeID(:),3) )&
!        /size(this%II_planeNodeID)
!
!    dist_val = norm(this%FEMDomain%mesh%nodcoord(this%II_planeNodeID(1),:)-center_coord)
!    this%D_PointNodeID = this%II_planeNodeID(1)
!
!    do i=2, size(this%II_planeNodeID)
!        if(  norm(this%FEMDomain%mesh%nodcoord(this%II_planeNodeID(i),:)-center_coord) < dist_val  )then
!            this%D_PointNodeID = this%II_planeNodeID(i)
!            dist_val = norm(this%FEMDomain%mesh%nodcoord(this%II_planeNodeID(i),:)-center_coord)
!        endif
!    enddo
!
!    buf    = this%FEMDomain%mesh%getElementList(&
!        xmin=this%width/2.0d0 - this%width/dble(this%division(1) )/2.0d0 ,&
!        xmax=this%width/2.0d0 + this%width/dble(this%division(1) )/2.0d0 ,&
!        ymin=this%width/2.0d0 - this%width/dble(this%division(2) )/2.0d0 ,&
!        ymax=this%width/2.0d0 + this%width/dble(this%division(2) )/2.0d0 ,&
!        zmax=0.0d0)
!    !this%A_PointElementID = buf(1)
!    this%A_PointElementID = median(buf)
!
!
!    buf    = this%FEMDomain%mesh%getElementList(&
!        xmin=this%width/2.0d0 - this%width/dble(this%division(1) )/2.0d0 ,&
!        xmax=this%width/2.0d0 + this%width/dble(this%division(1) )/2.0d0 ,&
!        ymin=this%width/2.0d0 - this%width/dble(this%division(2) )/2.0d0 ,&
!        ymax=this%width/2.0d0 + this%width/dble(this%division(2) )/2.0d0 ,&
!        zmin=this%length)
!
!    !this%B_PointElementID = buf(1)
!    this%B_PointElementID = median(buf)
!
!    if(debug) print *, this%A_PointNodeID
!    if(debug) print *, this%B_PointNodeID
!    if(debug) print *, this%A_PointElementID
!    if(debug) print *, this%B_PointElementID
!
      this%A_PointNodeID = (this%division(1) + 1)*(this%division(2) + 1)/2
      this%B_PointNodeID = this%FEMDomain%nn() - (this%division(1) + 1)*(this%division(2) + 1)/2

      if (present(Leaf_angle_z)) then
         call this%FEMDomain%rotate(x=radian(Angle), z=Leaf_angle_z)
      else
         call this%FEMDomain%rotate(x=radian(Angle), z=math%PI*2.0d0*random%random())
      end if

   end subroutine
! #####################################################

! #####################################################
   subroutine vtkEar(this, name)
      class(Ear_), intent(inout) :: this
      character(*), intent(in) :: name

      call this%FEMDomain%vtk(name=name)

   end subroutine
! #####################################################

! ########################################
   recursive subroutine moveEar(this, x, y, z, reset)
      class(Ear_), intent(inout) :: this
      real(real64), optional, intent(in) :: x, y, z
      logical, optional, intent(in) :: reset
      real(real64), allocatable :: origin1(:), origin2(:), disp(:)

      if (present(reset)) then
         if (reset .eqv. .true.) then
            call this%femdomain%move(-this%disp_x, -this%disp_y, -this%disp_z)
            this%disp_x = 0.0d0
            this%disp_y = 0.0d0
            this%disp_z = 0.0d0
         end if
      end if

      call this%femdomain%move(x, y, z)
      this%disp_x = this%disp_x + input(default=0.0d0, option=x)
      this%disp_y = this%disp_y + input(default=0.0d0, option=y)
      this%disp_z = this%disp_z + input(default=0.0d0, option=z)

   end subroutine
! ########################################

! ########################################
   recursive subroutine rotateEar(this, x, y, z)
      class(Ear_), intent(inout) :: this
      real(real64), optional, intent(in) :: x, y, z

      call this%FEMDomain%rotate(x=x, y=y, z=z)
   end subroutine
! ########################################

! ########################################
   function getCoordinateEar(this, nodetype) result(ret)
      class(Ear_), intent(in) :: this
      character(*), intent(in) :: nodetype
      real(real64), allocatable :: ret(:)
      integer(int32) :: dimnum, n, i
      integer(int32), allocatable :: buf(:)

      dimnum = size(this%femdomain%mesh%nodcoord, 2)

      allocate (ret(dimnum))
      ret(:) = 0.0d0
      if (nodetype == "A" .or. nodetype == "a") then

         ! 20220701 this may be correct
         ret = this%femdomain%mesh%nodcoord(this%A_PointNodeID, :)
         return

         n = size(this%I_planeNodeID)
         if (n == 0) then
            print *, "ERROR >> getCoordinateEar >> size(this%I_planeNodeID) = 0"
         end if
         if (.not. allocated(this%I_planeNodeID)) then

            print *, "ERROR >> getCoordinateEar >> .not. allocated(this%I_planeNodeID) "

         end if
         do i = 1, n
            ret(:) = ret(:) + this%femdomain%mesh%nodcoord(this%I_planeNodeID(i), :)
         end do
         ret(:) = 1.0d0/dble(n)*ret(:)

         !ret = this%femdomain%mesh%nodcoord(this%A_PointNodeID,:)
      end if

      if (nodetype == "B" .or. nodetype == "b") then
         !ret = this%femdomain%mesh%nodcoord(this%B_PointNodeID,:)

         ! 20220701 this may be correct
         ret = this%femdomain%mesh%nodcoord(this%B_PointNodeID, :)
         return

         n = size(this%II_planeNodeID)
         if (n == 0) then
            print *, "ERROR >> getCoordinateEar >> size(this%II_planeNodeID) = 0"
         end if
         if (.not. allocated(this%I_planeNodeID)) then

            print *, "ERROR >> getCoordinateEar >> .not. allocated(this%II_planeNodeID) "

         end if
         do i = 1, n
            ret(:) = ret(:) + this%femdomain%mesh%nodcoord(this%II_planeNodeID(i), :)
         end do
         ret(:) = 1.0d0/dble(n)*ret(:)
      end if

   end function
! ########################################

! ########################################

   subroutine connectEar(obj, direct, stem)
      class(Ear_), intent(inout) :: obj
      class(Stem_), intent(inout) :: stem

      character(2), intent(in) :: direct
      real(real64), allocatable :: x1(:), x2(:), disp(:)

      if (direct == "->" .or. direct == "=>") then
         ! move obj to connect stem (stem is not moved.)
         x1 = obj%getCoordinate("A")
         x2 = stem%getCoordinate("B")
         disp = x2 - x1
         call obj%move(x=disp(1), y=disp(2), z=disp(3))
      end if

      if (direct == "<-" .or. direct == "<=") then
         ! move obj to connect stem (stem is not moved.)

         x1 = stem%getCoordinate("A")
         x2 = obj%getCoordinate("B")
         disp = x2 - x1
         call stem%move(x=disp(1), y=disp(2), z=disp(3))
      end if
   end subroutine

! ########################################

! ##############################################

   subroutine stlEar(obj, name)
      class(Ear_), intent(inout) :: obj
      character(*), intent(in) ::name
      if (obj%femdomain%mesh%empty()) then
         return
      end if

      call obj%femdomain%stl(Name=name)
   end subroutine
! ########################################

end module
