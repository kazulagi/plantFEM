
module RangeClass
   use MathClass
   use ArrayClass
   implicit none

   real(real64), parameter :: PF_RANGE_INFTY = dble(1.0e+14)

   type :: Range_
      real(real64) :: x_range(1:2)
      real(real64) :: y_range(1:2)
      real(real64) :: z_range(1:2)
      real(real64) :: t_range(1:2)
      logical :: x_substituted(1:2)
      logical :: y_substituted(1:2)
      logical :: z_substituted(1:2)
      logical :: t_substituted(1:2)

      ! for sphere
      logical      :: sphere_is_active = .false.
      real(real64) :: sphere_center(1:3)
      real(real64) :: sphere_radius

      ! for cylinder
      logical      :: cylinder_is_active = .false.
      real(real64) :: cylinder_p1(1:3)
      real(real64) :: cylinder_p2(1:3)
      real(real64) :: cylinder_radius
      
      ! for cone
      logical      :: cone_is_active = .false.
      real(real64) :: cone_p1(1:3)
      real(real64) :: cone_p2(1:3)
      real(real64) :: cone_radius(1:2)
      
      ! for multiple 

   contains
      procedure,public :: init => initRange
      procedure,public :: set => setRange
      procedure,public :: get => getRange

      procedure,public :: set_x => set_xRange
      procedure,public :: set_y => set_yRange
      procedure,public :: set_z => set_zRange
      procedure,public :: set_t => set_tRange

      procedure,public :: inside => insideRange

      procedure,public :: lx => lx_RangeClass
      procedure,public :: ly => ly_RangeClass
      procedure,public :: lz => lz_RangeClass


      !procedure,public :: getSubrange    => getSubrangeRange
      !procedure,public :: getSubrangeIdx => getSubrangeIdxRange
   end type

   interface to_range
      module procedure :: to_range_int32, to_range_real64, to_range_real64_rect,&
         to_range_real64_sphere,to_range_real64_cylinder,to_range_real64_Cone
   end interface
   
   interface print
      module procedure :: printRange
   end interface print

   interface operator(.in.)
      module procedure :: in_detect_by_range_int32, in_detect_by_range_real64, &
            in_int32_int32vector,in_detect_by_range_xyz_real64
   end interface

   interface operator(.and.)
      module procedure :: and_rect_real64
   end interface

contains

   ! ###############################################################
   function in_int32_int32vector(intval, intlist) result(ret)
      integer(int32), intent(in) :: intval, intlist(:)
      integer(int32) :: i
      logical :: ret

      ret = .false.
      do i = 1, size(intlist)
         if (intlist(i) == intval) then
            ret = .true.
            return
         end if
      end do

   end function
   ! ###############################################################


   ! ###############################################################
   function in_detect_by_range_int32(intval, in_range) result(ret)
      integer(int32), intent(in) :: intval
      type(Range_), intent(in) :: in_range
      logical :: ret

      ret = (in_range%x_range(1) <= dble(intval)) .and. (dble(intval) <= in_range%x_range(2))

   end function
   ! ###############################################################



   ! ###############################################################
   function in_detect_by_range_real64(real64val, in_range) result(ret)
      real(real64), intent(in) :: real64val
      type(Range_), intent(in) :: in_range
      logical :: ret

      ret = (in_range%x_range(1) <= real64val) .and. (real64val <= in_range%x_range(2))

   end function
   ! ###############################################################



   ! ###############################################################
   function in_detect_by_range_xyz_real64(real64val, in_range) result(ret)
      real(real64), intent(in) :: real64val(:)
      real(real64) :: x(1:3),y(1:3),tot_l,l,nvec(1:3),this_R,cone_r_limit,theta
      type(Range_), intent(in) :: in_range
      logical :: ret
      x(:) = 0.0d0

      ! <SPHERE>
      if(in_range%sphere_is_active )then
         x(1:size(real64Val)) = real64Val(:)
         ret = (in_range%sphere_radius &
            >= sqrt(dot_product(in_range%sphere_center-x,in_range%sphere_center-x) )  ) 
         return
      endif

      ! <CYLINDER>
      if(in_range%cylinder_is_active )then
         x(1:size(real64Val)) = real64Val(:)

         ! p1                        p2
         ! *-------------------------*
         !
         !             * x
         !
         tot_l = sqrt(dot_product(&
            in_range%cylinder_p2-in_range%cylinder_p1,&
            in_range%cylinder_p2-in_range%cylinder_p1) )
         
         if (tot_l == 0.0d0)then
            if (dot_product(x-in_range%cylinder_p1,x-in_range%cylinder_p1)==0.0d0 )then
               ret = .True.
            else
               ret = .False.
            endif
         else
            nvec = (in_range%cylinder_p2-in_range%cylinder_p1)/tot_l
            l = dot_product(x-in_range%cylinder_p1,in_range%cylinder_p2-in_range%cylinder_p1)/tot_L
            
            if (l < 0.0d0)then
               ret = .false.
            elseif (l > tot_L)then
               ret = .false.
            else
               y = in_range%cylinder_p1(:) + l*nvec(:)
               this_R = sqrt(dot_product(x-y,x-y))
               ret = (in_range%cylinder_radius >= this_R ) 
            endif
         endif
         return
      endif

      !<CONE>
      if(in_range%cone_is_active )then
         x(1:size(real64Val)) = real64Val(:)

         ! p1                        p2
         ! *-------------------------*
         !
         !             * x
         !
         tot_l = sqrt(dot_product(&
            in_range%cone_p2-in_range%cone_p1,&
            in_range%cone_p2-in_range%cone_p1) )
         
         if (tot_l == 0.0d0)then
            if (dot_product(x-in_range%cone_p1,x-in_range%cone_p1)==0.0d0 )then
               ret = .True.
            else
               ret = .False.
            endif
         else
            nvec = (in_range%cone_p2-in_range%cone_p1)/tot_l
            l = dot_product(x-in_range%cone_p1,in_range%cone_p2-in_range%cone_p1)/tot_L
            
            theta = l/tot_l

            cone_r_limit = (1.0d0-theta)*in_range%cone_radius(1) + (theta)*in_range%cone_radius(2)

            if (l < 0.0d0)then
               ret = .false.
            elseif (l > tot_L)then
               ret = .false.         
            else
               y = in_range%cone_p1(:) + l*nvec(:)
               this_R = sqrt(dot_product(x-y,x-y))
               ret = (cone_r_limit >= this_R ) 
            endif
         endif
         return
      endif


      if(size(real64val)==2 )then
         ret =    ((in_range%x_range(1) <= real64val(1)) .and. (real64val(1)  <= in_range%x_range(2))) &
            .and. ((in_range%y_range(1) <= real64val(2)) .and. (real64val(2) <= in_range%y_range(2)))
      elseif(size(real64val)==3 )then
         ret =    ((in_range%x_range(1) <= real64val(1)) .and. (real64val(1) <= in_range%x_range(2))) &
            .and. ((in_range%y_range(1) <= real64val(2)) .and. (real64val(2) <= in_range%y_range(2))) &
            .and. ((in_range%z_range(1) <= real64val(3)) .and. (real64val(3) <= in_range%z_range(2))) 
      else
         ret = (in_range%x_range(1) <= real64val(1)) .and. (real64val(1) <= in_range%x_range(2))
      endif



   end function
   ! ###############################################################




   function to_range_int32(from_to) result(ret_range)
      type(Range_) :: ret_range
      integer(int32), intent(in) :: from_to(1:2)

      call ret_range%init()
      ret_range%x_range(1:2) = dble(from_to)
      ret_range%x_substituted(:) = .True.

   end function

   function to_range_real64(from_to) result(ret_range)
      type(Range_) :: ret_range
      real(real64), intent(in) :: from_to(:)

      call ret_range%init()
      ret_range%x_range(1:2) = from_to
      ret_range%x_substituted(:) = .True.

   end function

   function to_range_real64_rect(x_min, y_min, z_min, x_max, y_max, z_max) result(ret_range)
      type(Range_) :: ret_range
      real(real64), optional, intent(in) :: x_min, y_min, z_min, x_max, y_max, z_max

      call ret_range%init()
      if (present(x_min)) then
         ret_range%x_range(1) = x_min
         ret_range%x_substituted(1) = .True.
      end if
      if (present(y_min)) then
         ret_range%y_range(1) = y_min
         ret_range%y_substituted(1) = .True.
      end if
      if (present(z_min)) then
         ret_range%z_range(1) = z_min
         ret_range%z_substituted(1) = .True.
      end if
      if (present(x_max)) then
         ret_range%x_range(2) = x_max
         ret_range%x_substituted(2) = .True.
      end if
      if (present(y_max)) then
         ret_range%y_range(2) = y_max
         ret_range%y_substituted(2) = .True.
      end if
      if (present(z_max)) then
         ret_range%z_range(2) = z_max
         ret_range%z_substituted(2) = .True.
      end if

   end function

! #########################################################
   subroutine initRange(this, MaxRange)
      class(Range_), intent(inout) :: this
      real(real64), optional, intent(in) :: MaxRange

      if (present(MaxRange)) then
         this%x_range = [-MaxRange, MaxRange]
         this%y_range = [-MaxRange, MaxRange]
         this%z_range = [-MaxRange, MaxRange]
         this%t_range = [-MaxRange, MaxRange]
      else
         this%x_range = [-PF_RANGE_INFTY, PF_RANGE_INFTY]
         this%y_range = [-PF_RANGE_INFTY, PF_RANGE_INFTY]
         this%z_range = [-PF_RANGE_INFTY, PF_RANGE_INFTY]
         this%t_range = [-PF_RANGE_INFTY, PF_RANGE_INFTY]
      end if

      this%x_substituted = [.false., .false.]
      this%y_substituted = [.false., .false.]
      this%z_substituted = [.false., .false.]
      this%t_substituted = [.false., .false.]

   end subroutine
! #########################################################

   subroutine setRange(this, x_min, x_max, y_min, y_max, z_min, z_max)
      class(Range_), intent(inout) :: this
      real(real64), optional, intent(in) :: x_min, x_max
      real(real64), optional, intent(in) :: y_min, y_max
      real(real64), optional, intent(in) :: z_min, z_max

      call this%set_x(x_min=x_min, x_max=x_max)
      call this%set_y(y_min=y_min, y_max=y_max)
      call this%set_z(z_min=z_min, z_max=z_max)

   end subroutine

! #########################################################
   subroutine set_xRange(this, x_min, x_max)
      class(Range_), intent(inout) :: this
      real(real64), optional, intent(in) :: x_min, x_max

      if (present(x_min)) then
         this%x_range(1) = x_min
         this%x_substituted(1) = .True.
      end if

      if (present(x_max)) then
         this%x_range(2) = x_max
         this%x_substituted(2) = .True.
      end if

   end subroutine
! #########################################################

! #########################################################
   subroutine set_yRange(this, y_min, y_max)
      class(Range_), intent(inout) :: this
      real(real64), optional, intent(in) :: y_min, y_max

      if (present(y_min)) then
         this%y_range(1) = y_min
         this%y_substituted(1) = .True.
      end if

      if (present(y_max)) then
         this%y_range(2) = y_max
         this%y_substituted(2) = .True.
      end if

   end subroutine
! #########################################################

! #########################################################
   subroutine set_zRange(this, z_min, z_max)
      class(Range_), intent(inout) :: this
      real(real64), optional, intent(in) :: z_min, z_max

      if (present(z_min)) then
         this%z_range(1) = z_min
         this%z_substituted(1) = .True.
      end if

      if (present(z_max)) then
         this%z_range(2) = z_max
         this%z_substituted(2) = .True.
      end if

   end subroutine
! #########################################################

! #########################################################
   subroutine set_tRange(this, t_min, t_max)
      class(Range_), intent(inout) :: this
      real(real64), optional, intent(in) :: t_min, t_max

      if (present(t_min)) then
         this%t_range(1) = t_min
         this%t_substituted(1) = .True.
      end if

      if (present(t_max)) then
         this%t_range(2) = t_max
         this%t_substituted(2) = .True.
      end if

   end subroutine
! #########################################################

! #########################################################
   function getRange(this, range_type) result(min_and_max)
      class(Range_), intent(inout) :: this
      character(1), intent(in) :: range_type
      real(real64) :: min_and_max(2)

      if (range_type == "x" .or. range_type == "X") then
         min_and_max = this%x_range
      end if

      if (range_type == "y" .or. range_type == "Y") then
         min_and_max = this%y_range
      end if

      if (range_type == "z" .or. range_type == "Z") then
         min_and_max = this%z_range
      end if

      if (range_type == "t" .or. range_type == "T") then
         min_and_max = this%t_range
      end if

   end function
! #########################################################

   pure function insideRange(this, point) result(inside_is_true)
      class(Range_), intent(in) :: this
      real(real64), intent(in) :: point(:)
      integer(int32) :: i
      logical :: inside_is_true

      inside_is_true = .true.
      do i = 1, size(point)
         if (i == 1) then
            if (this%x_range(1) >= point(i) .or. this%x_range(2) <= point(i)) then
               inside_is_true = .false.
               return
            end if
         elseif (i == 2) then
            if (this%y_range(1) >= point(i) .or. this%y_range(2) <= point(i)) then
               inside_is_true = .false.
               return
            end if
         elseif (i == 3) then
            if (this%z_range(1) >= point(i) .or. this%z_range(2) <= point(i)) then
               inside_is_true = .false.
               return
            end if
         elseif (i == 4) then
            if (this%t_range(1) >= point(i) .or. this%t_range(2) <= point(i)) then
               inside_is_true = .false.
               return
            end if
         end if
      end do

   end function
! #########################################################
   subroutine printRange(range)
      type(Range_),intent(in) :: range

      print *, "[",range%x_range(1),",",range%x_range(2),"]"
      print *, "[",range%y_range(1),",",range%y_range(2),"]"
      print *, "[",range%z_range(1),",",range%z_range(2),"]"
   end subroutine

! #########################################################
elemental function and_rect_real64(range1,range2) result(ret)
   type(Range_),intent(in) :: range1,range2
   type(Range_) :: ret

   ret%x_range(1) = maxval([range1%x_range(1),range2%x_range(1)])
   ret%x_range(2) = minval([range1%x_range(2),range2%x_range(2)])
   ret%y_range(1) = maxval([range1%y_range(1),range2%y_range(1)])
   ret%y_range(2) = minval([range1%y_range(2),range2%y_range(2)])
   ret%z_range(1) = maxval([range1%z_range(1),range2%z_range(1)])
   ret%z_range(2) = minval([range1%z_range(2),range2%z_range(2)])
   
   if(ret%x_range(1) > ret%x_range(2))then
      ret%x_range(:) = 0.0d0 
   endif
   if(ret%y_range(1) > ret%y_range(2))then
      ret%y_range(:) = 0.0d0 
   endif
   if(ret%z_range(1) > ret%z_range(2))then
      ret%z_range(:) = 0.0d0 
   endif

end function


!function getSubrange(this,dim,n) result(ret)
!   class(Range_),intent(in) :: this
!   integer(int32),intent(in) :: dim ! number of dimension (e.g. x-y-z => 3)
!   integer(int32),intent(in) :: n   ! number of division (e.g. n=2 for binomial search)
!   type(Range_),allocatable :: ret(:)
!   real(real64) :: dr
!   integer(int32) :: i,j,k,itr,n,idx
!
!   if(dim==3)then
!      allocate(ret(8))
!      idx = 0
!      do x_id=1,n
!         do y_id=1,n
!            do z_id=1,n
!               idx = idx + 1
!               dr = (this%x_range(2)-this%x_range(1))/dble(n)
!               ret(idx)%x_range(1) = this%x_range(1) + dr*dble(x_id-1)
!               ret(idx)%x_range(2) = this%x_range(2) + dr*dble(x_id  )
!               dr = (this%y_range(2)-this%y_range(1))/dble(n)
!               ret(idx)%y_range(1) = this%y_range(1) + dr*dble(y_id-1)
!               ret(idx)%y_range(2) = this%y_range(2) + dr*dble(y_id  )
!               dr = (this%z_range(2)-this%z_range(1))/dble(n)
!               ret(idx)%z_range(1) = this%z_range(1) + dr*dble(z_id-1)
!               ret(idx)%z_range(2) = this%z_range(2) + dr*dble(z_id  )
!            enddo
!         enddo
!      enddo
!      return
!   else
!      ! not implemented yet.
!      return
!   endif
!
!
!
!end function

! ######################################################
function lx_RangeClass(this) result(ret)
   class(Range_),intent(in) :: this
   real(real64) :: ret

   ret = this%x_range(2) - this%x_range(1) 

end function
! ######################################################


! ######################################################
function ly_RangeClass(this) result(ret)
   class(Range_),intent(in) :: this
   real(real64) :: ret

   ret = this%y_range(2) - this%y_range(1) 
   
end function
! ######################################################


! ######################################################
function lz_RangeClass(this) result(ret)
   class(Range_),intent(in) :: this
   real(real64) :: ret

   ret = this%z_range(2) - this%z_range(1) 
   
end function
! ######################################################


! ######################################################
function to_range_real64_sphere(center,radius) result(ret)
   real(real64),intent(in) :: center(1:3),radius
   type(Range_) :: ret

   ret%sphere_is_active = .true.
   ret%sphere_center = center
   ret%sphere_radius = abs(radius)

end function
! ######################################################


! ######################################################
function to_range_real64_cylinder(p1,p2,radius) result(ret)
   real(real64),intent(in) :: p1(1:3),p2(1:3),radius
   type(Range_) :: ret
   
   ret%cylinder_is_active = .true.
   ret%cylinder_p1 = p1
   ret%cylinder_p2 = p2
   ret%cylinder_radius = abs(radius)

end function
! ######################################################



! ######################################################
function to_range_real64_Cone(p1,p2,radius) result(ret)
   real(real64),intent(in) :: p1(1:3),p2(1:3),radius(1:2)
   type(Range_) :: ret
   
   ret%cone_is_active = .true.
   ret%cone_p1 = p1
   ret%cone_p2 = p2
   ret%cone_radius = abs(radius)

end function
! ######################################################



end module RangeClass
