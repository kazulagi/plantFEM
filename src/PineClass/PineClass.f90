module PineClass
   !use LeafClass
   use ArrayClass
   use IOClass
   use StemClass
   use FEMDomainClass
   implicit none

   type :: Pine_
      type(Stem_), allocatable :: stem(:)

      real(real64) :: mainstem_length
      real(real64), allocatable :: mainstem_diameters(:)
      real(real64), allocatable :: mainstem_diameters_h(:)
      integer(int32) :: divisions(1:2)
      real(real64) :: Branch_heights(1:2)
      integer(int32) :: NumberOfBranch
      real(real64) :: DiameterOfBranch_ave
      real(real64) :: DiameterOfBranch_sig
      real(real64) :: AngleOfBranch_ave
      real(real64) :: AngleOfBranch_sig

   contains
      procedure, public :: init => initPine
   end type


contains


! --------------------------------------------------------------
   subroutine initPine(this, config,debug)
      class(Pine_), intent(inout) :: this
      character(*), intent(in) :: config
      logical,optional,intent(in) :: debug
      type(IO_) :: f
      integer(int32) :: r_num, l_num
      real(real64) :: z, r_max, resize_ratio
      real(real64), allocatable :: x(:)
      integer(int32), allocatable :: surface_elements(:, :),vec(:)
      integer(int32) :: i, branch_idx
      type(Random_) :: random
      logical :: debug_mode

      debug_mode = .false.
      if(present(debug) )then
         debug_mode = debug
      endif

      this%mainstem_length = freal(f%parse_json(config, to_list("Mainstem", "Length")))
      this%mainstem_diameters = &
         f%parse_json(config, to_list("Mainstem", "Diameters")) .as.real64_vector()
      this%mainstem_diameters_h = &
         f%parse_json(config, to_list("Mainstem", "Diameter_heights")) .as.real64_vector()
      this%divisions = &
         f%parse_json(config, to_list("Mainstem", "Divisions")) .as.int32_vector()
      this%Branch_heights = f%parse_json(config, to_list("Branch", "Branch_heights")) .as.real64_vector()
      this%NumberOfBranch = fint(f%parse_json(config, to_list("Branch", "NumberOfBranch")))
      this%DiameterOfBranch_ave = freal(f%parse_json(config, to_list("Branch", "DiameterOfBranch_ave")))
      this%DiameterOfBranch_sig = freal(f%parse_json(config, to_list("Branch", "DiameterOfBranch_sig")))
      this%AngleOfBranch_ave = freal(f%parse_json(config, to_list("Branch", "AngleOfBranch_ave")))
      this%AngleOfBranch_sig = freal(f%parse_json(config, to_list("Branch", "AngleOfBranch_sig")))

      if (allocated(this%stem)) then
         deallocate (this%stem)
      end if

      allocate (this%stem(1))
      r_num = this%divisions(1)
      l_num = this%divisions(2)
      print *, l_num
      call this%stem(1)%femdomain%to_cylinder(x_num=r_num, y_num=r_num, z_num=l_num)
      call this%stem(1)%femdomain%resize( &
         x=1.0d0, &
         y=1.0d0, &
         z=this%mainstem_length)

      do i = 1, this%stem(1)%femdomain%nn()
         x = this%stem(1)%femdomain%mesh%nodcoord(i, :)
         z = x(3)
         r_max = 0.50d0* &
                 linear_interpolate_pine(x=this%mainstem_diameters_h, f=this%mainstem_diameters, x0=z)
         resize_ratio = r_max
         this%stem(1)%femdomain%mesh%nodcoord(i, 1:2) &
            = resize_ratio*this%stem(1)%femdomain%mesh%nodcoord(i, 1:2)
      end do
      if(debug_mode)then
         print *, "[ok] :: PineClass :: Stem generaton is done!"
      endif

      ! grow branches
      ! とりあえず，surface elementをすべて取得
      surface_elements = this%stem(1)%femdomain%getSurfaceElements(to_range( &
                           z_min=this%Branch_heights(1), &
                           z_max=this%Branch_heights(2) &
                           ) &
                         )
      if(debug_mode)then
         print *, "[ok] :: PineClass :: Surface elements are selected!"
      endif

      ! create idx
      !surface_elements = surface_elements( &
      !                   random%draw([(i, i=1, size(surface_elements, 1))], this%NumberOfBranch) &
      !                   , :)
!
      ! >>>>>> branching >>>>>> 

      vec = [(i_i,i_i=1,size(surface_elements,1))]
      surface_elements = surface_elements(random%draw(vec,this%NumberOfBranch),:)
      
      if(debug_mode)then
         print *, "[ok] :: PineClass :: Branching points are identified!"
      endif
      call this%stem(1)%femdomain%extract(SurfaceElements=surface_elements,repeat=10)

   end subroutine
! --------------------------------------------------------------



! --------------------------------------------------------------
   function linear_interpolate_pine(x, f, x0) result(ret)
      real(real64), intent(in) :: x(:), f(:), x0
      real(real64) :: ret, theta
      integer(int32) :: i

      if (x0 < x(1)) then
         ret = f(1)
         return
      end if

      if (x0 > x(size(x))) then
         ret = f(size(f))
         return
      end if

      do i = 1, size(x) - 1
         if (x(i) <= x0 .and. x0 <= x(i + 1)) then
            theta = (x0 - x(i))/(x(i + 1) - x(i))
            ! theta = 0 => ret = f(i)
            ! theta = 1 => ret = f(i+1)
            ret = (1.0d0 - theta)*f(i) + theta*f(i + 1)
            return
         end if
      end do

   end function
! --------------------------------------------------------------


end module PineClass
