module LoggerClass
   use MathClass
   use StringClass
   use IOClass
   use ArrayClass
   use FEMDomainClass
   implicit none

   integer(int32) :: PF_MAX_CHANNEL_NUM = 100

   type :: Logger_
      type(String_), allocatable     :: channel_name(:)
      type(Real64Ptr_), allocatable :: channel_value(:)
      integer(int32), allocatable :: channel_id(:)
      real(real64) :: position(1:3) = 0.0d0
      logical, allocatable :: channel_active(:)

      logical :: initialized = .False.
      integer(int32)::counter = 0

      ! for generic obervation point
      integer(int32) :: point_DOF = 0
      integer(int32) :: elementID = 0
      real(real64), allocatable :: weight(:)
      type(Real64Ptr_), allocatable :: source_values(:, :)

   contains
      procedure :: init => initLogger
      procedure :: numchannel => numchannelLogger
      procedure, pass ::  setLogger_byvalue, setLogger_byDomain, setLogger_byDomains
      generic :: set => setLogger_byvalue, setLogger_byDomain, setLogger_byDomains
      procedure :: start => startLogger
      procedure :: save => saveLogger
      procedure :: reset => resetLogger
      procedure :: vtk => vtkLogger
      procedure :: move => moveLogger

   end type
contains

   !------------------------------------------------------
   subroutine initLogger(this, MAX_CHANNEL_NUM)
      class(Logger_), intent(inout) :: this
      integer(int32), optional, intent(in) :: MAX_CHANNEL_NUM
      integer(int32) :: n

      if (allocated(this%channel_name)) then
         deallocate (this%channel_name)
      end if
      if (allocated(this%channel_value)) then
         deallocate (this%channel_value)
      end if
      if (allocated(this%channel_id)) then
         deallocate (this%channel_id)
      end if
      if (allocated(this%channel_active)) then
         deallocate (this%channel_active)
      end if

      n = input(default=PF_MAX_CHANNEL_NUM, option=MAX_CHANNEL_NUM)

      allocate (this%channel_name(n))
      allocate (this%channel_value(n))
      allocate (this%channel_id(n))
      allocate (this%channel_active(n))

      this%channel_id(:) = 0
      this%channel_active(:) = .False.
      this%initialized = .True.

   end subroutine
   !------------------------------------------------------

   !------------------------------------------------------
   function numchannelLogger(this) result(ret)
      class(Logger_), intent(inout) :: this
      integer(int32) :: ret, i, n

      ret = 0
      do i = 1, size(this%channel_active)
         if (this%channel_active(i)) then
            ret = ret + 1
         else
            cycle
         end if
      end do
   end function
   !------------------------------------------------------

   !------------------------------------------------------
   subroutine setLogger_byvalue(this, channel_name, channel_value, channel_id, position)
      class(Logger_), intent(inout) :: this
      character(*), intent(in) :: channel_name
      real(real64), target, intent(in) :: channel_value
      integer(int32), optional, intent(in) :: channel_id
      real(real64), optional, intent(in) :: position(:)
      integer(int32) :: n

      if (.not. this%initialized) then
         call this%init()
      end if

      if (present(channel_id)) then
         n = channel_id
      else
         n = this%numchannel() + 1
      end if

      this%channel_name(n) = channel_name

      this%channel_value(n)%ptr => channel_value
      this%channel_id(n) = n
      this%channel_active(n) = .true.

      if (present(position)) then
         this%position(1:size(position)) = position(1:size(position))
      end if

   end subroutine
   !------------------------------------------------------

   !------------------------------------------------------
   subroutine startLogger(this)
      class(Logger_), intent(inout) :: this
      integer(int32) :: i, n
      type(IO_) :: f

      n = 0
      this%counter = 0
      do i = 1, size(this%channel_active)
         if (this%channel_active(i)) then
            call f%open(this%channel_name(i)%all//".txt", "a")
            call f%close()
            n = n + 1
         end if
         if (n == this%numchannel()) return
      end do

   end subroutine
   !------------------------------------------------------

   !------------------------------------------------------
   subroutine saveLogger(this, t, debug)
      class(Logger_), intent(inout) :: this
      integer(int32) :: i, n, j
      real(real64), optional, intent(in) :: t
      logical, optional, intent(in) :: debug
      real(real64) :: channel_val
      type(IO_) :: f

      logical :: debug_mode

      if (present(debug)) then
         debug_mode = .true.
      else
         debug_mode = .false.
      end if
      if (debug_mode) print *, "[ok] saveLogger >> started"
      if (allocated(this%source_values)) then
         ! with setLogger_byDomain()
         ! it may have some bugs.
         do i = 1, this%point_DOF
            call f%open(this%channel_name(1)%all + "_dim_"+str(i) + ".txt", "a")
            channel_val = 0.0d0

            !if(debug_mode) print *, "saveLogger >> computing values>>start"
            do j = 1, size(this%source_values, 1)
               channel_val = channel_val + this%weight(j)*this%source_values(j, i)%ptr
            end do
            !if(debug_mode) print *, "saveLogger >> computing values>> done!"

            if (present(t)) then
               write (f%fh, *) t, channel_val
            else
               !call f%write(this%counter,channel_val )
               write (f%fh, *) this%counter, channel_val

            end if
            call f%close()
         end do
      else
         ! without setLogger_byDomain()
         n = 0
         this%counter = this%counter + 1
         do i = 1, size(this%channel_active)
            if (this%channel_active(i)) then
               call f%open(this%channel_name(i)%all//".txt", "a")
               if (present(t)) then
                  write (f%fh, *) t, this%channel_value(i)%ptr
               else
                  write (f%fh, *) this%counter, this%channel_value(i)%ptr
               end if

               call f%close()
               n = n + 1
            end if
            if (n == this%numchannel()) return
         end do
      end if
      if (debug_mode) print *, "[ok] saveLogger >> done"
   end subroutine
   !------------------------------------------------------

   !------------------------------------------------------
   subroutine resetLogger(this)
      class(Logger_), intent(inout) :: this
      integer(int32) :: i, n
      type(IO_) :: f

      n = 0
      this%counter = 0
      do i = 1, size(this%channel_active)
         if (this%channel_active(i)) then
            call f%open(this%channel_name(i)%all//".txt", "a")
            call f%close("delete")
            n = n + 1
         end if
         if (n == this%numchannel()) return
      end do

   end subroutine
   !------------------------------------------------------

   subroutine vtkLogger(this, name)
      class(Logger_), intent(in) :: this
      character(*), intent(in) :: name
      type(IO_) :: f

      call f%open(name + ".vtk", "w")
      call f%write("# vtk DataFile Version 2.0")
      call f%write(name)
      call f%write("ASCII")
      call f%write("DATASET UNSTRUCTURED_GRID")
      call f%write("POINTS 1 float")
      call f%write(str(this%position(1)) + " "+str(this%position(2)) + " "+str(this%position(3)))
      call f%write("CELLS 1 9 8 0 0 0 0 0 0 0 0")
      call f%flush()
      call f%close()

   end subroutine

   subroutine moveLogger(this, x, y, z)
      class(Logger_), intent(inout) :: this
      real(real64), optional, intent(in) :: x, y, z

      if (present(x)) then
         this%position(1) = this%position(1) + x
      end if

      if (present(y)) then
         this%position(2) = this%position(2) + y
      end if

      if (present(z)) then
         this%position(3) = this%position(3) + z
      end if

   end subroutine
! ###################################################################

   subroutine setLogger_byDomains(this, femdomains, position, dataset, DOF, name)
      class(Logger_), intent(inout) :: this
      type(FEMDomain_), intent(inout) :: femdomains(:)
      real(real64), intent(in) :: position(:)
      real(real64), target, intent(in) :: dataset(:)
      character(*), intent(in) :: name

      real(real64) :: localCoord(1:3)
      integer(int32) :: i, j, dof, node_id, from, to
      type(ShapeFunction_) :: sf
      type(IO_) :: f

      from = 1
      do i = 1, size(femdomains)
         if (femdomains(i)%empty()) then
            cycle
         else
            if (femdomains(i)%has(position)) then
               to = from + femdomains(i)%nn()*DOF - 1
               call this%set(femdomain=femdomains(i), position=position, &
                             dataset=dataset(from:to), name=name)
               return
            end if
            from = from + femdomains(i)%nn()*DOF
         end if

      end do

   end subroutine

! ###################################################################
   subroutine setLogger_byDomain(this, femdomain, position, dataset, name)
      class(Logger_), intent(inout) :: this
      type(FEMDomain_), intent(inout) :: femdomain
      real(real64), intent(in) :: position(:)
      real(real64), target, intent(in) :: dataset(:)
      character(*), intent(in) :: name

      real(real64) :: localCoord(1:3)
      integer(int32) :: i, j, dof, node_id
      type(ShapeFunction_) :: sf
      type(IO_) :: f

      if (.not. this%initialized) then
         call this%init()
      end if

      ! only single channel
      this%channel_name(1)%all = name
      this%position = position

      ! detect dataset-type
      if (mod(size(dataset), femdomain%nn()) == 0) then
         ! node-wise value
         this%point_DOF = size(dataset)/femdomain%nn()
         if (allocated(this%source_values)) then
            deallocate (this%source_values)
         end if

         this%ElementID = femdomain%getElementID(x=position)
         if (this%ElementID <= 0) then
            print *, "setLogger_byDomain >> invalid element position"
            print *, "ERROR code: ", this%ElementID
            stop
         end if
         localCoord = femdomain%getLocalCoordinate(ElementID=this%ElementID, &
                                                   x=position(1), y=position(2), z=position(3))

         sf = femdomain%getShapeFunction(ElementID=this%ElementID, position=position)
         this%weight = sf%nmat

         allocate (this%source_values(femdomain%nne(), this%point_DOF))
         do i = 1, femdomain%nne()
            do dof = 1, this%point_DOF
               node_id = femdomain%mesh%elemnod(this%ElementID, i)
               this%source_values(i, dof)%ptr => dataset(this%point_DOF*(node_id - 1) + dof)
            end do
         end do

      elseif (mod(size(dataset), femdomain%ne()) == 0) then
         ! element-wise value
         this%point_DOF = size(dataset)/femdomain%ne()

         if (allocated(this%source_values)) then
            deallocate (this%source_values)
         end if
         this%ElementID = femdomain%getElementID(x=position)
         ! same value
         this%weight = eyes(1)

         allocate (this%source_values(1, this%point_DOF))

         do dof = 1, this%point_DOF
            this%source_values(1, dof)%ptr => &
               dataset(this%point_DOF*(this%ElementID - 1) + dof)
         end do

      else
         print *, "[ERROR] setLogger_byDomain >> size(dataset) should be %nn()*n or %ne()*n"
         return
      end if

      do i = 1, this%point_DOF
         call f%open(this%channel_name(1)%all + "_dim_"+str(i) + ".txt", "a")
         call f%close()
      end do

   end subroutine

end module LoggerClass

