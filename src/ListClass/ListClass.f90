module ListClass
   use iso_fortran_env
   implicit none
   
   
   
   type :: List_content_
      character(:), allocatable :: char
   end type

   type :: List_fix_content_
      character(len=200) :: char
      integer(int32) :: char_len
   end type

   type :: List_
      type(List_content_), allocatable :: content(:)
      type(List_fix_content_), allocatable :: fcontent(:)
      type(List_), allocatable :: list(:)
   contains
      procedure, public :: get => get_list_content_listclass
      procedure, public :: append => append_list_content_listclass
      procedure, public :: new => new_list_listclass
      procedure, public :: print => print_listclass
      procedure, public :: size => size_listclass

      procedure, public :: help => help_listclass
      procedure, public :: split => split_char_into_list

   end type

   interface to_list
      module procedure to_list_repeat_listclass, to_list_0_listclass, to_list_1_listclass, &
         to_list_2_listclass, to_list_3_listclass, to_list_4_listclass, &
         to_list_5_listclass, to_list_6_listclass, to_list_7_listclass, &
         to_list_int32vec_listclass, &
         to_list_real32vec_listclass, &
         to_list_real64vec_listclass

   end interface

   interface operator(//)
      module procedure joint_listclass
   end interface

   interface operator(//)
      module procedure joint_listcontentclass, joint_arraylistcontentclass
   end interface

   interface operator(.get.)
      module procedure get_element_of_listclass
   end interface

   interface argv
      module procedure argv_get_cmd_args_as_list
   end interface

   interface str
      module procedure str_listclass
   end interface
contains
! #####################################################

   function argv_get_cmd_args_as_list() result(ret)
      type(List_) :: ret
      integer(int32) ::i, n, length, status
      character(:),allocatable :: this_arg
      !character(:),allocatable ::line

      n = command_argument_count()
      call ret%new(n) 
      
      do i = 1,n
         call  get_command_argument(i,length=length,status=status)
         if(allocated(this_arg))then
            deallocate(this_arg)
         endif      
         allocate(character(length)::this_arg)
         
         call  get_command_argument(i,this_arg,status=status)
         
         ret%fcontent(i)%char = this_arg
         ret%fcontent(i)%char_len = length
          
      enddo
      

   end function

! #####################################################

   pure function get_list_content_listclass(this, idx) result(ret)
      class(List_), intent(in) :: this
      integeR(int32), intent(in) :: idx
      character(:), allocatable :: ret

      if (allocated(this%fcontent))then
         ret = this%fcontent(idx)%char(1:this%fcontent(idx)%char_len)
         return

      endif
      if (.not. allocated(this%fcontent)) then
         ret = ""
         return
      end if



   end function
! #####################################################

   pure subroutine new_list_listclass(this, length)
      class(List_), intent(inout) :: this
      integer(int32), intent(in) :: length
      integer(int32) :: i

      !if (allocated(this%fcontent)) then
      !   deallocate (this%fcontent)
      !end if

      if (allocated(this%fcontent)) then
         deallocate (this%fcontent)
      end if

      allocate (this%fcontent(length))
      do i = 1, length
         this%fcontent(i)%char = ""
      end do

   end subroutine

! #####################################################
   subroutine append_list_content_listclass(this, char)
      class(List_), intent(inout) :: this
      character(*), intent(in) :: char
      type(List_) :: buf
      integer(int32) :: i, n

      buf = this
      n = size(buf%fcontent)
      call this%new(length=size(buf%fcontent) + 1)

      do i = 1, size(buf%fcontent)
         this%fcontent(i)%char = buf%fcontent(i)%char
         this%fcontent(i)%char_len = buf%fcontent(i)%char_len
      end do

      this%fcontent(n + 1)%char = char
      this%fcontent(n + 1)%char_len = len(char)

   end subroutine
! #####################################################

! #####################################################
   pure function size_listclass(this) result(ret)
      class(List_), intent(in) :: this
      integer(int32) :: ret

      ret = size(this%fcontent)

   end function
! #####################################################

! #####################################################
   subroutine print_listclass(this)
      class(List_), intent(in) :: this
      character(1) :: comma
      integer(int32) :: i

      print *, str(this)

   end subroutine
! #####################################################

! #####################################################
   function str_listclass(this) result(ret)
      class(List_), intent(in) :: this
      character(1) :: comma
      character(:),allocatable :: ret
      integer(int32) :: i
      ret = ""
      ret = ret //  "["
      if(allocated(this%fcontent))then
         do i = 1, this%size()
            if (i < this%size())then
               ret = ret // trim(this%fcontent(i)%char(1:this%fcontent(i)%char_len)) // ","
            else
               ret = ret // trim(this%fcontent(i)%char(1:this%fcontent(i)%char_len))
            endif
            
            
         end do
      endif

      ret = ret //  "]"

   end function
! #####################################################

   pure function joint_listcontentclass(List_content_1, List_content_2) result(List_content)
      type(List_fix_content_), intent(in) :: List_content_1, List_content_2
      type(List_fix_content_) :: List_content

      List_content%char = List_content_1%char//List_content_2%char

   end function

! #####################################################
   pure function joint_listclass(list1, list2) result(ret_list)
      type(List_), intent(in) :: list1, list2
      type(List_) :: ret_list
      integer(int32) :: i

      if (allocated(list1%fcontent) .and. .not. allocated(list2%fcontent)) then
         ret_list = list1
         return
      end if

      if (allocated(list2%fcontent) .and. .not. allocated(list1%fcontent)) then
         ret_list = list2
         return
      end if

      if (.not. allocated(list1%fcontent) .and. .not. allocated(list2%fcontent)) then
         return
      end if

      call ret_list%new(list1%size() + list2%size())

      do i = 1, list1%size()
         ret_list%fcontent(i)%char = list1%fcontent(i)%char
         ret_list%fcontent(i)%char_len = list1%fcontent(i)%char_len
      end do
      do i = 1, list2%size()
         ret_list%fcontent(i + list1%size())%char = list2%fcontent(i)%char
         ret_list%fcontent(i + list1%size())%char_len = list2%fcontent(i)%char_len
      end do

   end function

   pure function joint_arraylistcontentclass(List_content_1, List_content_2) result(List_content)
      type(List_fix_content_), intent(in) :: List_content_1(:), List_content_2(:)
      type(List_fix_content_), allocatable :: List_content(:)
      integer(int32) :: i

      List_content = List_content_1

      do i = 1, size(List_content_1)
         List_content(i)%char(1:List_content(i)%char_len) = List_content(i)%char(1:List_content(i)%char_len)&
            //List_content_2(i)%char(1:List_content_2(i)%char_len)
      end do

   end function

! #####################################################
   function to_list_0_listclass() result(this)
      type(List_) :: this

      allocate (this%fcontent(0))

   end function
! #####################################################

   function to_list_repeat_listclass(char1, num_repeat) result(this)
      character(*), intent(in) :: char1
      type(List_) :: this
      integer(int32), intent(in) :: num_repeat
      integer(int32) :: i

      allocate (this%fcontent(num_repeat))
      do i = 1, num_repeat
         this%fcontent(i)%char = char1
         this%fcontent(i)%char_len = len(char1)
      end do

   end function

! #####################################################

! #####################################################

   function to_list_1_listclass(char1) result(this)
      character(*), intent(in) :: char1
      character(:), allocatable :: buf
      type(List_) :: this
      integer(int32) :: ac_from, ac_to

      ac_from = index(char1, "[")
      ac_to = index(char1, "]", back=.true.)
      if (ac_from /= 0 .and. ac_to /= 0) then
         buf = char1(ac_from + 1:ac_to - 1)
         call this%split(buf, ",")
      else
         allocate (this%fcontent(1))
         this%fcontent(1)%char = char1
         this%fcontent(1)%char_len = len(char1)
      end if
   end function

! #####################################################

   function to_list_2_listclass(char1, char2) result(this)
      character(*), intent(in) :: char1, char2
      type(List_) :: this

      allocate (this%fcontent(2))
      this%fcontent(1)%char = char1
      this%fcontent(2)%char = char2

      this%fcontent(1)%char_len = len(char1)
      this%fcontent(2)%char_len = len(char2)

   end function

! #####################################################

   function to_list_3_listclass(char1, char2, char3) result(this)
      character(*), intent(in) :: char1, char2, char3
      type(List_) :: this

      allocate (this%fcontent(3))
      this%fcontent(1)%char = char1
      this%fcontent(2)%char = char2
      this%fcontent(3)%char = char3

      this%fcontent(1)%char_len = len(char1)
      this%fcontent(2)%char_len = len(char2)
      this%fcontent(3)%char_len = len(char3)

   end function

! #####################################################

   function to_list_4_listclass(char1, char2, char3, char4) result(this)
      character(*), intent(in) :: char1, char2, char3, char4
      type(List_) :: this

      allocate (this%fcontent(4))
      this%fcontent(1)%char = char1
      this%fcontent(2)%char = char2
      this%fcontent(3)%char = char3
      this%fcontent(4)%char = char4

      this%fcontent(1)%char_len = len(char1)
      this%fcontent(2)%char_len = len(char2)
      this%fcontent(3)%char_len = len(char3)
      this%fcontent(4)%char_len = len(char4)

   end function

! #####################################################

! #####################################################

   function to_list_5_listclass(char1, char2, char3, char4, char5) result(this)
      character(*), intent(in) :: char1, char2, char3, char4, char5
      type(List_) :: this

      allocate (this%fcontent(5))
      this%fcontent(1)%char = char1
      this%fcontent(2)%char = char2
      this%fcontent(3)%char = char3
      this%fcontent(4)%char = char4
      this%fcontent(5)%char = char5


      this%fcontent(1)%char_len = len(char1)
      this%fcontent(2)%char_len = len(char2)
      this%fcontent(3)%char_len = len(char3)
      this%fcontent(4)%char_len = len(char4)
      this%fcontent(5)%char_len = len(char5)
   end function

! #####################################################

! #####################################################

   function to_list_6_listclass(char1, char2, char3, char4, char5, char6) result(this)
      character(*), intent(in) :: char1, char2, char3, char4, char5, char6
      type(List_) :: this

      allocate (this%fcontent(6))
      this%fcontent(1)%char = char1
      this%fcontent(2)%char = char2
      this%fcontent(3)%char = char3
      this%fcontent(4)%char = char4
      this%fcontent(5)%char = char5
      this%fcontent(6)%char = char6


      this%fcontent(1)%char_len = len(char1)
      this%fcontent(2)%char_len = len(char2)
      this%fcontent(3)%char_len = len(char3)
      this%fcontent(4)%char_len = len(char4)
      this%fcontent(5)%char_len = len(char5)
      this%fcontent(6)%char_len = len(char6)
   end function

! #####################################################

! #####################################################

   function to_list_7_listclass(char1, char2, char3, char4, char5, char6, char7) result(this)
      character(*), intent(in) :: char1, char2, char3, char4, char5, char6, char7
      type(List_) :: this

      allocate (this%fcontent(7))
      this%fcontent(1)%char = char1
      this%fcontent(2)%char = char2
      this%fcontent(3)%char = char3
      this%fcontent(4)%char = char4
      this%fcontent(5)%char = char5
      this%fcontent(6)%char = char6
      this%fcontent(7)%char = char7


      this%fcontent(1)%char_len = len(char1)
      this%fcontent(2)%char_len = len(char2)
      this%fcontent(3)%char_len = len(char3)
      this%fcontent(4)%char_len = len(char4)
      this%fcontent(5)%char_len = len(char5)
      this%fcontent(6)%char_len = len(char6)
      this%fcontent(7)%char_len = len(char7)
   end function

! #####################################################

! #####################################################
   function to_list_int32vec_listclass(int32vec) result(this)
      integer(int32), intent(in) :: int32vec(:)
      type(List_) :: this
      integer(int32) :: i
      character(len=50):: b

      allocate (this%fcontent(size(int32vec)))
      do i = 1, size(int32vec)
         write (b, *) int32vec(i)
         this%fcontent(i)%char = trim(adjustl(b))
         this%fcontent(i)%char_len = len(this%fcontent(i)%char)
      end do

   end function
! #####################################################

! #####################################################
   function to_list_real32vec_listclass(real32vec) result(this)
      real(real32), intent(in) :: real32vec(:)
      type(List_) :: this
      integer(int32) :: i
      character(len=50):: b

      allocate (this%fcontent(size(real32vec)))
      do i = 1, size(real32vec)
         write (b, '(f0.10)') real32vec(i)
         this%fcontent(i)%char = trim(adjustl(b))
         this%fcontent(i)%char_len = len(this%fcontent(i)%char)
      end do

   end function
! #####################################################

! #####################################################
   function to_list_real64vec_listclass(real64vec) result(this)
      real(real64), intent(in) :: real64vec(:)
      type(List_) :: this
      integer(int32) :: i
      character(len=50):: b

      allocate (this%fcontent(size(real64vec)))
      do i = 1, size(real64vec)

         write (b, '(G31.20)') real64vec(i)
         this%fcontent(i)%char = trim(adjustl(b))
         this%fcontent(i)%char_len = len(this%fcontent(i)%char)
      end do

   end function
! #####################################################


! #####################################################
   subroutine help_listclass(this)
      class(List_), intent(in) :: this

      print *, "function   get(this,idx) "
      print *, "subroutine append(this,char) "
      print *, "subroutine new(this,length)"
      print *, "print => print_listclass"
      print *, "size => size_listclass"

   end subroutine
! #####################################################

! #####################################################
   subroutine split_char_into_list(this, str_val, delimiter)
      character(*), intent(in) :: str_val, delimiter
      class(List_), intent(inout) :: this
      integer(int32) :: i, n, last_idx, current_idx

      ! split charactor(*) into list
      ! First, count the number of entity
      n = 0
      do i = 1, len(str_val)
         if (str_val(i:i) == delimiter) then
            n = n + 1
         end if
      end do

      call this%new(n)
      last_idx = 1
      n = 0
      do current_idx = 1, len(str_val) - (len(delimiter) - 1)
         if (str_val(current_idx:current_idx + len(delimiter) - 1) == delimiter) then
            n = n + 1
            this%fcontent(n)%char = trim(str_val(last_idx:current_idx - 1))
            this%fcontent(n)%char_len = (current_idx - 1)-last_idx+1
            last_idx = current_idx + 1
         end if
      end do
      if (index(str_val, delimiter, back=.true.) < len(str_val)) then
         n = index(str_val, delimiter, back=.true.)
         if (trim(str_val(n + 1:)) /= "") then
            call this%append(str_val(n + 1:))
         end if
      end if

   end subroutine
! #####################################################

function get_element_of_listclass(this_list,idx) result(ret)
   character(:),allocatable :: ret
   type(List_),intent(in) :: this_list
   integer(int32),intent(in) :: idx

   ret = ""
   if(allocated(this_list%fcontent) )then
      if (size(this_list%fcontent) >= idx )then
         ret = this_list%fcontent(idx)%char(1:this_list%fcontent(idx)%char_len)
      endif
   endif

end function

end module ListClass
