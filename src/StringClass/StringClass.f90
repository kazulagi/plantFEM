module StringClass
   use iso_fortran_env
   implicit none

   integer(int32), parameter :: ascii = selected_char_kind('ASCII')

   !> Derived type for string object.
   type :: string_
      character(len=:), allocatable :: all

   contains
      procedure, public :: char => charString
      !! It returns the string to array of character.

      procedure, public :: str => charString
      !! It initializes the string by an array of character.

      procedure, public :: print => printString
      !! It shows the string in the terminal.

      procedure, public :: lower => lowerString
      !! It converts the CAPITAL to the lowercase
      procedure, public :: upper => upperString
      !! It converts the lowercase to the CAPITAL
   end type

   public :: operator(+)
   public :: assignment(=)

   !> It replaces a character into another.
   interface replace
      module procedure replaceChar
   end interface

   !> It merges two strings.
   interface operator(+)
      module procedure addstring, addstringchar, addcharstring
   end interface

   !> It assigns a string.
   interface assignment(=)
      module procedure assignstring
   end interface

   !> It shows the string in the terminal.
   interface print
      module procedure printString, printStringVec, printStringArray
   end interface

   !> It marges two arrays of characters to a string.
   interface operator(+)
      module procedure addCharChar
   end interface

   !> It detects whether a array of character is contained in the string or not.
   interface operator(.in.)
      module procedure in_detect_char
   end interface

contains
!==============================================================
   function ascii_lowercaseString(this) result(ret)
      class(String_), intent(in) :: this
      character(len=:), allocatable :: ret

      ret = this%all
      print *, "Caution:: ascii_lowercaseString not implemented."

   end function
!==============================================================
!
   function charString(this) result(ret)
      class(String_), intent(in) :: this
      character(len=:), allocatable :: ret

      ret = this%all
   end function
!==============================================================
!

!==============================================================
!
!==============================================================
   function addstring(x, y) result(z)

      type(string_), intent(in) :: x, y
      type(string_)             :: z

      z%all = x%all//y%all

   end function
!==============================================================

!==============================================================
   function addstringchar(x, y) result(z)

      type(string_), intent(in) :: x
      character(*), intent(in) :: y
      type(string_)             :: z

      z%all = x%all//y

   end function
!==============================================================

!==============================================================
   function addcharstring(y, x) result(z)

      type(string_), intent(in) :: x
      character(*), intent(in) :: y
      type(string_)             :: z

      z%all = x%all//y

   end function
!==============================================================

!==============================================================
   subroutine assignstring(x, y)
      type(string_), intent(out) :: x
      character(*), intent(in)  :: y

      x%all = y
   end subroutine
!==============================================================



!==============================================================
   subroutine printString(this)
      class(string_), intent(in) :: this

      print *, this%all

   end subroutine
!==============================================================

!==============================================================
   subroutine printStringVec(this)
      class(string_), intent(in) :: this(:)
      integer(int32) :: j

      do j = 1, size(this, 1)
         write (*, '(A)') this(j)%all//" "
      end do

   end subroutine
!==============================================================

!==============================================================
   subroutine printStringArray(this)
      class(string_), intent(in) :: this(:, :)
      integer(int32) :: i, j

      do i = 1, size(this, 1)
         do j = 1, size(this, 2) - 1
            write (*, '(A)', advance="no") this(i, j)%all//" "
         end do
         write (*, '(A)', advance="yes") this(i, size(this, 2))%all//" "
      end do

   end subroutine
!==============================================================

!==============================================================
   function lowerString(this) result(ret)
      class(string_), intent(inout) :: this
      type(string_) :: ret
      integer(int32) :: i
      ret%all = ""
      do i = 1, len(this%all)
         if (this%all(i:i) >= "A" .and. this%all(i:i) <= "Z") then
            ret = ret + char(ichar(this%all(i:i)) + 32)
         else
            ret = ret + this%all(i:i)
         end if
      end do

   end function
!==============================================================

!==============================================================
   function upperString(this) result(ret)
      class(string_), intent(inout) :: this
      type(string_) :: ret
      integer(int32) :: i

      do i = 1, len(this%all)
         if (this%all(i:i) >= "a" .and. this%all(i:i) <= "z") then
            ret = ret + char(ichar(this%all(i:i)) - 32)
         else
            ret = ret + this%all(i:i)
         end if
      end do

   end function
!==============================================================

   pure function addCharChar(char1, char2) result(char3)
      character(*), intent(in) :: char1, char2
      character(:), allocatable :: char3

      char3 = char1//char2

   end function

! ############################################################
   recursive subroutine replaceChar(word, keyword, to)
      character(*), intent(inout) :: word
      character(*), intent(in) ::keyword, to
      character(:), allocatable :: old_word
      integer(int32) :: n, from

      n = len(keyword)
      old_word = word
      from = index(word, keyword)
      if (from == 0) return
      if (from == len(word)) then
         word = old_word(:from - 1)
         return
      end if

      word = old_word(:from - 1)//to//old_word(from + n:)
      call replaceChar(word, keyword, to)

   end subroutine
! ############################################################

! ############################################################
   function in_detect_char(key, chararg) result(ret)
      character(*), intent(in) :: key, chararg
      logical :: ret

      if (index(chararg, key) == 0) then
         ret = .false.
      else
         ret = .true.
      end if

   end function
! ############################################################

end module
