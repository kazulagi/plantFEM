module StringClass
  use iso_fortran_env
  implicit none

  integer(int32),parameter :: ascii = selected_char_kind('ASCII')
  type :: string_
    character(len=:),allocatable :: all

  contains
    procedure, public :: char => charString 
    procedure, public :: str => charString
    
    procedure, public :: print => printString
    procedure, public :: lower => lowerString
    procedure, public :: upper => upperString
  end type

  
  public :: operator(+)
  public :: assignment(=)

  interface operator(+)
      module procedure addstring, addstringchar,addcharstring
  end interface
    
  interface assignment(=)
      module procedure assignstring
  end interface

  interface print
      module procedure printString
  end interface


  
contains
!==============================================================
function ascii_lowercaseString(this) result(ret)
  class(String_),intent(in) :: this
  character(len=:),allocatable :: ret  


end function
!==============================================================
!
function charString(this) result(ret)
class(String_),intent(in) :: this
character(len=:),allocatable :: ret 

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

z%all = x%all // y%all

end function 
!==============================================================


!==============================================================
function addstringchar(x, y) result(z)

  type(string_), intent(in) :: x
  character(*),intent(in) :: y
  type(string_)             :: z

  z%all = x%all // y

end function 
!==============================================================

!==============================================================
function addcharstring(y,x) result(z)

  type(string_), intent(in) :: x
  character(*),intent(in) :: y
  type(string_)             :: z

  z%all = x%all // y

end function 
!==============================================================

!==============================================================
subroutine assignstring(x, y)
type(string_), intent(out) :: x
character(*),intent(in)  :: y

x%all = y
end subroutine 
!==============================================================

!==============================================================
subroutine printString(this)
  class(string_),intent(in) :: this    

  print *, this%all

end subroutine
!==============================================================

!==============================================================
function lowerString(this) result(ret)
  class(string_),intent(inout) :: this
  type(string_) :: ret
  integer(int32) :: i
  ret%all = ""
  do i=1,len(this%all)
      if( this%all(i:i)>="A" .and. this%all(i:i)<="Z" )then
          ret = ret + char(ichar(this%all(i:i)) + 32 )
      else
          ret = ret + this%all(i:i)
      endif
  enddo

end function
!==============================================================

!==============================================================
function upperString(this) result(ret)
  class(string_),intent(inout) :: this
  type(string_) :: ret
  integer(int32) :: i
  
  do i=1,len(this%all)
      if( this%all(i:i)>="a" .and. this%all(i:i)<="z" )then
          ret = ret + char(ichar(this%all(i:i)) - 32 )
      else
          ret = ret + this%all(i:i)
      endif
  enddo

end function
!==============================================================

end module 