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
      module procedure printString,printStringVec,printStringArray
  end interface


  
contains
!==============================================================
function ascii_lowercaseString(this) result(ret)
  class(String_),intent(in) :: this
  character(len=:),allocatable :: ret  

  ret = this%all
  print *, "Caution:: ascii_lowercaseString not implemented."
  
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
subroutine printStringVec(this)
  class(string_),intent(in) :: this(:)
  integer(int32) :: j


  do j=1,size(this,1)
      write(*,'(A)') this(j)%all//" "
  enddo


end subroutine
!==============================================================


!==============================================================
subroutine printStringArray(this)
  class(string_),intent(in) :: this(:,:)
  integer(int32) :: i,j

  do i=1,size(this,1)
    do j=1,size(this,2)-1
      write(*,'(A)',advance="no") this(i,j)%all//" "
    enddo
    write(*,'(A)',advance="yes") this(i,size(this,2))%all//" "
  enddo

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