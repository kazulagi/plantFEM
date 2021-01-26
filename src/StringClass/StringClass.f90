module StringClass
    implicit none

    type :: string_
      character(len=:),allocatable :: all
    contains
      procedure, public :: char => charString 
      procedure, public :: str => charString 
    end type

    
    public :: operator(+)
    public :: assignment(=)

    interface operator(+)
        module procedure addstring, addstringchar,addcharstring
    end interface
      
    interface assignment(=)
        module procedure assignstring
    end interface

contains

!==============================================================
!
function charString(x) result(ret)
  class(String_),intent(in) :: x
  character(len=:),allocatable :: ret 

  ret = x%all
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

end module 