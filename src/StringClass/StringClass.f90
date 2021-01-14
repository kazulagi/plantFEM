module StringClass
    implicit none

    type :: string_
        character(len=:),allocatable :: all
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