module ConsoleClass
   !! This module defines a class of console, which is inspired by the console of Javascript.
   use MathClass
   implicit none

   integer(int32), parameter ::PF_CONSOLE_ARG_LEN = 200
   type :: Console_
      !! It is inspired by the console of Javascript.
      character(:), allocatable :: line
   contains
      !> Similar to console.log() in Javascript.
      procedure, public :: log => logConsole
      procedure, public :: write => logConsole
      procedure, public :: writeLine => logConsole
      procedure, public :: read => readConsole
      procedure, public :: readLine => readlineConsole
      procedure, public :: argument => argumentConsole

      procedure, public :: in => inConsole

      procedure, public :: asInt => asIntConsole
      procedure, public :: asInteger => asIntConsole
      procedure, public :: as_Int => asIntConsole
      procedure, public :: as_Integer => asIntConsole

      procedure, public :: asReal => asRealConsole
      procedure, public :: as_Real => asRealConsole
      procedure, public :: asFloat => asRealConsole
      procedure, public :: as_Float => asRealConsole
      procedure, public :: asDouble => asRealConsole
      procedure, public :: as_Double => asRealConsole

      procedure, public :: asChar => asCharConsole
   end type
contains

! ###################################################################
   subroutine logConsole(obj, line)
      class(Console_), intent(inout) :: obj
      character(*), intent(in) :: line

      write (*, *) line
      obj%line = line

   end subroutine
! ###################################################################

! ###################################################################
   subroutine readConsole(obj)
      class(Console_), intent(inout) :: obj
      character(3000) :: aline
      read (*, *) aline
      obj%line = aline
   end subroutine
! ###################################################################

! ###################################################################
   function readLineConsole(obj) result(line)
      class(Console_), intent(inout) :: obj
      character(3000) :: aline
      character(:), allocatable :: line

      read (*, *) aline
      line = aline
      obj%line = line

   end function
! ###################################################################

! ###################################################################
   function asIntConsole(obj) result(ret)
      class(Console_), intent(inout) :: obj
      integer(int32) :: ret
      ret = fint(obj%line)

   end function
! ###################################################################

! ###################################################################
   function asRealConsole(obj) result(ret)
      class(Console_), intent(inout) :: obj
      real(real64) :: ret

      ret = freal(obj%line)

   end function
! ###################################################################

! ###################################################################
   function asCharConsole(obj) result(ret)
      class(Console_), intent(inout) :: obj
      character(:), allocatable :: ret

      ret = obj%line

   end function
! ###################################################################

! ###################################################################
   function inConsole(obj, word) result(ret)
      class(Console_), intent(inout) :: obj
      character(*), intent(in) :: word
      logical :: ret

      if (index(obj%line, word) /= 0) then
         ret = .true.
      else
         ret = .false.
      end if
   end function
! ###################################################################

   function argumentConsole(obj) result(cmd_arg)
      class(Console_), intent(inout) :: obj
      character(PF_CONSOLE_ARG_LEN) :: cmd_arg_PF_CONSOLE_ARG_LEN
      character(:), allocatable :: cmd_arg

      call get_command_argument(1, cmd_arg_PF_CONSOLE_ARG_LEN)

      cmd_arg = cmd_arg_PF_CONSOLE_ARG_LEN

   end function

end module
