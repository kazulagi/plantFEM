module HTTPCLass
   !! This module is for HTTP connections.
   !! It just calls CURL.
   use MathClass
   implicit none

   type :: http_
      !! A class for creating HTTP connection.

      character(1000) :: body = "Hello!"
      character(1000) :: token = "your_access_token"
      character(100)  :: channel = "your_channel"
      character(1000)  :: url = "url"
   contains
      !> HTTP POST
      procedure, public :: post => posthttp
      !> HTTP GET
      procedure, public :: get => gethttp
   end type
contains

   subroutine posthttp(obj)
      class(http_), intent(in) :: obj

      call execute_command_line("curl -X POST '"// &
                                obj%url//"' -d 'token="// &
                                obj%token//"' -d 'channel="// &
                                obj%channel//"' -d 'text="// &
                                obj%body//"'" &
                                )

   end subroutine

   subroutine gethttp(obj)
      class(http_), intent(in) :: obj

      call execute_command_line("curl -X POST '"// &
                                obj%url//"' -d 'token="// &
                                obj%token//"' -d 'channel="// &
                                obj%channel//"' -d 'text="// &
                                obj%body//"'" &
                                )

   end subroutine

end module HTTPClass
