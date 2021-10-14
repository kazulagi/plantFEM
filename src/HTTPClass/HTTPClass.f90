module HTTPCLass
    use MathClass
    implicit none

    type :: http_
        character(1000) :: body="Hello!"
        character(1000) :: token="your_access_token"
        character(100)  :: channel="your_channel"
        character(1000)  :: url="url"
    contains
        procedure, public :: post => posthttp
        procedure, public :: get => gethttp
    end type
contains

subroutine posthttp(obj)
    class(http_),intent(in) :: obj

    call execute_command_line("curl -X POST '"//&
    trim(obj%url)//"' -d 'token="//&
    trim(obj%token)//"' -d 'channel="//&
    trim(obj%channel)//"' -d 'text="//&
    trim(obj%body)//"'"&
    )

end subroutine

subroutine gethttp(obj)
    class(http_),intent(in) :: obj

    call execute_command_line("curl -X POST '"//&
    trim(obj%url)//"' -d 'token="//&
    trim(obj%token)//"' -d 'channel="//&
    trim(obj%channel)//"' -d 'text="//&
    trim(obj%body)//"'"&
    )

end subroutine

end module HTTPClass
