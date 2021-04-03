module IOClass
    use iso_fortran_env
    use MathClass
    use StringClass
    implicit none

    type :: IO_
        integer :: fh=100
        logical :: active=.false.
        logical :: EOF=.true.
        character(200)::path,name,extention
    contains
        procedure,public :: unit => unitIO

        procedure,public :: numLine => numLineIO

        procedure,pass   :: openIOchar
        procedure,pass   :: openIOstring
        
        generic,public :: open => openIOchar, openIOstring
        !procedure,public :: open => openIO
        procedure,pass :: writeIOchar
        procedure,pass :: writeIOstring
        procedure,pass :: writeIOint32
        procedure,pass :: writeIOre64
        generic,public :: write => writeIOchar,writeIOstring,writeIOre64,writeIOint32
        !procedure,public :: write => writeIO
        procedure,pass :: readIOchar
        generic,public :: read => readIOchar

        procedure,public :: readline => readlineIO
        procedure,public :: close => closeIO    
    end type

    
    interface print
        module procedure printChar,printString, printReal64, printReal32, printInt64, printInt32
    end interface print

contains

function numLineIO(obj,name) result(line)
    class(IO_),intent(inout) :: obj
    type(IO_) :: f
    character(*),intent(in) :: name
    integer(int32) :: line
    character(len=1) :: content

    call f%open(name)
    
    line=1
    do 
        content = f%readline()
        if(f%EOF .eqv. .true.) then
            line=line-1
            exit
        endif
        line = line+1
    enddo

    call f%close()

end function

! #############################################
function readlineIO(obj) result(ret)
    class(IO_),intent(inout) :: obj
    character(len=:),allocatable :: ret

    if(obj%EOF .eqv. .true.)then
        print *, "ERROR :: file is not opened or EOF"
        allocate(character(len=30000) :: ret )
        ret = " "
        return
    endif

    allocate(character(len=30000) :: ret )
    read(obj%fh,'(A)',end=100) ret
    ret = trim(adjustl(ret) )
    return

100 ret = " "
    obj%EOF =.true.
end function
! #############################################


! #############################################
function unitIO(obj) result(unit)
    class(IO_),intent(inout) :: obj
    integer(int32) :: unit
    unit=obj%fh
end function
! #############################################

! #############################################
subroutine openIOchar(obj,path,name,extention,fh)
    class(IO_),intent(inout) :: obj
    character(*),optional,intent(in)::path,name,extention
    integer(int32),optional,intent(in) :: fh
    logical :: yml=.False.
    

!    if(present(extention) )then
!        if( trim(extention) == "yml" )then
!            yml=.True.
!        endif
!    endif
!    if(present(extention) )then
!        if( trim(extention) == ".yml" )then
!            yml=.True.
!        endif
!    endif
!    if(present(extention) )then
!        if( trim(extention) == ".YML" )then
!            yml=.True.
!        endif
!    endif
!    if(present(extention) )then
!        if( trim(extention) == ".yaml" )then
!            yml=.True.
!        endif
!    endif
!    if(present(extention) )then
!        if( trim(extention) == ".YAML" )then
!            yml=.True.
!        endif
!    endif
!    if(present(extention) )then
!        if( trim(extention) == "yaml" )then
!            yml=.True.
!        endif
!    endif
!    if( index(path,"yml") /= 0 )then
!        yml=.True.
!    endif
!    if( index(path,"yaml") /= 0 )then
!        yml=.True.
!    endif
!
!    if(yml .eqv. .true.)then
!        
!        return
!    endif

    if(obj%active .eqv. .true.)then
        
        print *, "ERROR :: "//trim(obj%path)//trim(obj%name)//trim(obj%extention)//" is already opened."
        stop
    endif

    obj%active=.true.
    if(present(fh) )then
        obj%fh=fh
    else
        obj%fh=10
    endif

    
    obj%path="./"
    obj%name="untitled"
    obj%name=".txt"

    if(present(path) )then
        obj%path=trim(path)
        if(present(name) )then
            obj%name=trim(name)
            if(present(extention) )then
                obj%extention=trim(extention)
                open(newunit=obj%fh,file=trim(path)//trim(name)//trim(extention) )
            else
                open(newunit=obj%fh,file=trim(path)//trim(name) )
            endif
        else
            open(newunit=obj%fh,file=trim(path) )
        endif
    else
        open(newunit=obj%fh,file="./untitled.txt" )
    endif
    
    obj%EOF = .false.

end subroutine openIOchar
! #############################################



! #############################################
subroutine openIOstring(obj,path_s,name_s,extention_s,fh)
    class(IO_),intent(inout) :: obj
    type(String_),intent(in) ::path_s
    type(String_),optional,intent(in)::name_s,extention_s
    character(len=:),allocatable::path,name,extention
    integer(int32),optional,intent(in) :: fh
    logical :: yml=.False.
    

!    if(present(extention) )then
!        if( trim(extention) == "yml" )then
!            yml=.True.
!        endif
!    endif
!    if(present(extention) )then
!        if( trim(extention) == ".yml" )then
!            yml=.True.
!        endif
!    endif
!    if(present(extention) )then
!        if( trim(extention) == ".YML" )then
!            yml=.True.
!        endif
!    endif
!    if(present(extention) )then
!        if( trim(extention) == ".yaml" )then
!            yml=.True.
!        endif
!    endif
!    if(present(extention) )then
!        if( trim(extention) == ".YAML" )then
!            yml=.True.
!        endif
!    endif
!    if(present(extention) )then
!        if( trim(extention) == "yaml" )then
!            yml=.True.
!        endif
!    endif
!    if( index(path,"yml") /= 0 )then
!        yml=.True.
!    endif
!    if( index(path,"yaml") /= 0 )then
!        yml=.True.
!    endif
!
!    if(yml .eqv. .true.)then
!        
!        return
!    endif

    if(obj%active .eqv. .true.)then
        
        print *, "ERROR :: "//trim(obj%path)//trim(obj%name)//trim(obj%extention)//" is already opened."
        stop
    endif

    obj%active=.true.
    if(present(fh) )then
        obj%fh=fh
    else
        obj%fh=10
    endif

    
    obj%path="./"
    obj%name="untitled"
    obj%name=".txt"

    obj%path=trim(path_s%str() )
    if(present(name_s) )then
        obj%name=trim(name_s%str())
        if(present(extention_s) )then
            obj%extention=trim(extention_s%str())
            open(newunit=obj%fh,file=trim(path_s%str())//trim(name_s%str())//trim(extention_s%str()) )
        else
            open(newunit=obj%fh,file=trim(path_s%str())//trim(name_s%str()) )
        endif
    else
        open(newunit=obj%fh,file=trim(path_s%str()) )
    endif
    
    obj%EOF = .false.

end subroutine openIOstring
! #############################################



! #############################################
subroutine writeIOchar(obj,char)
    class(IO_),intent(inout) :: obj
    character(*),intent(in) :: char
    
    write(obj%fh, '(A)') char

end subroutine writeIOchar
! #############################################



! #############################################
subroutine writeIOint32(obj,in32)
    class(IO_),intent(inout) :: obj
    integer(int32),intent(in) :: in32
    
    write(obj%fh, '(A)') trim(str(in32))

end subroutine writeIOint32
! #############################################


! #############################################
subroutine writeIOre64(obj,re64)
    class(IO_),intent(inout) :: obj
    real(real64),intent(in) :: re64
    
    write(obj%fh, '(A)') trim(str(re64))

end subroutine writeIOre64
! #############################################


! #############################################
subroutine writeIOstring(obj,string)
    class(IO_),intent(inout) :: obj
    type(String_),intent(in) :: string
    write(obj%fh, '(A)') str(string)
    

end subroutine writeIOstring
! #############################################

! #############################################
function readIOchar(obj) result(char)
    class(IO_),intent(inout) :: obj
    character(200) :: char
    
    read(obj%fh,'(A)' ) char

end function readIOchar
! #############################################

! #############################################
!function readIOstring(obj) result(char)
!    class(IO_),intent(inout) :: obj
!    type(String_) :: char
!    
!    read(obj%fh,'(A)' ) char%all
!
!end function readIOstring
! #############################################


! #############################################
subroutine closeIO(obj)
    class(IO_),intent(inout) :: obj

    if(obj%active .eqv. .false.)then
        print *, "ERROR :: "//"file is already closed."
        stop
    endif

    close(obj%fh)
    obj%fh=0
    obj%active=.false.
    
end subroutine closeIO
! #############################################

! #############################################
subroutine printChar(char)
    character(*),intent(in) :: char

    write(*,'(A)' ) trim(char)

end subroutine
! #############################################

! #############################################
subroutine printString(char)
    type(String_) :: char

    write(*,'(A)' ) trim(char%all)

end subroutine
! #############################################

! #############################################
subroutine printReal64(re64)
    real(real64),intent(in) :: re64
    character(20) :: char

    write(char, '(f20.10)') re64
    write(*,'(A)' ) trim(adjustl(char))

end subroutine
! #############################################
! #############################################
subroutine printReal32(re32)
    real(real32),intent(in) :: re32

    character(20) :: char

    write(char, '(f20.5)') re32
    write(*,'(A)' ) trim(adjustl(char))

end subroutine
! #############################################


! #############################################
subroutine printint64(in64)
    integer(int64),intent(in) :: in64

    character(20) :: char

    write(char, '(i15.7)') in64
    write(*,'(A)' ) trim(adjustl(char))

end subroutine
! #############################################
! #############################################
subroutine printint32(in32)
    integer(int32),intent(in) :: in32

    character(20) :: char

    write(char, '(i10)') in32
    write(*,'(A)' ) trim(adjustl(char))
end subroutine
! #############################################
end module IOClass