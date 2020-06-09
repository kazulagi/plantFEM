module IOClass
    use iso_fortran_env
    use MathClass
    implicit none

    type :: IO_
        integer :: fh=0
        logical :: active=.false.
        character(200)::path,name,extention
    contains
        procedure,public :: open => openIO
        procedure,public :: write => writeIO
        procedure,public :: close => closeIO
    end type

    
    interface print
        module procedure printChar, printReal64, printReal32, printInt64, printInt32
    end interface print

contains

! #############################################
subroutine openIO(obj,path,name,extention,fh)
    class(IO_),intent(inout) :: obj
    character(*),optional,intent(in)::path,name,extention
    integer(int32),optional,intent(in) :: fh
    
    obj%active=.true.
    if(present(fh) )then
        obj%fh=fh
    else
        obj%fh=10
    endif

    
    if(present(path) )then
        if(present(name) )then
            if(present(extention) )then
                open(obj%fh,file=trim(path)//trim(name)//trim(extention) )
            else
                open(obj%fh,file=trim(path)//trim(name) )
            endif
        else
            open(obj%fh,file=trim(path) )
        endif
    else
        open(obj%fh,file="./untitled.txt" )
    endif
    

end subroutine openIO
! #############################################


! #############################################
subroutine writeIO(obj,char,in32, re64)
    class(IO_),intent(inout) :: obj
    character(*),optional,intent(in) :: char
    integer(int32),optional,intent(in) :: in32
    real(real64),optional,intent(in) :: re64
    
    if(present(char) )then
        write(obj%fh, '(A)') char
    endif
    
    if(present(in32) )then
        write(obj%fh, '(A)') trim(str(in32))
    endif

    if(present(re64) )then
        write(obj%fh, '(A)') trim(str(re64))
    endif

end subroutine writeIO
! #############################################

! #############################################
subroutine closeIO(obj)
    class(IO_),intent(inout) :: obj

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