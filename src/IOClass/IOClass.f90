module IOClass
    use iso_fortran_env
    use MathClass
    use StringClass
    implicit none

    type :: IO_
        integer :: fh=100
        logical :: active=.false.
        logical :: EOF=.true.
        character(1) :: state
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
        procedure,pass :: writeIOint32Vector
        procedure,pass :: writeIOint32Array
        procedure,pass :: writeIOre64
        procedure,pass :: writeIOre64Vector
        procedure,pass :: writeIOre64Array
        procedure,pass :: writeIOcomplex64
        procedure,pass :: writeIOcomplex64Vector
        procedure,pass :: writeIOcomplex64Array
        generic,public :: write => writeIOchar,writeIOstring,writeIOre64,writeIOre64Vector,writeIOre64Array,&
            writeIOint32,writeIOint32Vector,writeIOint32Array,&
            writeIOcomplex64,writeIOcomplex64Vector,writeIOcomplex64Array
        !procedure,public :: write => writeIO
        procedure,pass :: readIOchar
        procedure,pass :: readIOInt
        procedure,pass :: readIOIntVector
        procedure,pass :: readIOIntArray
        procedure,pass :: readIOReal64
        procedure,pass :: readIOReal64Vector
        procedure,pass :: readIOReal64Array
        generic,public :: read => readIOchar,readIOInt,readIOIntVector,readIOIntArray&
            ,readIOReal64,readIOReal64Vector,readIOReal64Array

        procedure,public :: readline => readlineIO
        procedure,public :: close => closeIO    
    end type

    
    interface print
        module procedure printChar,printString, printReal64, printReal32, printInt64, printInt32
    end interface print

    interface disp
        module procedure printChar,printString, printReal64, printReal32, printInt64, printInt32
    end interface disp

    interface plot
        module procedure plotRealArray
    end interface

    interface spy
        module procedure spyRealArray
    end interface
contains

! ===========================================
subroutine readIOInt(obj,val)
    class(IO_),intent(in) :: obj
    integer(int32),intent(inout) :: val

    read(obj%fh,*) val

end subroutine
! ===========================================


! ===========================================
subroutine readIOIntVector(obj,val)
    class(IO_),intent(in) :: obj
    integer(int32),intent(inout) :: val(:)

    read(obj%fh,*) val(:)

end subroutine
! ===========================================


! ===========================================
subroutine readIOIntArray(obj,val)
    class(IO_),intent(in) :: obj
    integer(int32),intent(inout) :: val(:,:)

    read(obj%fh,*) val(:,:)

end subroutine
! ===========================================

! ===========================================
subroutine readIOReal64(obj,val)
    class(IO_),intent(in) :: obj
    real(real64),intent(inout) :: val

    read(obj%fh,*) val

end subroutine
! ===========================================


! ===========================================
subroutine readIOReal64Vector(obj,val)
    class(IO_),intent(in) :: obj
    real(real64),intent(inout) :: val(:)

    read(obj%fh,*) val(:)

end subroutine
! ===========================================


! ===========================================
subroutine readIOReal64Array(obj,val)
    class(IO_),intent(in) :: obj
    real(real64),intent(inout) :: val(:,:)

    read(obj%fh,*) val(:,:)

end subroutine
! ===========================================

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
subroutine openIOchar(obj,path,state,name,extention,fh)
    class(IO_),intent(inout) :: obj
    character(1),optional,intent(in) :: state ! w or r
    character(*),optional,intent(in)::path,name,extention
    integer(int32),optional,intent(in) :: fh
    logical :: yml=.False.

    if(present(state) )then
        obj%state = state
    else
        obj%state="w"
    endif
    

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
                if(present(state) )then
                    if(state=="r")then
                        open(newunit=obj%fh,file=trim(path)//trim(name)//trim(extention),status='old')
                    elseif(state=="w")then
                        open(newunit=obj%fh,file=trim(path)//trim(name)//trim(extention),status='replace')
                    else
                        call print("Error :: IOClass % open >> argument <state> should be w or r ")
                        stop
                    endif
                else
                    open(newunit=obj%fh,file=trim(path)//trim(name)//trim(extention) )
                endif
            else
                if(present(state) )then
                    if(state=="r")then
                        open(newunit=obj%fh,file=trim(path)//trim(name),status='old' )
                    elseif(state=="w")then
                        open(newunit=obj%fh,file=trim(path)//trim(name),status='replace' )
                    else
                        call print("Error :: IOClass % open >> argument <state> should be w or r ")
                        stop
                    endif
                else
                    open(newunit=obj%fh,file=trim(path)//trim(name) )
                endif
            endif
        else
            if(present(state) )then
                if(state=="r")then
                    open(newunit=obj%fh,file=trim(path),status='old' )
                elseif(state=="w")then
                    open(newunit=obj%fh,file=trim(path),status='replace' )
                else
                    call print("Error :: IOClass % open >> argument <state> should be w or r ")
                    stop
                endif
            else
                open(newunit=obj%fh,file=trim(path) )
            endif
        endif
    else
        open(newunit=obj%fh,file="./untitled.txt",status="replace" )
    endif
    
    obj%EOF = .false.

end subroutine openIOchar
! #############################################



! #############################################
subroutine openIOstring(obj,path_s,state,name_s,extention_s,fh)
    class(IO_),intent(inout) :: obj
    character(1),optional,intent(in) :: state ! w or r
    type(String_),intent(in) ::path_s
    type(String_),optional,intent(in)::name_s,extention_s
    character(len=:),allocatable::path,name,extention
    integer(int32),optional,intent(in) :: fh
    logical :: yml=.False.
    

    if(present(state) )then
        obj%state = state
    else
        obj%state="w"
    endif
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

    if(present(state) )then
        if(state == "w")then
            open(newunit=obj%fh,file=trim(path_s%str()),status="replace" )
        elseif(state == "r")then
            open(newunit=obj%fh,file=trim(path_s%str()),status="old" )
        else
            call print("Error :: IOClass % open >> argument <state> should be w or r ")
        endif
    endif

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

    if(obj%state=="r")then
        call print("IOClass >> Error >> This file is readonly. ")
        call print("Nothing is written.")
        return
    endif
    
    write(obj%fh, '(A)') char

end subroutine writeIOchar
! #############################################



! #############################################
subroutine writeIOint32(obj,in32)
    class(IO_),intent(inout) :: obj
    integer(int32),intent(in) :: in32

    if(obj%state=="r")then
        call print("IOClass >> Error >> This file is readonly. ")
        call print("Nothing is written.")
        return
    endif
    
    write(obj%fh, '(A)') trim(str(in32))

end subroutine writeIOint32
! #############################################


! #############################################
subroutine writeIOint32Vector(obj,in32)
    class(IO_),intent(inout) :: obj
    integer(int32),intent(in) :: in32(:)
    integer(int32) :: i

    if(obj%state=="r")then
        call print("IOClass >> Error >> This file is readonly. ")
        call print("Nothing is written.")
        return
    endif
    do i=1,size(in32)
        write(obj%fh, '(A)') trim(str(in32(i) ))
    enddo
end subroutine
! #############################################


! #############################################
subroutine writeIOint32Array(obj,in32)
    class(IO_),intent(inout) :: obj
    integer(int32),intent(in) :: in32(:,:)
    integer(int32) :: i

    if(obj%state=="r")then
        call print("IOClass >> Error >> This file is readonly. ")
        call print("Nothing is written.")
        return
    endif
    do i=1,size(in32,1)
        write(obj%fh, *) in32(i,:) 
    enddo
end subroutine
! #############################################


! #############################################
subroutine writeIOre64(obj,re64)
    class(IO_),intent(inout) :: obj
    real(real64),intent(in) :: re64
    
    if(obj%state=="r")then
        call print("IOClass >> Error >> This file is readonly. ")
        call print("Nothing is written.")
        return
    endif
    write(obj%fh, '(A)') trim(str(re64))

end subroutine writeIOre64
! #############################################


! #############################################
subroutine writeIOre64Vector(obj,re64,sparse)
    class(IO_),intent(inout) :: obj
    real(real64),intent(in) :: re64(:)
    logical,optional,intent(in) :: sparse
    integer(int32) :: i

    if(obj%state=="r")then
        call print("IOClass >> Error >> This file is readonly. ")
        call print("Nothing is written.")
        return
    endif

    if(present(sparse) )then
        if(sparse)then
            do i=1,size(re64,1)
                if(re64(i)==0.0d0)then
                    write(obj%fh, '(A)', advance='no') '0 '
                else
                    write(obj%fh, '(A)', advance='no') '* '
                endif
                write(obj%fh, '(A)', advance='yes') ' '
            enddo
            return
        endif
    endif


    do i=1,size(re64)
        write(obj%fh, '(A)') trim(str(re64(i) ))
    enddo
end subroutine
! #############################################



! #############################################
subroutine writeIOre64Array(obj,re64,sparse)
    class(IO_),intent(inout) :: obj
    real(real64),intent(in) :: re64(:,:)
    logical,optional,intent(in) :: sparse
    integer(int32) :: i,j

    if(obj%state=="r")then
        call print("IOClass >> Error >> This file is readonly. ")
        call print("Nothing is written.")
        return
    endif
    if(present(sparse) )then
        if(sparse)then
            
            do i=1,size(re64,1)
                do j=1,size(re64,2)
                    if(re64(i,j)==0.0d0)then
                        write(obj%fh, '(A)', advance='no') '0 '
                    else
                        write(obj%fh, '(A)', advance='no') '* '
                    endif
                enddo 
                write(obj%fh, '(A)', advance='yes') ' '
            enddo
            return
        endif
    endif
    do i=1,size(re64,1)
        write(obj%fh, *) re64(i,:) 
    enddo
end subroutine
! #############################################

! #############################################
subroutine writeIOcomplex64(obj,complex64)
    class(IO_),intent(inout) :: obj
    complex(kind(0d0) ),intent(in) :: complex64
    
    if(obj%state=="r")then
        call print("IOClass >> Error >> This file is readonly. ")
        call print("Nothing is written.")
        return
    endif
    write(obj%fh, '(A)') trim(str(complex64))

end subroutine writeIOcomplex64
! #############################################


! #############################################
subroutine writeIOcomplex64Vector(obj,complex64)
    class(IO_),intent(inout) :: obj
    complex(kind(0d0) ),intent(in) :: complex64(:)
    integer(int32) :: i

    if(obj%state=="r")then
        call print("IOClass >> Error >> This file is readonly. ")
        call print("Nothing is written.")
        return
    endif
    do i=1,size(complex64,1)
        write(obj%fh, '(A)') trim(str(complex64(i) ))
    enddo
end subroutine
! #############################################

! #############################################
subroutine writeIOcomplex64Array(obj,complex64)
    class(IO_),intent(inout) :: obj
    complex(kind(0d0) ),intent(in) :: complex64(:,:)
    integer(int32) :: i

    if(obj%state=="r")then
        call print("IOClass >> Error >> This file is readonly. ")
        call print("Nothing is written.")
        return
    endif
    do i=1,size(complex64,1)
        write(obj%fh, *) complex64(i,:)
    enddo
end subroutine
! #############################################

! #############################################
subroutine writeIOstring(obj,string)
    class(IO_),intent(inout) :: obj
    type(String_),intent(in) :: string

    if(obj%state=="r")then
        call print("IOClass >> Error >> This file is readonly. ")
        call print("Nothing is written.")
        return
    endif
    write(obj%fh, '(A)') str(string)
    

end subroutine writeIOstring
! #############################################

! #############################################
subroutine readIOchar(obj,char) 
    class(IO_),intent(inout) :: obj
    character(200) :: char
    
    read(obj%fh,'(A)' ) char

end subroutine readIOchar
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

subroutine plotRealArray(x,y,z,xr,yr,zr) 
    real(real64),intent(in) :: x(:),y(:)
    real(real64),optional,intent(in) :: z(:)
    character(*),optional,intent(in) :: xr, yr, zr 
    type(IO_) :: f
    integer(int32) :: i

    if(present(z) )then
    
        call f%open("__plotRealArray__buf_.txt")
        do i=1,size(x)
            call f%write(str(x(i))//" "//str(y(i)) //" "//str(z(i)) )
        enddo
        call f%close()
        
        call print("")
        call print("Press Ctrl+c to close window.")
        call f%open("__plotRealArray__buf_.gp")
        call f%write('unset key' )
        do i=1,size(x)
            if(present(xr) )then
                call f%write('set xr'//trim(xr) )
            endif
            if(present(yr) )then
                call f%write('set yr'//trim(yr) )
            endif
            if(present(zr) )then
                call f%write('set zr'//trim(zr) )
            endif
            call f%write('splot "__plotRealArray__buf_.txt" w l')
            call f%write("pause -1")
        enddo
        call f%close()
    
        call system("gnuplot __plotRealArray__buf_.gp")
    
        call system("rm __plotRealArray__buf_.txt")
        call system("rm __plotRealArray__buf_.gp")
    
        return
    endif
    
    call f%open("__plotRealArray__buf_.txt")
    do i=1,size(x)
        call f%write(str(x(i))//" "//str(y(i)) )
    enddo
    call f%close()
    
    call print("")
    call print("Press Ctrl+c to close window.")
    call f%open("__plotRealArray__buf_.gp")
    call f%write('unset key' )
    do i=1,size(x)
        if(present(xr) )then
            call f%write('set xr'//trim(xr) )
        endif
        if(present(yr) )then
            call f%write('set yr'//trim(yr) )
        endif
        call f%write('plot "__plotRealArray__buf_.txt" w l')
        call f%write("pause -1")
    enddo
    call f%close()

    call system("gnuplot __plotRealArray__buf_.gp")

    call system("rm __plotRealArray__buf_.txt")
    call system("rm __plotRealArray__buf_.gp")

end subroutine

subroutine spyRealArray(array)
    real(real64),intent(in) :: array(:,:)
    real(real64),allocatable :: x(:),y(:)
    integer(int32) :: i,j,non_zero

    non_zero=0
    do i=1,size(array,1)
        do j=1,size(array,2)
            if(array(i,j)/=0.0d0 )then
                non_zero=non_zero+1
            endif
        enddo
    enddo
    allocate(x(non_zero),y(non_zero) )

    non_zero=0
    do i=1,size(array,1)
        do j=1,size(array,2)
            if(array(i,j)/=0.0d0 )then
                non_zero=non_zero+1
                x(non_zero) = dble(i)
                y(non_zero) = dble(j)
            endif
        enddo
    enddo

    call plot(x=x, y=y)
    


end subroutine

end module IOClass