module IOClass

#ifdef INTEL
    use IFPORT
#endif

    use iso_fortran_env
    use MathClass
    use StringClass
    implicit none

    integer(int32),parameter :: STAT_ARRAY_SIZE=13

#ifdef INTEL
    integer(int32),parameter :: STAT_ARRAY_SIZE=12    
#endif
    
    
    integer(int32),parameter :: PF_JSON=1
    integer(int32),parameter :: PF_CSV=2
    integer(int32),parameter :: PF_real64=1
    integer(int32),parameter :: PF_int32=2
    integer(int32),parameter :: PF_char=3
    


    type :: IO_
        integer :: fh=100
        logical :: active=.false.
        logical :: EOF=.true.
        character(1) :: state
        character(200)::path,name,extention
        character(:),allocatable:: title
        character(:),allocatable:: xlabel,ylabel,zlabel
        character(:),allocatable :: filename
        integer(int32) :: lastModifiedTime=0
        logical :: json_mode = .false.

        ! if a rule is set
        logical :: activate_rule=.false.
        integer(int32) :: header, offset
        integer(int32),allocatable :: content_type(:)

    contains
        procedure,public :: unit => unitIO

        procedure,public :: numLine => numLineIO

        procedure,public :: flush => flushIO

        !set & reset rule
        procedure,public :: rule => ruleIO
        procedure,public :: ResetRule => ResetRuleIO
        !procedure,public :: import => importIO
        

        procedure,pass   :: openIOchar
        procedure,pass   :: openIOstring

        procedure, pass :: parseIOChar200
        procedure, pass :: parseIO2keysChar200
        generic,public :: parse =>parseIOChar200,parseIO2keysChar200

        generic,public :: open => openIOchar, openIOstring

        ! file properties
        procedure,public :: diff => diffIO
        procedure,public :: updated => diffIO
        procedure,public :: FileDateTime => FileDateTimeIO
        procedure,public :: LastModified => FileDateTimeIO
        procedure,public :: owner => ownerIO ! statb(5)
        procedure,public :: size => sizeIO ! stab(8)

        ! while reading files,
        procedure,public :: rewind => rewindIO
        procedure,public :: goBack => goBackIO
        procedure,public :: goForward => goForwardIO

        !procedure,public :: open => openIO
        procedure,pass :: writeIOchar,writeIOcharchar,writeIOcharcharchar
        procedure,pass :: writeIOstring,writeIOstringstring,writeIOstringstringstring

        ! writer for JSON format
        procedure,pass :: dumpIOJSON_Key_Vector
        procedure,pass :: dumpIOJSON_Key_VectorRe32
        procedure,pass :: dumpIOJSON_Key_VectorInt32
        procedure,pass :: dumpIOJSON_Key_value
        procedure,pass :: dumpIOJSON_Key_valueRe32
        procedure,pass :: dumpIOJSON_Key_valueRe64
        procedure,pass :: dumpIOJSON_Key_valueChar

        procedure,pass :: dumpIOJSON_Key_ArrayRe64
        procedure,pass :: dumpIOJSON_Key_ArrayInt32

        generic,public :: dump =>dumpIOJSON_Key_Vector,dumpIOJSON_Key_VectorRe32,&
        dumpIOJSON_Key_VectorInt32,dumpIOJSON_Key_value,&
        dumpIOJSON_Key_valueRe32,dumpIOJSON_Key_valueRe64,&
        dumpIOJSON_Key_valueChar,dumpIOJSON_Key_ArrayRe64,&
        dumpIOJSON_Key_ArrayInt32
        ! commandline args
        procedure,public :: arg => argIO

        ! WRITE
        ! int-char-int
        procedure,pass :: writeIOint32re64

        procedure,pass :: writeIOint32re64vector
        procedure,pass :: writeIOint32int32vector

        procedure,pass :: writeIOint32
        procedure,pass :: writeIOint32int32
        procedure,pass :: writeIOint32int32int32
        procedure,pass :: writeIOint32int32int32int32
        procedure,pass :: writeIOint32int32int32int32int32
        procedure,pass :: writeIOint32int32int32int32int32int32


        
        procedure,pass :: writeIOint32Vector
        procedure,pass :: writeIOint32Vectorint32Vector
        procedure,pass :: writeIOint32Vectorint32Vectorint32Vector
        procedure,pass :: writeIOint32Vectorint32Vectorre64Vector
        procedure,pass :: writeIOint32Vectorre64Vector
        procedure,pass :: writeIOre64Vectorre64Vector
        procedure,pass :: writeIOre64Vectorre64Vectorre64Vector
        procedure,pass :: writeIOint32Array

        procedure,pass :: writeIOre64
        procedure,pass :: writeIOre64re64
        procedure,pass :: writeIOre64re64re64
        procedure,pass :: writeIOre64re64re64re64
        procedure,pass :: writeIOre64re64re64re64re64
        procedure,pass :: writeIOre64re64re64re64re64re64

        
        procedure,pass :: writeIOre64Vector
        procedure,pass :: writeIOre64Array
        procedure,pass :: writeIOcomplex64
        procedure,pass :: writeIOcomplex64Vector
        procedure,pass :: writeIOcomplex64Array
        generic,public :: write => writeIOchar,writeIOstring,writeIOre64,writeIOre64Vector,writeIOre64Array,&
            writeIOint32,writeIOint32Vector,writeIOint32Array,&
            writeIOre64re64,writeIOre64re64re64,writeIOre64re64re64re64,&
            writeIOre64re64re64re64re64,writeIOre64re64re64re64re64re64,&
            writeIOint32int32,writeIOint32int32int32,writeIOint32int32int32int32,&
            writeIOint32int32int32int32int32,writeIOint32int32int32int32int32int32,&
            writeIOstringstring,writeIOstringstringstring,&
            writeIOcharchar,writeIOcharcharchar,&
            writeIOint32re64vector,writeIOint32int32vector,&
            writeIOint32re64,&
            writeIOcomplex64,writeIOcomplex64Vector,writeIOcomplex64Array,&
            writeIOint32Vectorint32Vector,&
            writeIOint32Vectorre64Vector,&
            writeIOre64Vectorre64Vector,writeIOre64Vectorre64Vectorre64Vector,&
            writeIOint32Vectorint32Vectorint32Vector,&
            writeIOint32Vectorint32Vectorre64Vector
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

        
        procedure,pass :: plotIO
        procedure,pass :: plotIODirect
        procedure,pass :: plotIODirectReal32
        generic,public :: plot => plotIO, plotIODirect,plotIODirectReal32
        
        procedure,public :: splot => splotIO


        procedure,public :: readline => readlineIO
        procedure,public :: close => closeIO    
    end type


    interface lowercase
        module procedure lowercaseChar, lowercaseString
    end interface 
    
    interface print
        module procedure printChar, printReal64,printComplex64, &
            printReal32, printInt64, printInt32,printInt32Int32, printCharAndIntVector
    end interface print

    interface disp
        module procedure printChar, printReal64, printReal32, printInt64, printInt32
    end interface disp

    interface plot
        module procedure plotRealArray
    end interface

    interface spy
        module procedure spyRealArray
    end interface


    interface CaesarCipher
        module procedure CaesarCipherChar
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
recursive subroutine openIOchar(obj,path,state,name,extention,fh)
    class(IO_),intent(inout) :: obj
    character(1),optional,intent(in) :: state ! w or r
    character(*),optional,intent(in)::path,name,extention
    character(:),allocatable :: localcp
    integer(int32),optional,intent(in) :: fh
    integer(int32) :: tag
    logical :: yml=.False.

    if(present(state) )then
        obj%state = state
    else
        obj%state="w"
    endif


    
    !if( index(path,"mongo://")/=0  )then
    !    
    !    return
    !endif


    if( index(path,"https://")/=0 .or. index(path,"http://")/=0 )then
        ! get online file
        ! read-only
        if(present(state) )then
            if(state=="w" .or. state=="a")then
                print *, "ERROR :: OpenIOChar :: Online files are read-only."
                stop
            endif
        endif
        ! download file
        tag = index(trim(path),"/",back=.true.)
        call execute_command_line("mkdir -p ./tmp")
        call execute_command_line("wget --no-check-certificate  '"//trim(path)//"' -O ./tmp/"//trim(path(tag+1:)))
        print *, "[ok] Downloaded > ./tmp/"//trim(path(tag+1:) )
        localcp = trim("./tmp/"//path(tag+1:) )
        call obj%open(trim(localcp),"r")
        return
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

    tag = index(trim(path),"/",back=.true.)
    if(tag/=0)then
        call execute_command_line("mkdir -p "//path(1:tag-1) )
    endif

    if(present(path) )then
        obj%path=trim(path)
        if(present(name) )then
            obj%name=trim(name)
            if(present(extention) )then
                obj%extention=trim(extention)
                if(present(state) )then
                    if(state=="r")then
                        open(newunit=obj%fh,file=trim(path)//trim(name)//trim(extention),status='old')
                        obj%filename=trim(path)//trim(name)//trim(extention)
                    elseif(state=="w")then
                        open(newunit=obj%fh,file=trim(path)//trim(name)//trim(extention),status='replace')
                        obj%filename = trim(path)//trim(name)//trim(extention)
                    elseif(state=="a")then
                        open(newunit=obj%fh,file=trim(path)//trim(name)//trim(extention),position='append')
                        obj%filename = trim(path)//trim(name)//trim(extention)
                    else
                        call print("Error :: IOClass % open >> argument <state> should be w or r ")
                        stop
                    endif
                else
                    open(newunit=obj%fh,file=trim(path)//trim(name)//trim(extention) )
                    obj%filename = trim(path)//trim(name)//trim(extention)
                endif
            else
                if(present(state) )then
                    if(state=="r")then
                        open(newunit=obj%fh,file=trim(path)//trim(name),status='old' )
                        obj%filename = trim(path)//trim(name)
                    elseif(state=="w")then
                        open(newunit=obj%fh,file=trim(path)//trim(name),status='replace' )
                        obj%filename=trim(path)//trim(name)
                    elseif(state=="a")then
                        open(newunit=obj%fh,file=trim(path)//trim(name),position='append' )
                        obj%filename=trim(path)//trim(name)
                    else
                        call print("Error :: IOClass % open >> argument <state> should be w or r ")
                        stop
                    endif
                else
                    open(newunit=obj%fh,file=trim(path)//trim(name) )
                    obj%filename = trim(path)//trim(name)
                endif
            endif
        else
            if(present(state) )then
                if(state=="r")then
                    open(newunit=obj%fh,file=trim(path),status='old' )
                    obj%filename = trim(path)
                elseif(state=="w")then
                    open(newunit=obj%fh,file=trim(path),status='replace' )
                    obj%filename = trim(path)
                elseif(state=="a")then
                    open(newunit=obj%fh,file=trim(path),position='append' )
                    obj%filename = trim(path)
                else
                    call print("Error :: IOClass % open >> argument <state> should be w or r ")
                    stop
                endif
            else
                open(newunit=obj%fh,file=trim(path) )
                obj%filename = trim(path)
            endif
        endif
    else
        open(newunit=obj%fh,file="./untitled.txt",status="replace" )
        obj%filename = "./untitled.txt"
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
            obj%filename = trim(path_s%str())
        elseif(state == "r")then
            open(newunit=obj%fh,file=trim(path_s%str()),status="old" )
            obj%filename = trim(path_s%str())
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
            obj%filename = trim(path_s%str())//trim(name_s%str())//trim(extention_s%str())
        else
            open(newunit=obj%fh,file=trim(path_s%str())//trim(name_s%str()) )
            obj%filename = trim(path_s%str())//trim(name_s%str()) 
        endif
    else
        open(newunit=obj%fh,file=trim(path_s%str()) )
        obj%filename = trim(path_s%str())
    endif
    
    obj%EOF = .false.

end subroutine openIOstring
! #############################################



! #############################################
subroutine writeIOchar(obj,char,append,advance)
    class(IO_),intent(inout) :: obj
    character(*),intent(in) :: char
    logical,optional,intent(in) :: append,advance
    logical :: adv
    
    if(obj%state=="r")then
        call print("IOClass >> Error >> This file is readonly. ")
        call print("Nothing is written.")
        return
    endif
    
    adv = .true.
    if(present(append) )then
        adv = .not.append
    endif
    if(present(advance) )then
        adv = advance
    endif
    if(adv .eqv. .true.)then
        write(obj%fh, '(A)') char
    else
        write(obj%fh, '(A)',advance="no") char
    endif
end subroutine writeIOchar
! #############################################


! #############################################
subroutine writeIOcharchar(obj,char1,char2)
    class(IO_),intent(inout) :: obj
    character(*),intent(in) :: char1,char2

    if(obj%state=="r")then
        call print("IOClass >> Error >> This file is readonly. ")
        call print("Nothing is written.")
        return
    endif
    
    write(obj%fh, '(A)') char1//" "//char2

end subroutine writeIOcharchar
! #############################################


! #############################################
subroutine writeIOcharcharchar(obj,char1,char2,char3)
    class(IO_),intent(inout) :: obj
    character(*),intent(in) :: char1,char2,char3

    if(obj%state=="r")then
        call print("IOClass >> Error >> This file is readonly. ")
        call print("Nothing is written.")
        return
    endif
    
    write(obj%fh, '(A)') char1//" "//char2//" "//char3

end subroutine writeIOcharcharchar
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
subroutine writeIOint32re64(obj,in32,re64)
    class(IO_),intent(inout) :: obj
    integer(int32),intent(in) :: in32
    real(real64),intent(in) :: re64

    if(obj%state=="r")then
        call print("IOClass >> Error >> This file is readonly. ")
        call print("Nothing is written.")
        return
    endif
    
    write(obj%fh, '(A)') trim(str(in32))//" "//trim(str(re64))

end subroutine writeIOint32re64
! #############################################

! #############################################


subroutine writeIOint32int32(obj,in32_1,in32_2)
    class(IO_),intent(inout) :: obj
    integer(int32),intent(in) :: in32_1,in32_2

    if(obj%state=="r")then
        call print("IOClass >> Error >> This file is readonly. ")
        call print("Nothing is written.")
        return
    endif
    
    write(obj%fh, '(A)') trim(str(in32_1))//" "//trim(str(in32_2))

end subroutine 

! ####################################################

! #############################################


subroutine writeIOint32int32int32(obj,in32_1,in32_2,in32_3)
    class(IO_),intent(inout) :: obj
    integer(int32),intent(in) :: in32_1,in32_2,in32_3

    if(obj%state=="r")then
        call print("IOClass >> Error >> This file is readonly. ")
        call print("Nothing is written.")
        return
    endif
    
    write(obj%fh, '(A)') trim(str(in32_1))//" "//trim(str(in32_2))//" "//trim(str(in32_3))

end subroutine 

! ####################################################

! #############################################


subroutine writeIOint32int32int32int32(obj,in32_1,in32_2,in32_3,in32_4)
    class(IO_),intent(inout) :: obj
    integer(int32),intent(in) :: in32_1,in32_2,in32_3,in32_4

    if(obj%state=="r")then
        call print("IOClass >> Error >> This file is readonly. ")
        call print("Nothing is written.")
        return
    endif
    
    write(obj%fh, '(A)') trim(str(in32_1))//" "//trim(str(in32_2))//" "//trim(str(in32_3))//" "//trim(str(in32_4))

end subroutine 

! ####################################################


! #############################################


subroutine writeIOint32int32int32int32int32(obj,in32_1,in32_2,in32_3,in32_4,in32_5)
    class(IO_),intent(inout) :: obj
    integer(int32),intent(in) :: in32_1,in32_2,in32_3,in32_4,in32_5

    if(obj%state=="r")then
        call print("IOClass >> Error >> This file is readonly. ")
        call print("Nothing is written.")
        return
    endif
    
    write(obj%fh, '(A)') trim(str(in32_1))//" "//trim(str(in32_2))//" "&
        //trim(str(in32_3))//" "//trim(str(in32_4))//" "//trim(str(in32_5))

end subroutine 

! ####################################################


! #############################################


subroutine writeIOint32int32int32int32int32int32(obj,in32_1,in32_2,in32_3,in32_4,in32_5,in32_6)
    class(IO_),intent(inout) :: obj
    integer(int32),intent(in) :: in32_1,in32_2,in32_3,in32_4,in32_5,in32_6

    if(obj%state=="r")then
        call print("IOClass >> Error >> This file is readonly. ")
        call print("Nothing is written.")
        return
    endif
    
    write(obj%fh, '(A)') trim(str(in32_1))//" "//trim(str(in32_2))//" "&
        //trim(str(in32_3))//" "//trim(str(in32_4))//" "//trim(str(in32_5))&
        //" "//trim(str(in32_6))

end subroutine 

! ####################################################



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
subroutine writeIOint32VectorInt32vector(obj,in32,in32_c)
    class(IO_),intent(inout) :: obj
    integer(int32),intent(in) :: in32(:),in32_c(:)
    integer(int32) :: i

    if(obj%state=="r")then
        call print("IOClass >> Error >> This file is readonly. ")
        call print("Nothing is written.")
        return
    endif
    do i=1,size(in32)
        write(obj%fh, '(A)') trim(str(in32(i) ))//" "//trim(str(in32_c(i) ))
    enddo
end subroutine
! #############################################

! #############################################
subroutine writeIOint32VectorInt32vectorInt32Vector(obj,in32,in32_c,in32_cc)
    class(IO_),intent(inout) :: obj
    integer(int32),intent(in) :: in32(:),in32_c(:),in32_cc(:)
    integer(int32) :: i

    if(obj%state=="r")then
        call print("IOClass >> Error >> This file is readonly. ")
        call print("Nothing is written.")
        return
    endif
    do i=1,size(in32)
        write(obj%fh, '(A)') trim(str(in32(i) ))//" "//trim(str(in32_c(i) )//" "//trim(str(in32_cc(i) )))
    enddo
end subroutine
! #############################################


! #############################################
subroutine writeIOint32VectorInt32vectorre64Vector(obj,in32,in32_c,re64_cc)
    class(IO_),intent(inout) :: obj
    integer(int32),intent(in) :: in32(:),in32_c(:)
    real(real64),intent(in) :: re64_cc(:)
    integer(int32) :: i

    if(obj%state=="r")then
        call print("IOClass >> Error >> This file is readonly. ")
        call print("Nothing is written.")
        return
    endif
    do i=1,size(in32)
        write(obj%fh, '(A)') trim(str(in32(i) ))//" "//trim(str(in32_c(i) )//" "//trim(str(re64_cc(i) )))
    enddo
end subroutine
! #############################################



! #############################################
subroutine writeIOint32VectorRe64vector(obj,in32,Re64)
    class(IO_),intent(inout) :: obj
    integer(int32),intent(in) :: in32(:)
    real(real64),intent(in) :: Re64(:)
    integer(int32) :: i

    if(obj%state=="r")then
        call print("IOClass >> Error >> This file is readonly. ")
        call print("Nothing is written.")
        return
    endif
    do i=1,size(in32)
        write(obj%fh, '(A)') trim(str(in32(i) ))//" "//trim(str(Re64(i) ))
    enddo
end subroutine
! #############################################


! #############################################
subroutine writeIORe64VectorRe64vector(obj,Re64_c,Re64)
    class(IO_),intent(inout) :: obj
    real(real64),intent(in)  :: Re64_c(:)
    real(real64),intent(in) :: Re64(:)
    integer(int32) :: i

    if(obj%state=="r")then
        call print("IOClass >> Error >> This file is readonly. ")
        call print("Nothing is written.")
        return
    endif
    do i=1,size(Re64_c)
        write(obj%fh, '(A)') trim(str(Re64_c(i) ))//" "//trim(str(Re64(i) ))
    enddo
end subroutine
! #############################################



! #############################################
subroutine writeIORe64VectorRe64vectorRe64vector(obj,Re64_cc,Re64_c,Re64)
    class(IO_),intent(inout) :: obj
    real(real64),intent(in)  :: Re64_cc(:)
    real(real64),intent(in)  :: Re64_c(:)
    real(real64),intent(in) :: Re64(:)
    integer(int32) :: i

    if(obj%state=="r")then
        call print("IOClass >> Error >> This file is readonly. ")
        call print("Nothing is written.")
        return
    endif
    do i=1,size(Re64_c)
        write(obj%fh, '(A)') trim(str(Re64_cc(i) ))//" "//trim(str(Re64_c(i) ))//" "//trim(str(Re64(i) ))
    enddo
end subroutine
! #############################################


! #############################################
subroutine writeIOint32int32vector(obj,in32_1,in32)
    class(IO_),intent(inout) :: obj
    integer(int32),intent(in) :: in32_1,in32(:)
    integer(int32) :: i

    if(obj%state=="r")then
        call print("IOClass >> Error >> This file is readonly. ")
        call print("Nothing is written.")
        return
    endif

    write(obj%fh, '(A)',advance="no") trim(str(in32_1))
    do i=1,size(in32)
        write(obj%fh, '(A)',advance="no") " "//trim(str(in32(i) ))
    enddo
    write(obj%fh, '(A)',advance="yes") " "
end subroutine
! #############################################


! #############################################
subroutine writeIOint32re64Vector(obj,in32_1,re64)
    class(IO_),intent(inout) :: obj
    integer(int32),intent(in) :: in32_1
    real(real64),intent(in) :: re64(:)
    integer(int32) :: i

    if(obj%state=="r")then
        call print("IOClass >> Error >> This file is readonly. ")
        call print("Nothing is written.")
        return
    endif

    write(obj%fh, '(A)',advance="no") trim(str(in32_1))
    do i=1,size(re64)
        write(obj%fh, '(A)',advance="no") " "//trim(str(re64(i) ))
    enddo
    write(obj%fh, '(A)',advance="yes") " "
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



subroutine writeIOre64re64(obj,re64_1,re64_2)
    class(IO_),intent(inout) :: obj
    real(real64),intent(in) :: re64_1,re64_2

    if(obj%state=="r")then
        call print("IOClass >> Error >> This file is readonly. ")
        call print("Nothing is written.")
        return
    endif
    if( isnan(re64_1) .or. abs(re64_1) > HUGE(real64)  )then
        write(obj%fh, '(A)') "NaN "//trim(str(re64_2))
    elseif( isnan(re64_2) .or. abs(re64_2) > HUGE(real64) )then
        write(obj%fh, '(A)') trim(str(re64_1))//" NaN"
    else
        write(obj%fh, '(A)') trim(str(re64_1))//" "//trim(str(re64_2))
    endif
end subroutine 

! ####################################################

! #############################################


subroutine writeIOre64re64re64(obj,re64_1,re64_2,re64_3)
    class(IO_),intent(inout) :: obj
    real(real64),intent(in) :: re64_1,re64_2,re64_3

    if(obj%state=="r")then
        call print("IOClass >> Error >> This file is readonly. ")
        call print("Nothing is written.")
        return
    endif
    
    write(obj%fh, '(A)') trim(str(re64_1))//" "//trim(str(re64_2))//" "//trim(str(re64_3))

end subroutine 

! ####################################################

! #############################################


subroutine writeIOre64re64re64re64(obj,re64_1,re64_2,re64_3,re64_4)
    class(IO_),intent(inout) :: obj
    real(real64),intent(in) :: re64_1,re64_2,re64_3,re64_4

    if(obj%state=="r")then
        call print("IOClass >> Error >> This file is readonly. ")
        call print("Nothing is written.")
        return
    endif
    
    write(obj%fh, '(A)') trim(str(re64_1))//" "//trim(str(re64_2))//" "//trim(str(re64_3))//" "//trim(str(re64_4))

end subroutine 

! ####################################################


! #############################################


subroutine writeIOre64re64re64re64re64(obj,re64_1,re64_2,re64_3,re64_4,re64_5)
    class(IO_),intent(inout) :: obj
    real(real64),intent(in) :: re64_1,re64_2,re64_3,re64_4,re64_5

    if(obj%state=="r")then
        call print("IOClass >> Error >> This file is readonly. ")
        call print("Nothing is written.")
        return
    endif
    
    write(obj%fh, '(A)') trim(str(re64_1))//" "//trim(str(re64_2))//" "&
        //trim(str(re64_3))//" "//trim(str(re64_4))//" "//trim(str(re64_5))

end subroutine 

! ####################################################


! #############################################


subroutine writeIOre64re64re64re64re64re64(obj,re64_1,re64_2,re64_3,re64_4,re64_5,re64_6)
    class(IO_),intent(inout) :: obj
    real(real64),intent(in) :: re64_1,re64_2,re64_3,re64_4,re64_5,re64_6

    if(obj%state=="r")then
        call print("IOClass >> Error >> This file is readonly. ")
        call print("Nothing is written.")
        return
    endif
    
    write(obj%fh, '(A)') trim(str(re64_1))//" "//trim(str(re64_2))//" "&
        //trim(str(re64_3))//" "//trim(str(re64_4))//" "//trim(str(re64_5))&
        //" "//trim(str(re64_6))

end subroutine 

! ####################################################




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
subroutine writeIOstringstring(obj,string1,string2)
    class(IO_),intent(inout) :: obj
    type(String_),intent(in) :: string1,string2

    if(obj%state=="r")then
        call print("IOClass >> Error >> This file is readonly. ")
        call print("Nothing is written.")
        return
    endif
    write(obj%fh, '(A)') str(string1)//str(string2)
    

end subroutine 
! #############################################

! #############################################
subroutine writeIOstringstringstring(obj,string1,string2,string3)
    class(IO_),intent(inout) :: obj
    type(String_),intent(in) :: string1,string2,string3

    if(obj%state=="r")then
        call print("IOClass >> Error >> This file is readonly. ")
        call print("Nothing is written.")
        return
    endif
    write(obj%fh, '(A)') str(string1)//" "//str(string2)//" "//str(string3)
    

end subroutine 
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
subroutine closeIO(obj,status)
    class(IO_),intent(inout) :: obj
    character(*),optional,intent(in) :: status

    if(obj%json_mode)then
        call obj%write('    "plantfem_end_signal":true')
        call obj%write("}")
        obj%json_mode = .false.
    endif

    if(obj%active .eqv. .false.)then
        print *, "ERROR :: "//"file is already closed. filename = "//obj%filename
        return
    endif
    if(present(status) )then
        close(obj%fh,status=status)
    else
        close(obj%fh)
    endif
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

subroutine printCharAndIntVector(char, IntVec)
    character(*),intent(in) :: char
    integer(int32),intent(in) :: IntVec(:)
    integer(int32) :: i

    call print(char)
    do i=1, size(IntVec)
        call print(str(i)//":"//str(IntVec(i) )    )
    enddo
end subroutine

! #############################################
subroutine printReal64(re64)
    real(real64),intent(in) :: re64
    character(20) :: char

    write(char, '(f20.10)') re64
    write(*,'(A)' ) trim(adjustl(char))

end subroutine
! #############################################


! #############################################
subroutine printComplex64(re64)
    complex(real64),intent(in) :: re64
    character(20) :: char

    write(*,*) re64

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

    character(40) :: char

    write(char, *) in64
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

! #############################################
subroutine printint32int32(in32,int32_c)
    integer(int32),intent(in) :: in32,int32_c

    character(20) :: char,char_c

    write(char, '(i10)') in32
    write(char_c, '(i10)') int32_c
    write(*,'(A)' ) trim(adjustl(char))//" "//trim(adjustl(char_c))
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
    
        call execute_command_line("gnuplot __plotRealArray__buf_.gp")
    
        call execute_command_line("rm __plotRealArray__buf_.txt")
        call execute_command_line("rm __plotRealArray__buf_.gp")
    
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
    
    if(present(xr) )then
        call f%write('set xr'//trim(xr) )
    endif
    if(present(yr) )then
        call f%write('set yr'//trim(yr) )
    endif
    call f%write('plot "__plotRealArray__buf_.txt" w l')
    call f%write("pause 10")
    call f%close()

    call execute_command_line("gnuplot __plotRealArray__buf_.gp")

    call execute_command_line("rm __plotRealArray__buf_.txt")
    call execute_command_line("rm __plotRealArray__buf_.gp")

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

! ################################################################

! ################################################################
subroutine plotIODirect(obj,x,Fx,option,logscale)
    class(IO_),intent(inout) ::  obj
    real(real64),intent(in) :: x(:),Fx(:)
    character(*),optional,intent(in) :: option
    logical,optional,intent(in) :: logscale


    call obj%open("__plotIODirect__.txt")
    call obj%write(x,Fx)
    call obj%close()

    call obj%plot(option=option,logscale=logscale)
end subroutine
! ################################################################

! ################################################################
subroutine plotIODirectReal32(obj,x,Fx,option,logscale)
    class(IO_),intent(inout) ::  obj
    real(real32),intent(in) :: x(:),Fx(:)
    character(*),optional,intent(in) :: option
    logical,optional,intent(in) :: logscale


    call obj%open("__plotIODirect__.txt")
    call obj%write(dble(x),dble(Fx))
    call obj%close()

    call obj%plot(option=option,logscale=logscale)
end subroutine
! ################################################################


! ################################################################
subroutine plotIO(obj,name,option,logscale)
    class(IO_),intent(inout) ::  obj
    character(*),optional,intent(in) :: name
    character(*),optional,intent(in) :: option
    type(IO_) :: gp_script
    logical,optional,intent(in) :: logscale

    if(present(name) )then
        obj%filename = name
    endif
    call obj%open(obj%filename,"r")
    call gp_script%open(trim(obj%filename)//"_gp_script.gp","w")
    call gp_script%write("set xlabel '"//obj%xlabel//"'")
    call gp_script%write("set ylabel '"//obj%ylabel//"'")
    call gp_script%write("set title '"//obj%title//"'")
    if(present(logscale) )then
        if(logscale)then
            call gp_script%write("set logscale")
        endif
    endif
    if(present(option) )then
        call gp_script%write("plot '"//obj%filename//"' "//option)
    else
        call gp_script%write("plot '"//obj%filename//"' ")
    endif


    call gp_script%close()
    call execute_command_line("gnuplot "//trim(obj%filename)//"_gp_script.gp -pause")
    call obj%close()
end subroutine
! ################################################################


! ################################################################
subroutine splotIO(obj,name,option,logscale)
    class(IO_),intent(inout) ::  obj
    character(*),optional,intent(in) :: name
    character(*),optional,intent(in) :: option
    type(IO_) :: gp_script
    logical,optional,intent(in) :: logscale

    if(present(name) )then
        obj%filename = name
    endif
    call obj%open(obj%filename,"r")
    call gp_script%open(trim(obj%filename)//"_gp_script.gp","w")
    call gp_script%write("set xlabel '"//obj%xlabel//"'")
    call gp_script%write("set ylabel '"//obj%ylabel//"'")
    call gp_script%write("set zlabel '"//obj%zlabel//"'")
    call gp_script%write("set title '"//obj%title//"'")
    if(present(logscale) )then
        if(logscale)then
            call gp_script%write("set logscale")
        endif
    endif
    if(present(option) )then
        call gp_script%write("splot '"//obj%filename//"' "//option)
    else
        call gp_script%write("splot '"//obj%filename//"' ")
    endif

    call gp_script%close()
    call execute_command_line("gnuplot "//trim(obj%filename)//"_gp_script.gp -pause")
    call obj%close()
end subroutine

subroutine flushIO(obj)
    class(IO_),intent(in) :: obj

    flush(obj%fh)
    
end subroutine



function parseIOChar200(obj,filename,fileformat,key1,debug) result(ret)
    class(IO_),intent(inout) :: obj
    integer(int32),optional,intent(in) :: fileformat
    character(*),intent(in) :: key1
    character(*),optional,intent(in) :: filename
    character(:),allocatable :: line
    integer(int32)::blcount=0
    integer(int32)::rmc,id,fformat
    character(:),allocatable :: ret
    logical,optional,intent(in) :: debug

    ret = " "
    fformat = input(default=1,option=fileformat)

    if(present(filename) )then
        call obj%open(filename,"r")
        if(index(filename,".json")/=0 )then
            fformat = 1
        endif
    endif



    if(fformat==PF_JSON)then 
        do
            line = obj%readline()
            if(present(debug) )then
                if(debug)then
                    print *, trim(line)
                endif
            endif
            if( adjustl(trim(line))=="{" )then
                blcount=1
                cycle
            endif
            if( adjustl(trim(line))=="}" )then
                exit
            endif

            if(blcount==1)then
                if(index(line,trim(key1) )/=0)then
                    rmc=index(line,",")
                    ! カンマがあれば除く
                    if(rmc /= 0)then
                        line(rmc:rmc)=" "
                    endif
                    id = index(line,":")
                    read(line(id+1:),*) ret
                    ret = adjustl(ret)

                    ! [があれば]まで読む
                    if( index(ret, "[")/=0 .and. index(ret, "]")==0 )then
                        do 
                            line = obj%readline()
                            ret = trim(ret) // trim(line)
                            if(index(ret, "]")/=0) exit
                        enddo
                    endif

                    if(index(ret, "],")/=0 )then
                        ret(index(ret, "],",back=.true.)+1:index(ret, "],",back=.true.)+1) = " "
                    endif
                    exit
                else
                    cycle
                endif
            endif

            if(obj%EOF) exit

        enddo
    endif
    
    if(present(filename) )then
        call obj%close()
    endif
    

end function



function parseIO2keysChar200(obj,filename,fileformat,key1,key2,debug) result(ret)
    class(IO_),intent(inout) :: obj
    integer(int32),optional,intent(in) :: fileformat
    character(*),intent(in) :: key1,key2
    character(*),optional,intent(in) :: filename
    character(:),allocatable :: line
    integer(int32)::blcount=0
    integer(int32)::rmc,id,fformat
    character(200) :: ret
    logical,optional,intent(in) :: debug
    ret = " "

    fformat = input(default=1,option=fileformat)
    if(present(filename) )then
        call obj%open(filename,"r")
        if(index(filename,".json")/=0 )then
            fformat = 1
        endif
    endif


    if(fformat==PF_JSON)then 
        do
            line = obj%readline()
            if(present(debug) )then
                if(debug)then
                    print *, trim(line)
                endif
            endif
            if( adjustl(trim(line))=="{" )then
                blcount=1
                cycle
            endif
            if( adjustl(trim(line))=="}" )then
                exit
            endif

            if(blcount==1)then
                if(index(line,trim(key1) )/=0)then
                    ! search for the second key
                    do 
                        line = obj%readline()
                        if(present(debug) )then
                            if(debug)then
                                print *, trim(line)
                            endif
                        endif

                        if( index(line,"}")/=0 )then
                            exit
                        endif
                        
                        
                        if(index(line,trim(key2))/=0 )then
                            rmc=index(line,",")
                            if(rmc /= 0)then
                                line(rmc:rmc)=" "
                            endif
                            id = index(line,":")
                            read(line(id+1:),*) ret
                            ret = adjustl(ret)
                            ! [があれば]まで読む
                            if( index(ret, "[")/=0 .and. index(ret, "]")==0 )then
                                do 
                                    line = obj%readline()
                                    ret = trim(ret) // trim(line)
                                    if(index(ret, "]")/=0) exit
                                enddo
                            endif
                            if(index(ret, "],")/=0 )then
                                ret(index(ret, "],",back=.true.)+1:index(ret, "],",back=.true.)+1) = " "
                            endif
                            exit
                        endif

                    enddo
                else
                    cycle
                endif
            endif

            if(obj%EOF) exit

        enddo
    endif
    
    if(present(filename) )then
        call obj%close()
    endif
    

end function
! #################################################################


! #################################################################
! get last modified time
function FileDateTimeIO(obj,name) result(ret)
    class(IO_),intent(inout) :: obj
    character(*),optional,intent(in) :: name
    integer(int32) :: ret, ierr,infostat(STAT_ARRAY_SIZE)

    if(present(name) )then
        call obj%open(name,"r")
        ierr = lstat(name, infostat)
        ret = infostat(10)
        call obj%close()
    else
        ierr = lstat(obj%filename, infostat)
        ret = infostat(10)
    endif

end function
! #################################################################


! #################################################################
! get owner use id
function ownerIO(obj,name) result(ret)
    class(IO_),intent(inout) :: obj
    character(*),optional,intent(in) :: name
    integer(int32) :: ret, ierr,infostat(STAT_ARRAY_SIZE)

    if(present(name) )then
        call obj%open(name,"r")
        ierr = lstat(name, infostat)
        ret = infostat(5)
        call obj%close()
    else
        ierr = lstat(obj%filename, infostat)
        ret = infostat(5)
    endif

end function
! #################################################################


! #################################################################
! get file size
function sizeIO(obj,name) result(ret)
    class(IO_),intent(inout) :: obj
    character(*),optional,intent(in) :: name
    integer(int32) :: ret, ierr,infostat(STAT_ARRAY_SIZE)

    if(present(name) )then
        call obj%open(name,"r")
        ierr = lstat(name, infostat)
        ret = infostat(8)
        call obj%close()
    else
        ierr = lstat(obj%filename, infostat)
        ret = infostat(8)
    endif

end function
! #################################################################



! #################################################################
! detect diff
function diffIO(obj,name) result(ret)
    class(IO_),intent(inout) :: obj
    character(*),optional,intent(in) ::name
    logical :: ret
    integer(int32) :: lastmodified
    ret=.false.
    if(present(name) )then
        if( obj%lastModifiedTime == obj%LastModified( trim(name) ) )then
            obj%lastModifiedTime = obj%LastModified( trim(name) )
            ret = .false.
        else
            obj%lastModifiedTime = obj%LastModified( trim(name) )
            ret = .true.
        endif
    else
        if( obj%lastModifiedTime == obj%LastModified() )then
            obj%lastModifiedTime = obj%LastModified()
            ret = .false.
        else
            obj%lastModifiedTime = obj%LastModified()
            ret = .true.
        endif
    endif

end function
! #################################################################


! #################################################################
subroutine rewindIO(obj) 
    class(IO_),intent(inout) :: obj

    rewind(obj%fh)
    
end subroutine
! #################################################################

! #################################################################
subroutine goBackIO(obj,lines) 
    class(IO_),intent(inout) :: obj
    integer(int32),optional,intent(in) :: lines
    integer(int32) :: numlines,i

    numlines = input(default=1,option=lines)
    do i=1,numlines
        backspace(obj%fh)
    enddo
    
end subroutine
! #################################################################

! #################################################################
subroutine goForwardIO(obj,lines) 
    class(IO_),intent(inout) :: obj
    integer(int32),optional,intent(in) :: lines
    integer(int32) :: numlines,i
    character(1):: a
    numlines = input(default=1,option=lines)
    do i=1,numlines
        read(obj%fh,'(A)') a
    enddo
    
end subroutine
! #################################################################

! #################################################################
function argIO(obj,id) result(arg)
    class(IO_),intent(in) :: obj
    integer(int32),intent(in) :: id
    character(200) :: arg
    integer(int32) :: status,length
    arg(:)=" "
    call get_command_argument(id, length = length, status = status)

    if(status==0)then
        call get_command_argument(id, arg(1:length), status = status)
    else
        arg(:)=" "
    endif
end function
! #################################################################

! #################################################################
function NumberOfArgIO(obj) result(ret)
    class(IO_),intent(in) :: obj
    integer(int32) :: ret

    ret = command_argument_count()
    
end function
! #################################################################

! Write for json
! #################################################################
subroutine dumpIOJSON_Key_Vector(obj,key,valueVector)
    class(IO_),intent(inout) :: obj
    character(*),intent(in) :: key
    real(real64),intent(in) :: valueVector(:)
    integer(int32) :: i, n
    
    if( index(obj%filename,".json")==0 )then
        print *, "writeIOJSON_Key_Vector >> obj%filename should contain .json"
        return
    endif

    if(.not.obj%json_mode)then
        ! first time
        obj%json_mode = .true.
        call obj%write("{")
    endif

    call obj%write('    "'//key//'":[')
    n =size(valueVector)
    do i =1,n-1
        if(abs(valueVector(i)) < 1.0d0 )then
            call obj%write("    0"//trim(str(valueVector(i)))//","  )
        else
            call obj%write("    "//trim(str(valueVector(i)))//","  )
        endif
    enddo
    if(abs(valueVector(n)) < 1.0d0 )then
        call obj%write("    0"//trim(str(valueVector( n ))))
    else
        call obj%write("    "//trim(str(valueVector( n ))))
    endif
    call obj%write("    ],")
    
end subroutine
! #################################################################

! #################################################################
subroutine dumpIOJSON_Key_VectorRe32(obj,key,valueVector)
    class(IO_),intent(inout) :: obj
    character(*),intent(in) :: key
    real(real32),intent(in) :: valueVector(:)
    integer(int32) :: i, n
    
    if( index(obj%filename,".json")==0 )then
        print *, "writeIOJSON_Key_Vector >> obj%filename should contain .json"
        return
    endif

    if(.not.obj%json_mode)then
        ! first time
        obj%json_mode = .true.
        call obj%write("{")
    endif

    call obj%write('    "'//key//'":[')
    n =size(valueVector)
    do i =1,n-1
        if(abs(valueVector(i)) < 1.0d0 )then
            call obj%write("    0"//trim(str(valueVector(i)))//","  )
        else
            call obj%write("    "//trim(str(valueVector(i)))//","  )
        endif
    enddo
    if(abs(valueVector(n)) < 1.0d0 )then
        call obj%write("    0"//trim(str(valueVector( n ))))
    else
        call obj%write("    "//trim(str(valueVector( n ))))
    endif
    call obj%write("    ],")

end subroutine
! #################################################################

! #################################################################
subroutine dumpIOJSON_Key_VectorInt32(obj,key,valueVector)
    class(IO_),intent(inout) :: obj
    character(*),intent(in) :: key
    integer(int32),intent(in) :: valueVector(:)
    integer(int32) :: i, n
    
    if( index(obj%filename,".json")==0 )then
        print *, "writeIOJSON_Key_Vector >> obj%filename should contain .json"
        return
    endif

    if(.not.obj%json_mode)then
        ! first time
        obj%json_mode = .true.
        call obj%write("{")
    endif

    call obj%write('    "'//key//'":[')
    n =size(valueVector)
    do i =1,n-1
        call obj%write("    "//trim(str(valueVector(i)))//","  )
    enddo
    call obj%write("    "//trim(str(valueVector( n ))))
    call obj%write("    ],")
    
end subroutine
! #################################################################


! #################################################################
subroutine dumpIOJSON_Key_ArrayInt32(obj,key,valueVector)
    class(IO_),intent(inout) :: obj
    character(*),intent(in) :: key
    integer(int32),intent(in) :: valueVector(:,:)
    integer(int32) :: i, n,j,m
    
    if( index(obj%filename,".json")==0 )then
        print *, "writeIOJSON_Key_Vector >> obj%filename should contain .json"
        return
    endif

    if(.not.obj%json_mode)then
        ! first time
        obj%json_mode = .true.
        call obj%write("{")
    endif

    n =size(valueVector,2)
    m = size(valueVector,1)

    call obj%write('    "'//key//'":[',advance=.false.)
    do j=1, m-1
        write(obj%fh,'(A)',advance="no")  "["//str(valueVector(j,1))//","

        do i =2,n-1
            write(obj%fh,'(A)',advance="no")  " "//str(valueVector(j,i))//","
        enddo
        write(obj%fh,'(A)',advance="yes")  str(valueVector(j,n))//"],"
        
    enddo
    write(obj%fh,'(A)',advance="no")  "    "//"["//str(valueVector(m,1))//","
    
    do i =2,n-1
        write(obj%fh,'(A)',advance="no")  " "//str(valueVector(m,i))//","    
    enddo
    write(obj%fh,'(A)',advance="no")  str(valueVector(m,n))//"]"
    call obj%write("],")
    
    
end subroutine
! #################################################################


! #################################################################
subroutine dumpIOJSON_Key_ArrayRe64(obj,key,valueVector)
    class(IO_),intent(inout) :: obj
    character(*),intent(in) :: key
    real(real64),intent(in) :: valueVector(:,:)
    integer(int32) :: i, n,j,m
    
    if( index(obj%filename,".json")==0 )then
        print *, "writeIOJSON_Key_Vector >> obj%filename should contain .json"
        return
    endif

    if(.not.obj%json_mode)then
        ! first time
        obj%json_mode = .true.
        call obj%write("{")
    endif

    n =size(valueVector,2)
    m = size(valueVector,1)
    
    call obj%write('    "'//key//'":[',advance=.false.)
    do j=1, m-1
        if(abs(valueVector(j,1))<1.0d0)then
            write(obj%fh,'(A)',advance="no")  "[0"//str(valueVector(j,1))//","
        else
            write(obj%fh,'(A)',advance="no")  "["//str(valueVector(j,1))//","
        endif

        do i =2,n-1
            if(abs(valueVector(j,i))<1.0d0)then
                write(obj%fh,'(A)',advance="no")  " 0"//str(valueVector(j,i))//","
            else
                write(obj%fh,'(A)',advance="no")  " "//str(valueVector(j,i))//","
            endif
        enddo
        if(abs(valueVector(j,n))<1.0d0)then
            write(obj%fh,'(A)',advance="yes")  " 0"//str(valueVector(j,n))//"],"
        else
            write(obj%fh,'(A)',advance="yes")  " "//str(valueVector(j,n))//"],"
        endif
    enddo
    if(abs(valueVector(m,1))<1.0d0)then
        write(obj%fh,'(A)',advance="no")  "    0"//"["//str(valueVector(m,1))//","
    else
        write(obj%fh,'(A)',advance="no")  "    "//"["//str(valueVector(m,1))//","
    endif
    do i =2,n-1
        if(abs(valueVector(m,i))<1.0d0)then
            write(obj%fh,'(A)',advance="no")  " 0"//str(valueVector(m,i))//","
        else
            write(obj%fh,'(A)',advance="no")  " "//str(valueVector(m,i))//","
        endif
    enddo
    if(abs(valueVector(m,n))<1.0d0)then
        write(obj%fh,'(A)',advance="no")  " 0"//str(valueVector(m,n))//"]"
    else
        write(obj%fh,'(A)',advance="no")  " "//str(valueVector(m,n))//"]"
    endif
    call obj%write("],")
    
end subroutine
! #################################################################


! #################################################################
subroutine dumpIOJSON_Key_value(obj,key,value)
    class(IO_),intent(inout) :: obj
    character(*),intent(in) :: key
    integer(int32),intent(in) :: value
    integer(int32) :: i, n
    
    if( index(obj%filename,".json")==0 )then
        print *, "writeIOJSON_Key_Vector >> obj%filename should contain .json"
        return
    endif

    if(.not.obj%json_mode)then
        ! first time
        obj%json_mode = .true.
        call obj%write("{")
    endif

    call obj%write('    "'//key//'":'//str(value)//",")
    
end subroutine
! #################################################################


! #################################################################
subroutine dumpIOJSON_Key_valueRe64(obj,key,value)
    class(IO_),intent(inout) :: obj
    character(*),intent(in) :: key
    real(real64),intent(in) :: value
    integer(int32) :: i, n
    
    if( index(obj%filename,".json")==0 )then
        print *, "writeIOJSON_Key_Vector >> obj%filename should contain .json"
        return
    endif

    if(.not.obj%json_mode)then
        ! first time
        obj%json_mode = .true.
        call obj%write("{")
    endif

    call obj%write('    "'//key//'":'//str(value)//",")
    
end subroutine
! #################################################################

! #################################################################
subroutine dumpIOJSON_Key_valueRe32(obj,key,value)
    class(IO_),intent(inout) :: obj
    character(*),intent(in) :: key
    real(real32),intent(in) :: value
    integer(int32) :: i, n
    
    if( index(obj%filename,".json")==0 )then
        print *, "writeIOJSON_Key_Vector >> obj%filename should contain .json"
        return
    endif

    if(.not.obj%json_mode)then
        ! first time
        obj%json_mode = .true.
        call obj%write("{")
    endif

    call obj%write('    "'//key//'":'//str(value)//",")
    
end subroutine
! #################################################################

! #################################################################
subroutine dumpIOJSON_Key_valueChar(obj,key,value)
    class(IO_),intent(inout) :: obj
    character(*),intent(in) :: key
    character(*),intent(in) :: value
    integer(int32) :: i, n
    
    if( index(obj%filename,".json")==0 )then
        print *, "writeIOJSON_Key_Vector >> obj%filename should contain .json"
        return
    endif

    if(.not.obj%json_mode)then
        ! first time
        obj%json_mode = .true.
        call obj%write("{")
    endif

    call obj%write('    "'//key//'":"'//trim(value)//'",')
    
end subroutine
! #################################################################

! #################################################################
pure function lowercaseChar(line) result(ret)
    character(*),intent(in) :: line
    character(:),allocatable :: ret
    integer(int32) :: i
    
    ret =line
    do i=1,len(line)
        if( "A" <= ret(i:i) .and. ret(i:i) <= "Z"  )then
            ret(i:i) = char(ichar(ret(i:i) )+ 32 )
        endif
    enddo
    
end function
! #################################################################

! #################################################################
pure function lowercaseString(line) result(ret)
    type(String_),intent(in) :: line
    character(:),allocatable :: ret
    integer(int32) :: i
    
    ret =line%all
    do i=1,len(line%all)
        if(ret(i:i) >= 'A' .and. ret(i:i) <='Z' )then
            ret(i:i) = char(ichar(ret(i:i) )+ 32 )
        endif
    enddo
    
end function
! #################################################################


! #################################################################
function CaesarCipherChar(line,fshift) result(ret)
    character(*),intent(in) :: line
    integer(int32),intent(in) :: fshift
    integer(int32) :: i
    character(:),allocatable :: ret

    ! 1-26 A-Z
    ! 27-32 [ \ ] ^ _ `
    ! 33-59 a-z 
    ! 60-64 | } ~  

    print *, "[Caution] bug exists"
    return
    !
    ret = line
    !do concurrent (i=1:len(line) )
    do i=1,len(line)
        print *,cyclic(ichar(line(i:i))+ fshift,max=64)
        print *, char(i=cyclic(ichar(line(i:i))+ fshift,max=64) )
        ret(i:i) = char(i=cyclic(ichar(line(i:i))+ fshift,max=64) )
    enddo

end function
! #################################################################


pure function cyclic(num,max) result(ret)
    integer(int32),intent(in)::num,max
    integer(int32) :: ret

    if(num>max)then
        ret = mod(num,max)
        if(ret==0)then
            ret = max
        endif
        return
    endif

    if(num<1)then
        ret = max + mod(num,max)
        return
    endif

    ret = num

end function
! ##########################################################

subroutine ruleIO(obj,header,offset,content_type)
    class(IO_),intent(inout) :: obj
    integer(int32),intent(in) :: header,offset,content_type(:)

    obj%activate_rule = .true.
    obj%header = header
    obj%offset = offset
    obj%content_type = content_type
    

end subroutine ruleIO
! ##########################################################


subroutine ResetRuleIO(obj,header,offset,content_type)
    class(IO_),intent(inout) :: obj
    integer(int32),intent(in) :: header,offset,content_type(:)

    obj%activate_rule = .false.
    obj%header = 0
    obj%offset = 0
    if(allocated(obj%content_type))then
        deallocate(obj%content_type)
    endif
    

end subroutine ResetRuleIO
! ##########################################################


! ##########################################################
!subroutine importIO(obj,path,realVector1,intArray) 
!
!end subroutine
! ##########################################################

end module IOClass