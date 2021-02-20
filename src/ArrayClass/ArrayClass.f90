module ArrayClass
    use, intrinsic :: iso_fortran_env
    use MathClass
    use RandomClass
    implicit none

    !interface arrayarray
    !    module procedure arrayarrayReal, arrayarrayInt
    !end interface arrayarray


    !interface newArray
    !    module procedure newArrayReal
    !end interface

    interface loadtxt
        module procedure loadtxtArrayReal
    end interface

    interface savetxt
        module procedure savetxtArrayReal,savetxtArrayint
    end interface

    interface add
        module procedure addArrayInt, addArrayReal,addArrayIntVec
    end interface add

    interface addArray
        module procedure addArrayInt, addArrayReal,addArrayIntVec
    end interface addArray

    interface Merge
        module procedure MergeArrayInt, MergeArrayReal
    end interface Merge


    interface CopyArray
        module procedure CopyArrayInt, CopyArrayReal,CopyArrayIntVec, CopyArrayRealVec,CopyArrayChar
    end interface CopyArray

    interface Copy
        module procedure CopyArrayInt, CopyArrayReal,CopyArrayIntVec, CopyArrayRealVec,CopyArrayChar
    end interface Copy


    interface Trim
        module procedure TrimArrayInt, TrimArrayReal
    end interface Trim

    interface TrimArray
        module procedure TrimArrayInt, TrimArrayReal
    end interface TrimArray



    interface open
        module procedure openArrayInt, openArrayReal, openArrayIntVec, openArrayRealVec,openArrayInt3, openArrayReal3
    end interface

    interface openArray
        module procedure openArrayInt, openArrayReal, openArrayIntVec, openArrayRealVec,openArrayInt3, openArrayReal3
    end interface
    interface load
        module procedure openArrayInt, openArrayReal, openArrayIntVec, openArrayRealVec,openArrayInt3, openArrayReal3
    end interface
    
    interface loadArray
        module procedure openArrayInt, openArrayReal, openArrayIntVec, openArrayRealVec,openArrayInt3, openArrayReal3
    end interface
    interface write
        module procedure writeArrayInt, writeArrayReal, writeArrayIntVec, writeArrayRealVec,writeArrayInt3, writeArrayReal3
    end interface
    
    interface writeArray
        module procedure writeArrayInt, writeArrayReal, writeArrayIntVec, writeArrayRealVec,writeArrayInt3, writeArrayReal3
    end interface
    interface save
        module procedure writeArrayInt, writeArrayReal, writeArrayIntVec, writeArrayRealVec,writeArrayInt3, writeArrayReal3
    end interface

    interface saveArray
        module procedure writeArrayInt, writeArrayReal, writeArrayIntVec, writeArrayRealVec,writeArrayInt3, writeArrayReal3
    end interface

    interface json
        module procedure jsonArrayReal, jsonArrayInt, jsonArrayRealVec, jsonArrayIntVec
    end interface


    interface Import
        module procedure ImportArrayInt, ImportArrayReal
    end interface Import
    
    interface ImportArray
        module procedure ImportArrayInt, ImportArrayReal
    end interface ImportArray

    interface Export
        module procedure ExportArrayInt, ExportArrayReal
    end interface Export

    interface ExportArray
        module procedure ExportArrayInt, ExportArrayReal
    end interface ExportArray


    interface ExportArraySize
        module procedure ExportArraySizeInt, ExportArraySizeReal
    end interface ExportArraySize

    interface ExportSize
        module procedure ExportArraySizeInt, ExportArraySizeReal
    end interface ExportSize

    interface InOrOut
            module procedure InOrOutReal,InOrOutInt
    end interface

    interface print
        module procedure ::  printArrayInt, printArrayReal,printArrayIntVec, printArrayRealVec
    end interface print
    
    interface ShowSize
        module procedure    ShowArraySizeInt, ShowArraySizeReal
        module procedure    ShowArraySizeIntvec, ShowArraySizeRealvec
        module procedure    ShowArraySizeIntThree, ShowArraySizeRealThree
    end interface ShowSize
    interface ShowArraySize
        module procedure    ShowArraySizeInt, ShowArraySizeReal
        module procedure    ShowArraySizeIntvec, ShowArraySizeRealvec
        module procedure    ShowArraySizeIntThree, ShowArraySizeRealThree
    end interface ShowArraySize

    interface Show
        module procedure ::  ShowArrayInt, ShowArrayReal,ShowArrayIntVec, ShowArrayRealVec
    end interface Show
    interface ShowArray
        module procedure ::  ShowArrayInt, ShowArrayReal,ShowArrayIntVec, ShowArrayRealVec
    end interface ShowArray


    interface Extend
        module procedure  :: ExtendArrayReal,ExtendArrayRealVec,ExtendArrayIntVec,ExtendArrayInt,ExtendArrayChar
    end interface Extend
    
    interface ExtendArray
        module procedure  :: ExtendArrayReal,ExtendArrayRealVec,ExtendArrayIntVec,ExtendArrayInt,ExtendArrayChar
    end interface ExtendArray

    interface insert
        module procedure :: insertArrayInt, insertArrayReal
    end interface insert
    
    interface insertArray
        module procedure :: insertArrayInt, insertArrayReal
    end interface insertArray

    interface remove
        module procedure :: removeArrayReal,removeArrayInt,removeArrayReal3rdOrder
    end interface remove

    interface removeArray
        module procedure :: removeArrayReal,removeArrayInt,removeArrayReal3rdOrder
    end interface removeArray

    interface mean
        module procedure :: meanVecReal,meanVecInt
    end interface mean

    interface distance
        module procedure ::distanceReal,distanceInt
    end interface


    interface countifSame
        module procedure ::countifSameIntArray,countifSameIntVec,countifSameIntArrayVec,countifSameIntVecArray
    end interface

    interface countif
        module procedure ::countifint,countifintvec,countifReal,countifRealvec,countiflogicVec
    end interface

    interface exist
        module procedure :: existIntVec, existIntArray
    end interface


    interface getif
        module procedure ::getifreal,getifrealvec!,getifint,getifintvec
    end interface

    interface quicksort
        module procedure :: quicksortreal,quicksortint
    end interface

    interface getKeyAndValue
        module procedure :: getKeyAndValueReal
    end interface
    
    interface addlist
        module procedure :: addListIntVec
    end interface

    interface Angles
        module procedure :: anglesReal3D
    end interface
    


    type :: Array_
        integer(int32),allocatable ,private :: inta(:,:)
        integer(real64),allocatable,private ::reala(:,:)
    contains
        procedure, public :: array => arrayarrayReal
    end type

contains
        

! ############## Elementary Entities ############## 
!=====================================
function loadtxtArrayInt(path,name,extention) result(intArray)
    integer(int32),allocatable :: intArray(:,:)
    character(*),intent(in) :: path,name,extention
    integer(int32) :: fh, n,m,x,i
    fh = 9
!    open(fh,file = trim(path)//trim(name)//trim(extention),status="old" )
!    do
!        read(fh, * , end=100 ) x
!        n=n+1
!    enddo
!100 continue
!    rewind(fh)
!    do
!        read(fh, '(i10)', advance='no',end=200 ) x
!        n=n+1
!    enddo
!200 continue
!    close(fh)
    
    open(fh,file = trim(path)//trim(name)//trim(extention),status="old" )
    read(fh,*) n,m
    allocate(intArray(n,m) )
    do i=1, n
        read(fh,*) intArray(i,1:m)
    enddo
    close(fh)

end function 
!=====================================

!=====================================
function loadtxtArrayReal(path,name,extention) result(realarray)
    real(real64),allocatable :: realarray(:,:)
    character(*),intent(in) :: path,name,extention
    integer(int32) :: fh, n,m,x,i
    fh = 9
!    open(fh,file = trim(path)//trim(name)//trim(extention),status="old" )
!    do
!        read(fh, * , end=100 ) x
!        n=n+1
!    enddo
!100 continue
!    rewind(fh)
!    do
!        read(fh, '(i10)', advance='no',end=200 ) x
!        n=n+1
!    enddo
!200 continue
!    close(fh)
    
    open(fh,file = trim(path)//trim(name)//trim(extention),status="old" )
    read(fh,*) n,m
    allocate(realArray(n,m) )
    do i=1, n
        read(fh,*) realArray(i,1:m)
    enddo
    close(fh)

end function 
!=====================================


subroutine savetxtArrayReal(realarray,path,name,extention) 
    real(real64),allocatable,intent(in) :: realarray(:,:)
    character(*),intent(in) :: path,name,extention
    integer(int32) :: fh, n,m,x,i,j
    fh = 9


    open(fh,file = trim(path)//trim(name)//trim(extention),status="replace" )
    n = size(realArray,1)
    m = size(realArray,2)

    if(.not.allocated(realarray))then
        n=0
        m=0
    endif

    if(trim(extention) == ".csv")then
        write(fh,*) n,",",m,","
        do i=1, n
            do j=1,m-1
                write(fh, '(A)',advance='no') trim(str(realArray(i,j)))//","
            enddo
            write(fh,'(A)',advance='yes') trim(str(realArray(i,m)))
        enddo
    elseif(trim(extention) == ".html")then
        write(fh,'(A)',advance='yes') "<!DOCTYPE html>"
        write(fh,'(A)',advance='yes') "<html>"
        write(fh,'(A)',advance='yes') "<head>"
        write(fh,'(A)',advance='no' ) "<title>"
        write(fh,'(A)',advance='no' ) "Saved by savetxt"
        write(fh,'(A)',advance='yes') "</title>"
        write(fh,'(A)',advance='yes') "</head>"
        write(fh,'(A)',advance='yes') "<body>"
        write(fh,'(A)',advance='yes')"<table border='5'>"
        write(fh,'(A)',advance='yes') '<meta http-equiv="refresh" content="3">'
        do i=1, n
            write(fh, '(A)',advance='yes') "<tr>"
            do j=1,m
                write(fh, '(A)',advance='no')  "<td><div contenteditable>"
                write(fh, '(A)',advance='no') trim(str(realArray(i,j)))
                write(fh, '(A)',advance='yes') "</td></div>"
            enddo
            write(fh, '(A)',advance='yes') "</tr>"
        enddo
        write(fh,'(A)',advance='yes') "</table>"
        write(fh,'(A)',advance='yes') "</body>"
        write(fh,'(A)',advance='yes') "</html>"
    else
        write(fh,*) n,m
        do i=1, n
            write(fh,*) realArray(i,1:m)
        enddo
    endif
    close(fh)
end subroutine
!=====================================


subroutine savetxtArrayint(realarray,path,name,extention) 
    integer(int32),allocatable,intent(in) :: realarray(:,:)
    character(*),intent(in) :: path,name,extention
    integer(int32) :: fh, n,m,x,i,j
    fh = 9


    open(fh,file = trim(path)//trim(name)//trim(extention),status="replace" )
    n = size(realArray,1)
    m = size(realArray,2)

    if(.not.allocated(realarray))then
        n=0
        m=0
    endif

    if(trim(extention) == ".csv")then
        write(fh,*) n,",",m,","
        do i=1, n
            do j=1,m-1
                write(fh, '(A)',advance='no') trim(str(realArray(i,j)))//","
            enddo
            write(fh,'(A)',advance='yes') trim(str(realArray(i,m)))
        enddo
    elseif(trim(extention) == ".html")then
        write(fh,'(A)',advance='yes') "<!DOCTYPE html>"
        write(fh,'(A)',advance='yes') "<html>"
        write(fh,'(A)',advance='yes') "<head>"
        write(fh,'(A)',advance='no' ) "<title>"
        write(fh,'(A)',advance='no' ) "Saved by savetxt"
        write(fh,'(A)',advance='yes') "</title>"
        write(fh,'(A)',advance='yes') "</head>"
        write(fh,'(A)',advance='yes') "<body>"
        write(fh,'(A)',advance='yes')"<table border='5'>"
        write(fh,'(A)',advance='yes') '<meta http-equiv="refresh" content="3">'
        do i=1, n
            write(fh, '(A)',advance='yes') "<tr>"
            do j=1,m
                write(fh, '(A)',advance='no')  "<td><div contenteditable>"
                write(fh, '(A)',advance='no') trim(str(realArray(i,j)))
                write(fh, '(A)',advance='yes') "</td></div>"
            enddo
            write(fh, '(A)',advance='yes') "</tr>"
        enddo
        write(fh,'(A)',advance='yes') "</table>"
        write(fh,'(A)',advance='yes') "</body>"
        write(fh,'(A)',advance='yes') "</html>"
    else
        write(fh,*) n,m
        do i=1, n
            write(fh,*) realArray(i,1:m)
        enddo
    endif
    close(fh)
end subroutine
!=====================================


subroutine arrayarrayReal(obj,reala)
    class(Array_),intent(inout) :: obj
    real(real64),intent(in) :: reala(:,:)
    integer(int32) :: n,m
    if(allocated(obj%reala))then
        deallocate(obj%reala)
    endif

    if(allocated(obj%inta))then
        deallocate(obj%inta)
    endif
    n = size(reala,1)
    m = size(reala,2)

    allocate(obj%reala(n,m) )
    allocate(obj%inta(n,m) )
    
    obj%reala(:,:) = reala(:,:)
    obj%inta(:,:) = int(reala(:,:))

end subroutine
!=====================================


!=====================================
subroutine arrayarrayint(obj,inta)
    class(Array_),intent(inout) :: obj
    integer(int32),intent(in) :: inta(:,:)
    integer(int32) :: n,m
    if(allocated(obj%inta))then
        deallocate(obj%inta)
    endif

    if(allocated(obj%reala))then
        deallocate(obj%reala)
    endif

    n = size(inta,1)
    m = size(inta,2)

    allocate(obj%inta(n,m) )
    allocate(obj%inta(n,m) )
    
    obj%inta(:,:) = inta(:,:)
    obj%reala(:,:) = dble(inta(:,:))

end subroutine
!=====================================



!=====================================
subroutine addArrayInt(a,b)
    integer(int32),allocatable,intent(inout)::a(:,:)
    integer(int32),intent(in)::b(:,:)
    integer(int32),allocatable::c(:,:)
    integer(int32) i,j,an,am,bn,bm

    if(allocated(c)) deallocate(c)
    an=size(a,1)
    am=size(a,2)

    bn=size(b,1)
    bm=size(b,2)

    if(am/=bm)then
        print *, "ERROR :: MergeArray, size(a,2)/= size(b,2)"
        return
    endif
    allocate(c(an+bn,am) )
    do i=1,an
        c(i,:)=a(i,:)
    enddo
    do i=1,bn
        c(i+an,:)=b(i,:)
    enddo
    
    call copyArray(c,a)

end subroutine
!=====================================



!=====================================
subroutine addArrayIntVec(a,b)
    integer(int32),allocatable,intent(inout)::a(:)
    integer(int32),intent(in)::b(:)
    integer(int32),allocatable::c(:)
    integer(int32) i,j,an,am,bn,bm

    if(allocated(c)) deallocate(c)
    an=size(a,1)

    bn=size(b,1)

    allocate(c(an+bn) )
    do i=1,an
        c(i)=a(i)
    enddo
    do i=1,bn
        c(i+an)=b(i)
    enddo
    
    call copyArray(c,a)

end subroutine
!=====================================

!=====================================
subroutine addArrayReal(a,b)
    real(real64),allocatable,intent(inout)::a(:,:)
    real(real64),intent(in)::b(:,:)
    real(real64),allocatable::c(:,:)
    integer(int32) i,j,an,am,bn,bm

    if(allocated(c)) deallocate(c)
    an=size(a,1)
    am=size(a,2)

    bn=size(b,1)
    bm=size(b,2)

    if(am/=bm)then
        print *, "ERROR :: MergeArray, size(a,2)/= size(b,2)"
        return
    endif
    allocate(c(an+bn,am) )
    do i=1,an
        c(i,:)=a(i,:)
    enddo
    do i=1,bn
        c(i+an,:)=b(i,:)
    enddo
    
    call copyArray(c,a)

end subroutine
!=====================================




!=====================================
subroutine MergeArrayInt(a,b,c)
    integer(int32),intent(in)::a(:,:)
    integer(int32),intent(in)::b(:,:)
    integer(int32),allocatable,intent(out)::c(:,:)
    integer(int32) i,j,an,am,bn,bm

    if(allocated(c)) deallocate(c)
    an=size(a,1)
    am=size(a,2)

    bn=size(b,1)
    bm=size(b,2)

    if(am/=bm)then
        print *, "ERROR :: MergeArray, size(a,2)/= size(b,2)"
        return
    endif
    allocate(c(an+bn,am) )
    do i=1,an
        c(i,:)=a(i,:)
    enddo
    do i=1,bn
        c(i+an,:)=b(i,:)
    enddo

end subroutine
!=====================================


!=====================================
subroutine MergeArrayReal(a,b,c)
    real(real64),intent(in)::a(:,:)
    real(real64),intent(in)::b(:,:)
    real(real64),allocatable,intent(out)::c(:,:)
    integer(int32) i,j,an,am,bn,bm
    if(allocated(c)) deallocate(c)
    an=size(a,1)
    am=size(a,2)

    bn=size(b,1)
    bm=size(b,2)

    if(am/=bm)then
        print *, "ERROR :: MergeArray, size(a,2)/= size(b,2)"
        return
    endif
    allocate(c(an+bn,am) )
    do i=1,an
        c(i,:)=a(i,:)
    enddo
    do i=1,bn
        c(i+an,:)=b(i,:)
    enddo

end subroutine
!=====================================




!=====================================
subroutine CopyArrayInt(a,ac,debug)
    integer(int32),allocatable,intent(in)::a(:,:)
    integer(int32),allocatable,intent(inout)::ac(:,:)
    integer(int32) i,j,n,m
    logical,optional,intent(in) :: debug

    if(.not.allocated(a) )then
        !print *, "CopyArray :: original array is not allocated"
        return
    endif
    n=size(a,1)
    m=size(a,2)
    if(allocated(ac) ) deallocate(ac)

    allocate(ac(n,m) )
    ac(:,:)=a(:,:)
    
end subroutine
!=====================================




!=====================================
subroutine CopyArrayChar(a,ac,debug)
    character(200),allocatable,intent(in)::a(:,:)
    character(200),allocatable,intent(inout)::ac(:,:)
    integer(int32) i,j,n,m
    logical,optional,intent(in) :: debug

    if(.not.allocated(a) )then
        !print *, "CopyArray :: original array is not allocated"
        return
    endif
    n=size(a,1)
    m=size(a,2)
    if(allocated(ac) ) deallocate(ac)

    allocate(ac(n,m) )
    ac(:,:)=a(:,:)
    
end subroutine
!=====================================

!=====================================
subroutine CopyArrayReal(a,ac)
    real(real64),allocatable,intent(in)::a(:,:)
    real(real64),allocatable,intent(inout)::ac(:,:)
    integer(int32) i,j,n,m

    if(.not.allocated(a) )then
        !print *, "CopyArray :: original array is not allocated"
        return
    endif
    n=size(a,1)
    m=size(a,2)

    if(allocated(ac) ) deallocate(ac)
    allocate(ac(n,m) )
    ac(:,:)=a(:,:)
    
end subroutine
!=====================================





!=====================================
subroutine CopyArrayIntVec(a,ac)
    integer(int32),allocatable,intent(in)::a(:)
    integer(int32),allocatable,intent(inout)::ac(:)
    integer(int32) i,j,n,m

    if(.not.allocated(a) )then 
        !print *, "CopyArray :: original array is not allocated"
        return
    endif
    n=size(a,1)
    if(allocated(ac) ) deallocate(ac)

    allocate(ac(n) )
    ac(:)=a(:)
    
end subroutine
!=====================================


!=====================================
subroutine CopyArrayRealVec(a,ac)
    real(real64),allocatable,intent(in)::a(:)
    real(real64),allocatable,intent(inout)::ac(:)
    integer(int32) i,j,n,m

    if(.not.allocated(a) )then
        
        !print *, "CopyArray :: original array is not allocated"
        return
    endif
    n=size(a,1)
    if(allocated(ac) ) deallocate(ac)

    allocate(ac(n) )
    ac(:)=a(:)
        
end subroutine
!=====================================




!=====================================
subroutine TrimArrayInt(a,k)
    integer(int32),allocatable,intent(inout)::a(:,:)
    integer(int32),intent(in)::k
    integer(int32),allocatable::ac(:,:)
    integer(int32) :: i,j,n,m

    n=size(a,1)
    m=size(a,2)
    allocate(ac(k,m ))

    do i=1,k
        ac(i,:) = a(i,:)
    enddo
    deallocate(a)
    allocate(a(k,m) )
    a(:,:)=ac(:,:)
    return

end subroutine
!=====================================


!=====================================
subroutine TrimArrayReal(a,k)
    real(real64),allocatable,intent(inout)::a(:,:)
    integer(int32),intent(in)::k
    real(real64),allocatable::ac(:,:)
    integer(int32) :: i,j,n,m

    n=size(a,1)
    m=size(a,2)
    allocate(ac(k,m ))


    do i=1,k
        ac(i,:) = a(i,:)
    enddo

    deallocate(a)
    allocate(a(k,m) )
    a(:,:)=ac(:,:)
    return

end subroutine
!=====================================


!=====================================
subroutine openArrayInt(fh, Array)
    integer(int32),intent(in) :: fh
    integer(int32),allocatable,intent(out)::Array(:,:)
    integer(int32) :: i,j,n,m
    read(fh,*) n
    read(fh,*) m
    if(n==0 .and. m==0)then
        return
    endif
    if(allocated(Array) ) deallocate(Array)
    allocate(Array(n,m) )
    do i=1,n
        read(fh,*) Array(i,1:m)
    enddo
end subroutine
!=====================================




!=====================================
subroutine openArrayInt3(fh, Array3)
    integer(int32),intent(in) :: fh
    integer(int32),allocatable,intent(out)::Array3(:,:,:)
    integer(int32) :: i,j,n,m,o
    read(fh,*) n
    read(fh,*) m
    read(fh,*) o
    if( abs(n)+abs(m)+abs(o)==0)then
        return
    endif

    if(allocated(Array3) ) deallocate(Array3)
    allocate(Array3(n,m,o) )
    do i=1,n
        do j=1, m
            read(fh,*) Array3(i,j,1:o)
        enddo
    enddo
end subroutine
!=====================================


!=====================================
subroutine openArrayReal3(fh, Array3)
    integer(int32),intent(in) :: fh
    real(real64),allocatable,intent(out)::Array3(:,:,:)
    integer(int32) :: i,j,n,m,o
    read(fh,*) n
    read(fh,*) m
    read(fh,*) o
    if( abs(n)+abs(m)+abs(o)==0)then
        return
    endif

    if(allocated(Array3) ) deallocate(Array3)
    allocate(Array3(n,m,o) )
    do i=1,n
        do j=1, m
            read(fh,*) Array3(i,j,1:o)
        enddo
    enddo
end subroutine
!====================================

!=====================================
subroutine openArrayReal(fh, Array)
    integer(int32),intent(in) :: fh
    real(real64),allocatable,intent(out)::Array(:,:)
    integer(int32) :: i,j,n,m
    read(fh,*) n
    read(fh,*) m
    if(n==0 .and. m==0)then
        return
    endif
    if(allocated(Array) ) deallocate(Array)
    allocate(Array(n,m) )
    do i=1,n
        read(fh,*) Array(i,1:m)
    enddo
end subroutine
!=====================================

!=====================================
subroutine openArrayIntVec(fh, Vector)
    integer(int32),intent(in) :: fh
    integer(int32),allocatable,intent(out)::Vector(:)
    integer(int32) :: i,j,n,m
    read(fh,*) n
    if(n==0 )then
        return
    endif
    if(allocated(Vector) ) deallocate(Vector)
    allocate(Vector(n) )
    do i=1,n
        read(fh,*) Vector(i)
    enddo
end subroutine
!=====================================


!=====================================
subroutine openArrayRealVec(fh, Vector)
    integer(int32),intent(in) :: fh
    real(real64),allocatable,intent(out)::Vector(:)
    integer(int32) :: i,j,n,m
    read(fh,*) n
    if(n==0 )then
        return
    endif
    if(allocated(Vector) ) deallocate(Vector)
    allocate(Vector(n) )
    do i=1,n
        read(fh,*) Vector(i)
    enddo
end subroutine
!=====================================





!=====================================
subroutine writeArrayInt(fh, Array)
    integer(int32),intent(in) :: fh
    integer(int32),allocatable,intent(in)::Array(:,:)
    integer(int32) :: i,j,n,m
    if(.not.allocated(Array) )then
        n=0
        m=0
    else
        n=size(Array,1)
        m=size(Array,2)
    endif

    write(fh,*) n
    write(fh,*) m
    do i=1,n
        write(fh,*) Array(i,1:m)
    enddo
end subroutine
!=====================================




!=====================================
subroutine writeArrayInt3(fh, Array3)
    integer(int32),intent(in) :: fh
    integer(int32),allocatable,intent(in)::Array3(:,:,:)
    integer(int32) :: i,j,n,m,o
    if(.not.allocated(Array3) )then
        n=0
        m=0
        o=0
    else
        n=size(Array3,1)
        m=size(Array3,2)
        o=size(Array3,3)
    endif
    write(fh,*) n
    write(fh,*) m
    write(fh,*) o
    do i=1,n
        do j=1,m
            write(fh,*) Array3(i,j,1:o)
        enddo
    enddo
end subroutine
!=====================================



!=====================================
subroutine writeArrayReal3(fh, Array3)
    integer(int32),intent(in) :: fh
    real(real64),allocatable,intent(in)::Array3(:,:,:)
    integer(int32) :: i,j,n,m,o
    if(.not.allocated(Array3) )then
        n=0
        m=0
        o=0
    else
        n=size(Array3,1)
        m=size(Array3,2)
        o=size(Array3,3)
    endif
    write(fh,*) n
    write(fh,*) m
    write(fh,*) o
    do i=1,n
        do j=1,m
            write(fh,*) Array3(i,j,1:o)
        enddo
    enddo
end subroutine
!=====================================
!=====================================
subroutine writeArrayReal(fh, Array)
    integer(int32),intent(in) :: fh
    real(real64),allocatable,intent(in)::Array(:,:)
    integer(int32) :: i,j,n,m
    if(.not.allocated(Array) )then
        n=0
        m=0
    else
        n=size(Array,1)
        m=size(Array,2)
    endif
    write(fh,*) n
    write(fh,*) m
    do i=1,n
        write(fh,*) Array(i,1:m)
    enddo
end subroutine
!=====================================

!=====================================
subroutine writeArrayIntVec(fh, Vector)
    integer(int32),intent(in) :: fh
    integer(int32),allocatable,intent(in)::Vector(:)
    integer(int32) :: i,j,n,m
    if(.not.allocated(Vector) )then
        n=0
        m=0
    else
        n=size(Vector)
    endif
    write(fh,*) n
    do i=1,n
        write(fh,*) Vector(i)
    enddo
end subroutine
!=====================================


!=====================================
subroutine writeArrayRealVec(fh, Vector)
    integer(int32),intent(in) :: fh
    real(real64),allocatable,intent(in)::Vector(:)
    integer(int32) :: i,j,n,m
    if(.not.allocated(Vector) )then
        n=0
        m=0
    else
        n=size(Vector)
    endif
    write(fh,*) n
    do i=1,n
        write(fh,*) Vector(i)
    enddo
end subroutine
!=====================================






!##################################################
subroutine ImportArrayInt(Mat,OptionalFileHandle,OptionalSizeX,OptionalSizeY,FileName)
    integer(int32),allocatable,intent(inout)::Mat(:,:)
    integer(int32),optional,intent(in)::OptionalFileHandle,OptionalSizeX,OptionalSizeY
    character(*) ,optional,intent(in) :: FileName
    integer(int32) i,j,n,m,fh

    if(present(FileName) )then
        if(allocated(Mat))then
            deallocate(Mat)
        endif
        fh=input(default=10,option=OptionalFileHandle)
        open(fh,file=FileName)
        read(fh,*) i,j
        allocate(Mat(i,j) )
        do i=1,size(Mat,1)
            read(fh,*) Mat(i,:)
        enddo
        return
    endif

    if(present(OptionalSizeX) )then
        n=OptionalSizeX
    elseif(allocated(Mat) )then
        n=size(Mat,1)
    else
        n=1
        print *, "Caution :: ArrayClass/ImportArray >> No size_X is set"
    endif


    if(present(OptionalSizeY) )then
        m=OptionalSizeY
    elseif(allocated(Mat) )then
        m=size(Mat,2)
    else
        m=1
        print *, "Caution :: ArrayClass/ImportArray >> No size_Y is set"
    endif


    if(present(OptionalFileHandle) )then
        fh=OptionalFileHandle
    else
        fh=10
    endif

    if(.not.allocated(Mat))then
        allocate(Mat(n,m) )
    endif

    if(size(Mat,1)/=n .or. size(Mat,2)/=m  )then
        deallocate(Mat)
        allocate(Mat(n,m) )
    endif

    do i=1,size(Mat,1)
        read(fh,*) Mat(i,:)
    enddo

    !include "./ImportArray.f90"

end subroutine ImportArrayInt
!##################################################


!##################################################
subroutine ImportArrayReal(Mat,OptionalFileHandle,OptionalSizeX,OptionalSizeY,FileName)
    real(real64),allocatable,intent(inout)::Mat(:,:)
    integer(int32),optional,intent(in)::OptionalFileHandle,OptionalSizeX,OptionalSizeY
    
    !include "./ImportArray.f90"
    character(*) ,optional,intent(in) :: FileName
    integer(int32) i,j,n,m,fh

    if(present(FileName) )then
        if(allocated(Mat))then
            deallocate(Mat)
        endif
        fh=input(default=10,option=OptionalFileHandle)
        open(fh,file=FileName)
        read(fh,*) i,j
        allocate(Mat(i,j) )
        do i=1,size(Mat,1)
            read(fh,*) Mat(i,:)
        enddo
        return
    endif
    if(present(OptionalSizeX) )then
        n=OptionalSizeX
    elseif(allocated(Mat) )then
        n=size(Mat,1)
    else
        n=1
        print *, "Caution :: ArrayClass/ImportArray >> No size_X is set"
    endif


    if(present(OptionalSizeY) )then
        m=OptionalSizeY
    elseif(allocated(Mat) )then
        m=size(Mat,2)
    else
        m=1
        print *, "Caution :: ArrayClass/ImportArray >> No size_Y is set"
    endif


    if(present(OptionalFileHandle) )then
        fh=OptionalFileHandle
    else
        fh=10
    endif

    if(.not.allocated(Mat))then
        allocate(Mat(n,m) )
    endif

    if(size(Mat,1)/=n .or. size(Mat,2)/=m  )then
        deallocate(Mat)
        allocate(Mat(n,m) )
    endif

    do i=1,size(Mat,1)
        read(fh,*) Mat(i,:)
    enddo


end subroutine ImportArrayReal
!##################################################



!##################################################
subroutine ExportArraySizeInt(Mat,RankNum,OptionalFileHandle)
    integer(int32),intent(in)::Mat(:,:)
    integer(int32),optional,intent(in)::OptionalFileHandle
    integer(int32),intent(in)::RankNum
    
    !#include "./ExportArraySize.f90"
    integer(int32) :: fh
    if(present(OptionalFileHandle) )then
        fh=OptionalFileHandle
    endif

    write(fh,*) size(Mat,RankNum)

end subroutine ExportArraySizeInt
!##################################################

!##################################################
subroutine ExportArraySizeReal(Mat,RankNum,OptionalFileHandle)
    real(real64),intent(in)::Mat(:,:)
    integer(int32),optional,intent(in)::OptionalFileHandle
    integer(int32),intent(in)::RankNum
    
    !#include "./ExportArraySize.f90"
    integer(int32) :: fh
    if(present(OptionalFileHandle) )then
        fh=OptionalFileHandle
    endif

    write(fh,*) size(Mat,RankNum)

end subroutine ExportArraySizeReal
!##################################################

!##################################################
subroutine ExportArrayInt(Mat,OptionalFileHandle)
    integer(int32),intent(in)::Mat(:,:)
    integer(int32),optional,intent(in)::OptionalFileHandle
    
    !#include "./ExportArray.f90"
    integer(int32) :: fh,i

    if(present(OptionalFileHandle) )then
        fh=OptionalFileHandle
    else
        fh=10
    endif

    do i=1,size(Mat,1)
        write(fh,*) Mat(i,:)
    enddo


end subroutine ExportArrayInt
!##################################################


!##################################################
subroutine ExportArrayReal(Mat,OptionalFileHandle)
    real(real64),intent(in)::Mat(:,:)
    integer(int32),optional,intent(in)::OptionalFileHandle
    
    !#include "./ExportArray.f90"
    integer(int32) :: fh,i

    if(present(OptionalFileHandle) )then
        fh=OptionalFileHandle
    else
        fh=10
    endif
    
    do i=1,size(Mat,1)
        write(fh,*) Mat(i,:)
    enddo


end subroutine ExportArrayReal
!##################################################



!##################################################
subroutine ShowArrayInt(Mat,IndexArray,FileHandle,Name,Add)
    integer(int32),intent(in)::Mat(:,:)
    integer(int32),optional,intent(in) :: IndexArray(:,:)
    integer(int32),optional,intent(in)::FileHandle ,Add
    character(*),optional,intent(in)::Name
    integer(int32) :: fh,i,j,k,l,nn

    nn=input(default=0,option=Add)
    if(present(FileHandle) )then
        fh=FileHandle
    else
        fh=10
    endif

    if(present(Name) )then
        open(fh,file=Name)
    endif
    
    if(present(IndexArray))then
        
        do i=1,size(IndexArray,1)
            do j=1,size(IndexArray,2)
                k = IndexArray(i,j)
                if(k <= 0)then
                    cycle
                endif
                
                
                if(present(FileHandle) .or. present(Name) )then
                    do l=1,size(Mat,2)-1
                        write(fh,'(i0)',advance='no') Mat(k,l)+nn
                        write(fh,'(A)',advance='no') "     "
                    enddo
                    write(fh,'(i0)',advance='yes') Mat(k,size(Mat,2) )+nn
                else
                    print *, Mat(k,:)
                endif
            enddo
        enddo
    else

        do j=1,size(Mat,1)
            
            if(present(FileHandle) .or. present(Name) )then
                !write(fh,*) Mat(j,:)
                do k=1,size(Mat,2)-1
                    write(fh,'(i0)',advance='no') Mat(j,k)+nn
                    write(fh,'(A)',advance='no') "     "
                enddo
                write(fh,'(i0)',advance='yes') Mat(j,size(Mat,2) )+nn
            else
                print *, Mat(j,:)
            endif

        enddo
        
    endif
    
    if(present(FileHandle) .or. present(Name) )then
        flush(fh)
    endif


    if(present(Name) )then
        close(fh)
    endif


end subroutine 
!##################################################




!##################################################
subroutine ShowArrayIntVec(Mat,IndexArray,FileHandle,Name,Add)
    integer(int32),intent(in)::Mat(:)
    integer(int32),optional,intent(in) :: IndexArray(:,:)
    integer(int32),optional,intent(in)::FileHandle ,Add
    character(*),optional,intent(in)::Name
    integer(int32) :: fh,i,j,k,l,nn

    nn=input(default=0,option=Add)
    if(present(FileHandle) )then
        fh=FileHandle
    else
        fh=10
    endif

    if(present(Name) )then
        open(fh,file=Name)
    endif
    
    if(present(IndexArray))then
        
        do i=1,size(IndexArray,1)
            do j=1,size(IndexArray,2)
                k = IndexArray(i,j)
                if(k <= 0)then
                    cycle
                endif
                
                
                if(present(FileHandle) .or. present(Name) )then
                    write(fh,'(i0)') Mat(k)+nn
                else
                    print *, Mat(k)
                endif
            enddo
        enddo
    else

        do j=1,size(Mat,1)
            
            if(present(FileHandle) .or. present(Name) )then
                !write(fh,*) Mat(j,:)
                write(fh,'(i0)') Mat(j)+nn
            else
                print *, Mat(j)
            endif

        enddo
        
    endif
    
    if(present(FileHandle) .or. present(Name) )then
        flush(fh)
    endif


    if(present(Name) )then
        close(fh)
    endif


end subroutine 
!##################################################


!##################################################
subroutine ShowArrayReal(Mat,IndexArray,FileHandle,Name,Add)
    real(real64),intent(in)::Mat(:,:)
    real(real64),optional,intent(in) :: Add
    integer(int32),optional,intent(in) :: IndexArray(:,:)
    integer(int32),optional,intent(in)::FileHandle
    character(*),optional,intent(in)::Name
    real(real64) :: nn
    integer(int32) :: fh,i,j,k,l

    nn=input(default=0.0d0,option=Add)
    if(present(FileHandle) )then
        fh=FileHandle
    else
        fh=10
    endif

    if(present(Name) )then
        open(fh,file=Name)
    endif

    if(present(IndexArray))then
        
        do i=1,size(IndexArray,1)
            do j=1,size(IndexArray,2)
                k = IndexArray(i,j)
                if(k <= 0)then
                    cycle
                endif
                
                
                if(present(FileHandle) .or. present(Name) )then
                    !write(fh,*) Mat(k,:)
                    do l=1,size(Mat,2)-1
                        write(fh, '(e22.14e3)',advance='no' ) Mat( k,l )+nn
                        write(fh,'(A)',advance='no') "     "
                    enddo
                    write(fh,'(e22.14e3)',advance='yes') Mat( k,size(Mat,2) )+nn
                else
                    print *, Mat(k,:)
                endif
            enddo
        enddo
    else

        do j=1,size(Mat,1)
            
            if(present(FileHandle) .or. present(Name) )then
                !write(fh,*) Mat(j,:)
                do l=1,size(Mat,2)-1
                    write(fh,'(e22.14e3)',advance='no') Mat( j,l )+nn
                    write(fh,'(A)',advance='no') "     "
                enddo
                write(fh,'(e22.14e3)',advance='yes') Mat( j,size(Mat,2) )+nn
            else
                print *, Mat(j,:)
            endif

        enddo
        
    endif
    
    if(present(FileHandle) .or. present(Name) )then
        flush(fh)
    endif



    if(present(Name) )then
        close(fh)
    endif

end subroutine 
!##################################################




!##################################################
subroutine ShowArrayRealVec(Mat,IndexArray,FileHandle,Name,Add)
    real(real64),intent(in)::Mat(:)
    integer(int32),optional,intent(in) :: IndexArray(:,:)
    integer(int32),optional,intent(in)::FileHandle ,Add
    character(*),optional,intent(in)::Name
    integer(int32) :: fh,i,j,k,l,nn

    nn=input(default=0,option=Add)
    if(present(FileHandle) )then
        fh=FileHandle
    else
        fh=10
    endif

    if(present(Name) )then
        open(fh,file=Name)
    endif
    
    if(present(IndexArray))then
        
        do i=1,size(IndexArray,1)
            do j=1,size(IndexArray,2)
                k = IndexArray(i,j)
                if(k <= 0)then
                    cycle
                endif
                
                
                if(present(FileHandle) .or. present(Name) )then
                    write(fh,'(i0)') Mat(k)+nn
                else
                    print *, Mat(k)
                endif
            enddo
        enddo
    else

        do j=1,size(Mat,1)
            
            if(present(FileHandle) .or. present(Name) )then
                !write(fh,*) Mat(j,:)
                write(fh,'(i0)') Mat(j)+nn
            else
                print *, Mat(j)
            endif

        enddo
        
    endif
    
    if(present(FileHandle) .or. present(Name) )then
        flush(fh)
    endif


    if(present(Name) )then
        close(fh)
    endif


end subroutine 
!##################################################



!##################################################
subroutine ShowArraySizeInt(Mat,OptionalFileHandle,Name)
    integer(int32),allocatable,intent(in)::Mat(:,:)
    integer(int32),optional,intent(in)::OptionalFileHandle
    character(*),optional,intent(in)::Name
    
    !#include "./ExportArray.f90"
    integer(int32) :: fh,i


    if(present(OptionalFileHandle) )then
        fh=OptionalFileHandle
    else
        fh=10
    endif


    if(present(Name) )then
        open(fh,file=Name)
    endif
    
    if(.not. allocated(Mat) )then
        print *, "Not allocated!"
        if(present(OptionalFileHandle) )then
            write(fh,*) "Not allocated!"
        endif
    endif
    print *, size(Mat,1), size(Mat,2) 
    if(present(OptionalFileHandle) .or. present(Name) )then
        write(fh,*) size(Mat,1), size(Mat,2)
    endif
    


    if(present(Name) )then
        close(fh)
    endif


end subroutine 
!##################################################


!##################################################
subroutine ShowArraySizeReal(Mat,OptionalFileHandle,Name)
    real(real64),allocatable,intent(in)::Mat(:,:)
    integer(int32),optional,intent(in)::OptionalFileHandle
    character(*),optional,intent(in)::Name
    
    !#include "./ExportArray.f90"
    integer(int32) :: fh,i

    if(present(OptionalFileHandle) )then
        fh=OptionalFileHandle
    else
        fh=10
    endif
    

    if(present(Name) )then
        open(fh,file=Name)
    endif
    
    if(.not. allocated(Mat) )then
        print *, "Not allocated!"
        if(present(OptionalFileHandle) )then
            write(fh,*) "Not allocated!"
        endif
    endif
    print *, size(Mat,1), size(Mat,2) 
    if(present(OptionalFileHandle).or. present(Name) )then
        write(fh,*) size(Mat,1), size(Mat,2)
    endif
    

    if(present(Name) )then
        close(fh)
    endif

end subroutine 
!##################################################



!##################################################
subroutine ShowArraySizeIntvec(Mat,OptionalFileHandle,Name)
    integer(int32),allocatable,intent(in)::Mat(:)
    integer(int32),optional,intent(in)::OptionalFileHandle
    character(*),optional,intent(in)::Name
    
    !#include "./ExportArray.f90"
    integer(int32) :: fh,i


    if(present(OptionalFileHandle) )then
        fh=OptionalFileHandle
    else
        fh=10
    endif
    

    if(present(Name) )then
        open(fh,file=Name)
    endif
    
    if(.not. allocated(Mat) )then
        print *, "Not allocated!"
        if(present(OptionalFileHandle).or. present(Name) )then
            write(fh,*) "Not allocated!"
        endif
    endif
    print *, size(Mat,1) 
    if(present(OptionalFileHandle).or. present(Name) )then
        write(fh,*) size(Mat,1)
    endif
    

    if(present(Name) )then
        close(fh)
    endif


end subroutine 
!##################################################


!##################################################
subroutine ShowArraySizeRealvec(Mat,OptionalFileHandle,Name)
    real(real64),allocatable,intent(in)::Mat(:)
    integer(int32),optional,intent(in)::OptionalFileHandle
    character(*),optional,intent(in)::Name
    
    !#include "./ExportArray.f90"
    integer(int32) :: fh,i

    if(present(OptionalFileHandle) )then
        fh=OptionalFileHandle
    else
        fh=10
    endif

    if(present(Name) )then
        open(fh,file=Name)
    endif
    
    if(.not. allocated(Mat) )then
        print *, "Not allocated!"
        if(present(OptionalFileHandle).or. present(Name) )then
            write(fh,*) "Not allocated!"
        endif
    endif
    print *, size(Mat,1)
    if(present(OptionalFileHandle) .or. present(Name))then
        write(fh,*) size(Mat,1)
    endif
    

    if(present(Name) )then
        close(fh)
    endif

end subroutine 
!##################################################



!##################################################
subroutine ShowArraySizeIntThree(Mat,OptionalFileHandle,Name)
    integer(int32),allocatable,intent(in)::Mat(:,:,:)
    integer(int32),optional,intent(in)::OptionalFileHandle
    character(*),optional,intent(in)::Name
    
    !#include "./ExportArray.f90"
    integer(int32) :: fh,i



    if(present(OptionalFileHandle) )then
        fh=OptionalFileHandle
    else
        fh=10
    endif

    if(present(Name) )then
        open(fh,file=Name)
    endif
    
    if(.not. allocated(Mat) )then
        print *, "Not allocated!"
        if(present(OptionalFileHandle) .or. present(Name))then
            write(fh,*) "Not allocated!"
        endif
    endif
    print *, size(Mat,1), size(Mat,2) , size(Mat,3) 
    if(present(OptionalFileHandle) .or. present(Name))then
        write(fh,*) size(Mat,1), size(Mat,2), size(Mat,3) 
    endif



    if(present(Name) )then
        close(fh)
    endif
end subroutine 
!##################################################


!##################################################
subroutine ShowArraySizeRealThree(Mat,OptionalFileHandle,Name)
    real(real64),allocatable,intent(in)::Mat(:,:,:,:)
    integer(int32),optional,intent(in)::OptionalFileHandle
    character(*),optional,intent(in)::Name
    
    !#include "./ExportArray.f90"
    integer(int32) :: fh,i

    if(present(OptionalFileHandle) )then
        fh=OptionalFileHandle
    else
        fh=10
    endif

    if(present(Name) )then
        open(fh,file=Name)
    endif
    
    if(.not. allocated(Mat) )then
        print *, "Not allocated!"
        if(present(OptionalFileHandle) .or. present(Name))then
            write(fh,*) "Not allocated!"
        endif
    endif
    print *, size(Mat,1), size(Mat,2) , size(Mat,3) 
    if(present(OptionalFileHandle).or. present(Name) )then
        write(fh,*) size(Mat,1), size(Mat,2), size(Mat,3) 
    endif
    


    if(present(Name) )then
        close(fh)
    endif

end subroutine 
!##################################################



!##################################################
function InOrOutReal(x,xmax,xmin,DimNum) result(Inside)
    real(real64),intent(in)::x(:)
    real(real64),intent(in)::xmax(:),xmin(:)
    integer(int32),optional,intent(in)::DimNum
    integer(int32) :: dim_num
    logical ::Inside
    integer(int32) :: i,j,n,cout

    cout=0
    if(present(DimNum) )then
        dim_num=DimNum
    else
        dim_num=size(x)
    endif

    do i=1,dim_num
        if(xmin(i) <= x(i) .and. x(i)<=xmax(i) )then
            cout=cout+1
        else
            cycle
        endif
    enddo

    if(cout==dim_num)then
        Inside = .true.
    else
        Inside = .false.
    endif

end function

!##################################################




!##################################################
function InOrOutInt(x,xmax,xmin,DimNum) result(Inside)
    integer(int32),intent(in)::x(:)
    integer(int32),intent(in)::xmax(:),xmin(:)
    integer(int32),optional,intent(in)::DimNum
    integer(int32) :: dim_num
    logical ::Inside
    integer(int32) :: i,j,n,cout

    cout=0
    if(present(DimNum) )then
        dim_num=DimNum
    else
        dim_num=size(x)
    endif

    do i=1,dim_num
        if(xmin(i) <= x(i) .and. x(i)<=xmax(i) )then
            cout=cout+1
        else
            cycle
        endif
    enddo

    if(cout==dim_num)then
        Inside = .true.
    else
        Inside = .false.
    endif

end function

!##################################################




!##################################################
subroutine ExtendArrayReal(mat,extend1stColumn,extend2ndColumn,DefaultValue)
    real(real64),allocatable,intent(inout)::mat(:,:)
    real(real64),allocatable :: buffer(:,:)
    logical,optional,intent(in) :: extend1stColumn,extend2ndColumn
    real(real64),optional,intent(in) :: DefaultValue
    real(real64) :: val
    integer(int32) :: n,m

    if( present(DefaultValue) )then
        val = DefaultValue
    else
        val = 0.0d0
    endif

    n=size(mat,1)
    m=size(mat,2)
    if(present(extend1stColumn) )then
        if(extend1stColumn .eqv. .true.)then
            allocate(buffer(n+1, m ) )
            buffer(:,:)=val
            buffer(1:n,1:m)=mat(1:n,1:m)
            deallocate(mat)
            call copyArray(buffer,mat)
            deallocate(buffer)
        endif
    endif

    n=size(mat,1)
    m=size(mat,2)
    if(present(extend2ndColumn) )then
        if(extend2ndColumn .eqv. .true.)then
            allocate(buffer(n, m+1 ) )
            buffer(:,:)=val
            buffer(1:n,1:m)=mat(1:n,1:m)
            deallocate(mat)
            call copyArray(buffer,mat)
            deallocate(buffer)
        endif
    endif


end subroutine
!##################################################


!##################################################
subroutine ExtendArrayRealVec(mat,DefaultValue,number)
    real(real64),allocatable,intent(inout)::mat(:)
    real(real64),allocatable :: buffer(:)
    integer,optional,intent(in) :: number
    real(real64),optional,intent(in) :: DefaultValue
    real(real64) :: val
    integer(int32) :: n,m,extn

    if( present(DefaultValue) )then
        val = DefaultValue
    else
        val = 0.0d0
    endif

    extn=input(default=1,option=number)
    n=size(mat,1)
    allocate(buffer(n+extn) )
    buffer(:)=val
    buffer(1:n)=mat(1:n)
    deallocate(mat)
    call copyArray(buffer,mat)
    deallocate(buffer)
        
end subroutine
!##################################################



!##################################################
subroutine ExtendArrayIntVec(mat,DefaultValue,number)
    integer(int32),allocatable,intent(inout)::mat(:)
    integer(int32),allocatable :: buffer(:)
    integer(int32),optional,intent(in) :: number
    integer(int32),optional,intent(in) :: DefaultValue
    integer(int32) :: val
    integer(int32) :: n,m,extn

    if( present(DefaultValue) )then
        val = DefaultValue
    else
        val = 0.0d0
    endif

    extn=input(default=1,option=number)
    n=size(mat,1)
    allocate(buffer(n+extn) )
    buffer(:)=val
    buffer(1:n)=mat(1:n)
    deallocate(mat)
    call copyArray(buffer,mat)
    deallocate(buffer)
        
end subroutine
!##################################################



!##################################################
subroutine ExtendArrayInt(mat,extend1stColumn,extend2ndColumn,DefaultValue)
    integer(int32),allocatable,intent(inout)::mat(:,:)
    integer(int32),allocatable :: buffer(:,:)
    logical,optional,intent(in) :: extend1stColumn,extend2ndColumn
    integer(int32),optional,intent(in) :: DefaultValue
    integer(int32) :: val
    integer(int32) :: i,j,k,n,m

    if( present(DefaultValue) )then
        val = DefaultValue
    else
        val = 0
    endif

    if(.not.allocated(mat) )then
        allocate(mat(1,1)    )
        mat(1,1)=val
        return
    endif

    n=size(mat,1)
    m=size(mat,2)
    if(present(extend1stColumn) )then
        if(extend1stColumn .eqv. .true.)then
            allocate(buffer(n+1, m ) )
            buffer(:,:)=val
            buffer(1:n,1:m)=mat(1:n,1:m)
            deallocate(mat)
            call copyArray(buffer,mat)
            deallocate(buffer)
        endif
    endif

    n=size(mat,1)
    m=size(mat,2)
    if(present(extend2ndColumn) )then
        if(extend2ndColumn .eqv. .true.)then
            allocate(buffer(n, m+1 ) )
            buffer(:,:)=val
            buffer(1:n,1:m)=mat(1:n,1:m)
            deallocate(mat)
            call copyArray(buffer,mat)
            deallocate(buffer)
        endif
    endif


end subroutine
!##################################################



!##################################################
subroutine ExtendArrayChar(mat,extend1stColumn,extend2ndColumn,DefaultValue)
    character(200),allocatable,intent(inout)::mat(:,:)
    character(200),allocatable :: buffer(:,:)
    logical,optional,intent(in) :: extend1stColumn,extend2ndColumn
    character(200),optional,intent(in) :: DefaultValue
    character(200) :: val
    integer(int32) :: i,j,k,n,m

    if( present(DefaultValue) )then
        val = DefaultValue
    else
        val = ""
    endif

    n=size(mat,1)
    m=size(mat,2)
    if(present(extend1stColumn) )then
        if(extend1stColumn .eqv. .true.)then
            allocate(buffer(n+1, m ) )
            buffer(:,:)=val
            buffer(1:n,1:m)(1:200)=mat(1:n,1:m)(1:200)
            deallocate(mat)
            call copyArray(buffer,mat)
            deallocate(buffer)
        endif
    endif

    n=size(mat,1)
    m=size(mat,2)
    if(present(extend2ndColumn) )then
        if(extend2ndColumn .eqv. .true.)then
            allocate(buffer(n, m+1 ) )
            buffer(:,:)=val
            buffer(1:n,1:m)(1:200)=mat(1:n,1:m)(1:200)
            deallocate(mat)
            call copyArray(buffer,mat)
            deallocate(buffer)
        endif
    endif


end subroutine
!##################################################


!##################################################
subroutine insertArrayInt(mat,insert1stColumn,insert2ndColumn,DefaultValue,NextOf)
    integer(int32),allocatable,intent(inout)::mat(:,:)
    integer(int32),allocatable :: buffer(:,:)
    logical,optional,intent(in) :: insert1stColumn,insert2ndColumn
    integer(int32),optional,intent(in) :: DefaultValue,NextOf
    integer(int32) :: val
    integer(int32) :: i,nof
    
    call extendArray(mat,insert1stColumn,insert2ndColumn,DefaultValue)

    if(present(DefaultValue))then
        val =DefaultValue
    else
        val =0
    endif

    if(present(NextOf))then
        nof=NextOf
    else 
        if( present(insert1stColumn ))then
            if( insert1stColumn .eqv. .true. )then
                nof=size(mat,1)-1
            endif
        endif
        
        if( present(insert2ndColumn ))then
            if( insert2ndColumn .eqv. .true. )then
                nof=size(mat,2)-1
            endif
        endif

    endif
    
    if( present(insert1stColumn ))then
        if( insert1stColumn .eqv. .true. )then
            do i=size(mat,1)-1,nof,-1
                mat(i+1,:)=mat(i,:)
            enddo
            mat(nof+1,:)=val
        endif
    endif
    if( present(insert2ndColumn ))then
        if( insert2ndColumn .eqv. .true. )then
            do i=size(mat,1)-1,nof,-1
                mat(:,i+1)=mat(:,i)
            enddo
            mat(:,nof+1)=val
        endif
    endif
end subroutine
!##################################################



!##################################################
subroutine insertArrayReal(mat,insert1stColumn,insert2ndColumn,DefaultValue,NextOf)
    real(real64),allocatable,intent(inout)::mat(:,:)
    real(real64),allocatable :: buffer(:,:)
    logical,optional,intent(in) :: insert1stColumn,insert2ndColumn
    
    integer(int32),optional,intent(in) :: NextOf
    real(real64),optional,intent(in) :: DefaultValue
    real(real64) :: val
    integer(int32) :: i,nof
    
    call extendArray(mat,insert1stColumn,insert2ndColumn,DefaultValue)

    if(present(DefaultValue))then
        val =DefaultValue
    else
        val =0
    endif

    if(present(NextOf))then
        nof=NextOf
    else 
        if( present(insert1stColumn ))then
            if( insert1stColumn .eqv. .true. )then
                nof=size(mat,1)-1
            endif
        endif
        
        if( present(insert2ndColumn ))then
            if( insert2ndColumn .eqv. .true. )then
                nof=size(mat,2)-1
            endif
        endif

    endif
    
    if( present(insert1stColumn ))then
        if( insert1stColumn .eqv. .true. )then
            do i=size(mat,1)-1,nof,-1
                mat(i+1,:)=mat(i,:)
            enddo
            mat(nof+1,:)=val
        endif
    endif
    if( present(insert2ndColumn ))then
        if( insert2ndColumn .eqv. .true. )then
            do i=size(mat,1)-1,nof,-1
                mat(:,i+1)=mat(:,i)
            enddo
            mat(:,nof+1)=val
        endif
    endif
end subroutine
!##################################################





!##################################################
subroutine removeArrayInt(mat,remove1stColumn,remove2ndColumn,NextOf)
    integer(int32),allocatable,intent(inout)::mat(:,:)
    integer(int32),allocatable :: buffer(:,:)
    logical,optional,intent(in) :: remove1stColumn,remove2ndColumn
    
    integer(int32),optional,intent(in) :: NextOf
    
    integer(int32) :: i,nof
    
    if( present(remove1stColumn ))then
        if( remove1stColumn .eqv. .true. )then
            nof=size(mat,1)
        endif
    endif
    
    if( present(remove2ndColumn ))then
        if( remove2ndColumn .eqv. .true. )then
            nof=size(mat,2)
        endif
    endif

    if(present(NextOf) )then
        if(NextOf >= nof)then
            return
        endif
    endif
    call copyArray(mat,buffer)


    if(present(NextOf))then
        nof=NextOf
    else 
        if( present(remove1stColumn ))then
            if( remove1stColumn .eqv. .true. )then
                nof=size(mat,1)-1
            endif
        endif
        
        if( present(remove2ndColumn ))then
            if( remove2ndColumn .eqv. .true. )then
                nof=size(mat,2)-1
            endif
        endif
    endif


    deallocate(mat)
    
    if( present(remove1stColumn ))then
        if( remove1stColumn .eqv. .true. )then
            if(size(buffer,1)-1 == 0)then
                print *, "Array is deleted"
                return
            endif
            allocate(mat(size(buffer,1)-1,size(buffer,2)) )
            mat(:,:)=0.0d0
            do i=1,nof
                mat(i,:)=buffer(i,:)
            enddo
            
            do i=nof+1,size(buffer,1)-1
                mat(i,:)=buffer(i+1,:)
            enddo

        endif
    endif
    if( present(remove2ndColumn ))then
        if( remove2ndColumn .eqv. .true. )then
            
            if(size(buffer,2)-1 == 0)then
                print *, "Array is deleted"
                return
            endif
            allocate(mat(size(buffer,1),size(buffer,2)-1) )
            mat(:,:)=0.0d0
            do i=1,nof
                mat(:,i)=buffer(:,i)
            enddo

            do i=nof+1,size(buffer,2)-1
                mat(:,i)=buffer(:,i+1)
            enddo
        endif
    endif

end subroutine
!##################################################



!##################################################
subroutine removeArrayReal(mat,remove1stColumn,remove2ndColumn,NextOf)
    real(real64),allocatable,intent(inout)::mat(:,:)
    real(real64),allocatable :: buffer(:,:)
    logical,optional,intent(in) :: remove1stColumn,remove2ndColumn
    
    integer(int32),optional,intent(in) :: NextOf
    
    integer(int32) :: i,nof,rmsin,m,n
    
    if( present(remove1stColumn ))then
        if( remove1stColumn .eqv. .true. )then
            nof=size(mat,1)
        endif
    endif
    
    if( present(remove2ndColumn ))then
        if( remove2ndColumn .eqv. .true. )then
            nof=size(mat,2)
        endif
    endif

    if(present(NextOf) )then
        if(NextOf >= nof)then
            return
        endif
    endif

    call copyArray(mat,buffer)



    if(present(NextOf))then
        nof=NextOf
    else 
        if( present(remove1stColumn ))then
            if( remove1stColumn .eqv. .true. )then
                nof=size(mat,1)-1
            endif
        endif
        
        if( present(remove2ndColumn ))then
            if( remove2ndColumn .eqv. .true. )then
                nof=size(mat,2)-1
            endif
        endif
    endif


    deallocate(mat)
    
    if( present(remove1stColumn ))then
        if( remove1stColumn .eqv. .true. )then
            if(size(buffer,1)-1 == 0)then
                print *, "Array is deleted"
                return
            endif
            allocate(mat(size(buffer,1)-1,size(buffer,2)) )
            mat(:,:)=0.0d0
            do i=1,nof
                mat(i,:)=buffer(i,:)
            enddo
            
            do i=nof+1,size(buffer,1)-1
                mat(i,:)=buffer(i+1,:)
            enddo

        endif
    endif
    if( present(remove2ndColumn ))then
        if( remove2ndColumn .eqv. .true. )then
            
            if(size(buffer,2)-1 == 0)then
                print *, "Array is deleted"
                return
            endif
            allocate(mat(size(buffer,1),size(buffer,2)-1) )
            mat(:,:)=0.0d0
            do i=1,nof
                mat(:,i)=buffer(:,i)
            enddo

            do i=nof+1,size(buffer,2)-1
                mat(:,i)=buffer(:,i+1)
            enddo
        endif
    endif

end subroutine
!##################################################


!##################################################
subroutine removeArrayReal3rdOrder(mat,remove1stColumn,remove2ndColumn,NextOf)
    real(real64),allocatable,intent(inout)::mat(:,:,:)
    real(real64),allocatable :: buffer(:,:,:)
    logical,optional,intent(in) :: remove1stColumn,remove2ndColumn
    
    integer(int32),optional,intent(in) :: NextOf
    
    integer(int32) :: i,nof,rmsin,m,n
    
    if( present(remove1stColumn ))then
        if( remove1stColumn .eqv. .true. )then
            nof=size(mat,1)
        endif
    endif
    
    if( present(remove2ndColumn ))then
        if( remove2ndColumn .eqv. .true. )then
            nof=size(mat,2)
        endif
    endif

    if(present(NextOf) )then
        if(NextOf >= nof)then
            return
        endif
    endif

    buffer = mat

    if(present(NextOf))then
        nof=NextOf
    else 
        if( present(remove1stColumn ))then
            if( remove1stColumn .eqv. .true. )then
                nof=size(mat,1)-1
            endif
        endif
        
        if( present(remove2ndColumn ))then
            if( remove2ndColumn .eqv. .true. )then
                nof=size(mat,2)-1
            endif
        endif
    endif


    deallocate(mat)
    
    if( present(remove1stColumn ))then
        if( remove1stColumn .eqv. .true. )then
            if(size(buffer,1)-1 == 0)then
                print *, "Array is deleted"
                return
            endif
            allocate(mat(size(buffer,1)-1,size(buffer,2),size(buffer,3)) )
            mat(:,:,:)=0.0d0
            do i=1,nof
                mat(i,:,:)=buffer(i,:,:)
            enddo
            
            do i=nof+1,size(buffer,1)-1
                mat(i,:,:)=buffer(i+1,:,:)
            enddo

        endif
    endif
    if( present(remove2ndColumn ))then
        if( remove2ndColumn .eqv. .true. )then
            
            if(size(buffer,2)-1 == 0)then
                print *, "Array is deleted"
                return
            endif
            allocate(mat(size(buffer,1),size(buffer,2)-1,size(buffer,3)) )
            mat(:,:,:)=0.0d0
            do i=1,nof
                mat(:,i,:)=buffer(:,i,:)
            enddo

            do i=nof+1,size(buffer,2)-1
                mat(:,i,:)=buffer(:,i+1,:)
            enddo
        endif
    endif

end subroutine
!##################################################




!##################################################
function meanVecReal(vec) result(mean_val)
    real(real64),intent(in)::vec(:)
    real(real64)::mean_val

    integer(int32) :: i
    mean_val=0.0d0
    do i=1,size(vec)
        mean_val=mean_val+vec(i)
    enddo
    mean_val=mean_val/dble(size(vec))
    
end function
!##################################################

!##################################################
function meanVecint(vec) result(mean_val)
    integer(int32),intent(in)::vec(:)
    integer(int32)::mean_val
    integer(int32) :: i
    mean_val=0
    do i=1,size(vec)
        mean_val=mean_val+vec(i)
    enddo
    mean_val=mean_val/size(vec)
    
end function
!##################################################


!##################################################
function distanceReal(x,y) result(dist)
    real(real64),intent(in)::x(:),y(:)
    real(real64)::dist

    integer(int32) :: i

    if(size(x)/=size(y) )then
        print *, "ERROR ArrayClass ::  distanceReal(x,y) size(x)/=size(y) "
        return
    endif
    dist=0.0d0
    do i=1,size(x)
        dist=dist+(x(i)-y(i) )*(x(i)-y(i) )
    enddo
    dist=sqrt(dist)
    return
end function
!##################################################


!##################################################
function distanceInt(x,y) result(dist)
    integer(int32),intent(in)::x(:),y(:)
    integer(int32)::dist

    integer(int32) :: i

    if(size(x)/=size(y) )then
        print *, "ERROR ArrayClass ::  distanceReal(x,y) size(x)/=size(y) "
        return
    endif
    dist=0
    do i=1,size(x)
        dist=dist+(x(i)-y(i) )*(x(i)-y(i) )
    enddo
    dist=int(sqrt(dble(dist)))
    return
end function
!##################################################


!##################################################
function countifSameIntArray(Array1,Array2) result(count_num)
    integer(int32),intent(in)::Array1(:,:),Array2(:,:)
    integer(int32) :: i,j,k,l,n,count_num,exist

    count_num=0
    do i=1,size(Array1,1)
        do j=1,size(Array1,2)
            exist=0
            do k=1,size(Array2,1)
                do l=1,size(Array2,2)
                    if(Array1(i,j)==Array2(k,l) )then
                        exist=1
                        exit
                    endif     
                enddo
                if(exist==1)then
                    exit
                endif
            enddo

            if(exist==1)then
                count_num=count_num+1
            endif
        enddo
    enddo

end function
!##################################################


!##################################################
function countifSameIntVec(Array1,Array2) result(count_num)
    integer(int32),intent(in)::Array1(:),Array2(:)
    integer(int32) :: i,j,k,l,n,count_num,exist

    count_num=0
    do i=1,size(Array1)
        exist=0
        do j=1,size(Array2,1)
            if(Array1(i)==Array2(j) )then
                exist=1
            endif    
        enddo
        if(exist==1)then
            count_num=count_num+1
        endif
    enddo

end function
!##################################################


!##################################################
function countifSameIntArrayVec(Array1,Array2) result(count_num)
    integer(int32),intent(in)::Array1(:,:),Array2(:)
    integer(int32) :: i,j,k,l,n,count_num,exist

    count_num=0
    do i=1,size(Array1,1)
        do j=1,size(Array1,2)
            exist=0
            do k=1,size(Array2,1)
                if(Array1(i,j)==Array2(k) )then
                    exist=1
                    exit
                endif  
                
                if(exist==1)then
                    exit
                endif  
            enddo
            if(exist==1)then
                count_num=count_num+1
            endif
        enddo
    enddo

end function
!##################################################



!##################################################
function countifSameIntVecArray(Array1,Array2) result(count_num)
    integer(int32),intent(in)::Array2(:,:),Array1(:)
    integer(int32) :: i,j,k,l,n,count_num,exist

    count_num=0
    do i=1,size(Array1,1)
        exist=0
        do j=1,size(Array2,1)
            do k=1,size(Array2,2)
                if(Array1(i)==Array2(j,k) )then
                    exist=1
                    exit
                endif    
            enddo
        enddo
        if(exist==1)then
            count_num=count_num+1
        endif
    enddo

end function
!##################################################


!##################################################
function countiflogicVec(Vector, tf) result(count_num)
    logical,intent(in)::Vector(:)
    logical,intent(in) :: tf
    integer(int32) :: count_num,i

    count_num=0
    do i=1,size(Vector)
        if(Vector(i) .eqv. tf)then
            count_num=count_num+1
        endif
    enddo

end function
!##################################################


!##################################################
function countifint(Array,Equal,notEqual,Value) result(count_num)
    integer(int32),intent(in)::Array(:,:),Value
    integer(int32) :: i,j,n,case,count_num
    logical,optional,intent(in)::Equal,notEqual
    

    if(present(Equal) )then
        if(Equal .eqv. .true.)then
            case=1
        else
            case=0
        endif
    
    elseif(present(notEqual) )then
        if(notEqual .eqv. .true.)then
            case=0
        else
            case=1
        endif
    else
        print *, "caution :: ArrayClass :: countifint :: please check Equal or notEqual"
        print *, "performed as Equal"
        case=0
    endif

    count_num=0
    if(case==0)then
        do i=1,size(Array,1)
            do j=1,size(Array,2)
                ! not Equal => count
                if(Array(i,j) /= Value )then
                    count_num=count_num+1
                else
                    cycle
                endif
            enddo
        enddo
    elseif(case==1)then

        do i=1,size(Array,1)
            do j=1,size(Array,2)
                ! Equal => count
                if(Array(i,j) == Value )then
                    count_num=count_num+1
                else
                    cycle
                endif
            enddo
        enddo
    else
        print *, "ERROR :: ArrayClass :: countifint :: please check Equal or notEqual"
    endif
end function
!##################################################

!##################################################
function countifintVec(Array,Equal,notEqual,Value) result(count_num)
    integer(int32),intent(in)::Array(:),Value
    integer(int32) :: i,j,n,case,count_num
    logical,optional,intent(in)::Equal,notEqual
    

    if(present(Equal) )then
        if(Equal .eqv. .true.)then
            case=1
        else
            case=0
        endif
    
    elseif(present(notEqual) )then
        if(notEqual .eqv. .true.)then
            case=0
        else
            case=1
        endif
    else
        print *, "caution :: ArrayClass :: countifint :: please check Equal or notEqual"
        print *, "performed as Equal"
        case=0
    endif

    count_num=0
    if(case==0)then
        do i=1,size(Array,1)
            ! not Equal => count
            if(Array(i) /= Value )then
                count_num=count_num+1
            else
                cycle
            endif
        
        enddo
    elseif(case==1)then

        do i=1,size(Array,1)
            ! Equal => count
            if(Array(i) == Value )then
                count_num=count_num+1
            else
                cycle
            endif
            
        enddo
    else
        print *, "ERROR :: ArrayClass :: countifint :: please check Equal or notEqual"
    endif
end function
!##################################################

!##################################################
function countifReal(Array,Equal,notEqual,Value) result(count_num)
    real(real64),intent(in)::Array(:,:),Value
    integer(int32) :: i,j,n,case,count_num
    logical,optional,intent(in)::Equal,notEqual
    

    if(present(Equal) )then
        if(Equal .eqv. .true.)then
            case=1
        else
            case=0
        endif
    
    elseif(present(notEqual) )then
        if(notEqual .eqv. .true.)then
            case=0
        else
            case=1
        endif
    else
        print *, "caution :: ArrayClass :: countifint :: please check Equal or notEqual"
        print *, "performed as Equal"
        case=0
    endif

    count_num=0
    if(case==0)then
        do i=1,size(Array,1)
            do j=1,size(Array,2)
                ! not Equal => count
                if(Array(i,j) /= Value )then
                    count_num=count_num+1
                else
                    cycle
                endif
            enddo
        enddo
    elseif(case==1)then

        do i=1,size(Array,1)
            do j=1,size(Array,2)
                ! Equal => count
                if(Array(i,j) == Value )then
                    count_num=count_num+1
                else
                    cycle
                endif
            enddo
        enddo
    else
        print *, "ERROR :: ArrayClass :: countifint :: please check Equal or notEqual"
    endif
end function
!##################################################

!##################################################
function countifRealVec(Array,Equal,notEqual,Value) result(count_num)
    real(real64),intent(in)::Array(:),Value
    integer(int32) :: i,j,n,case,count_num
    logical,optional,intent(in)::Equal,notEqual
    

    if(present(Equal) )then
        if(Equal .eqv. .true.)then
            case=1
        else
            case=0
        endif
    
    elseif(present(notEqual) )then
        if(notEqual .eqv. .true.)then
            case=0
        else
            case=1
        endif
    else
        print *, "caution :: ArrayClass :: countifint :: please check Equal or notEqual"
        print *, "performed as Equal"
        case=0
    endif

    count_num=0
    if(case==0)then
        do i=1,size(Array,1)
            ! not Equal => count
            if(Array(i) /= Value )then
                count_num=count_num+1
            else
                cycle
            endif
        
        enddo
    elseif(case==1)then

        do i=1,size(Array,1)
            ! Equal => count
            if(Array(i) == Value )then
                count_num=count_num+1
            else
                cycle
            endif
            
        enddo
    else
        print *, "ERROR :: ArrayClass :: countifint :: please check Equal or notEqual"
    endif
end function
!##################################################


!##################################################
recursive subroutine quicksortint(list) 
    integer(int32),intent(inout) :: list(:)
    integer(int32) :: border,a,b,buf
    integer(int32) :: i,j,border_id,n,start,last,scope1_out,scope2_out
    integer(int32) :: scope1,scope2
    logical :: crossed
    ! http://www.ics.kagoshima-u.ac.jp/~fuchida/edu/algorithm/sort-algorithm/quick-sort.html


    
    scope1=1
    scope2=size(list)
    
    if(size(list)==1)then
        return
    endif

    ! determine border
    n=size(list)
    a=list(1)
    b=list(2)

    
    if(a >= b)then
        border=a
        if(size(list)==2 )then
            list(1)=b
            list(2)=a
            return
        endif
    else
        border=b
        if(size(list)==2 )then
            list(1)=a
            list(2)=b
            return
        endif
    endif

    last=scope2
    crossed=.false.

    
    do start=scope1,scope2
        if(list(start)>=border)then
            do 
                if(list(last)<border )then
                    ! exchange
                    buf=list(start)
                    list(start)=list(last)
                    list(last)=buf
                    exit
                else
                    if(start >=last )then
                        crossed = .true.
                        exit
                    else
                        last=last-1
                    endif
                endif
            enddo
        else
            cycle
        endif 

        if(crossed .eqv. .true.)then
            call quicksort(list(scope1:start))
            call quicksort(list(last:scope2))
            exit
        else
            cycle
        endif

    enddo

end subroutine
!##################################################



!##################################################
recursive subroutine quicksortreal(list) 
    real(real64),intent(inout) :: list(:)
    real(real64) :: border,a,b,buf
    integer(int32) :: i,j,border_id,n,start,last,scope1_out,scope2_out
    integer(int32) :: scope1,scope2
    logical :: crossed
    ! http://www.ics.kagoshima-u.ac.jp/~fuchida/edu/algorithm/sort-algorithm/quick-sort.html


    
    scope1=1
    scope2=size(list)
    
    if(size(list)==1)then
        return
    endif

    ! determine border
    n=size(list)
    a=list(1)
    b=list(2)

    
    if(a >= b)then
        border=a
        if(size(list)==2 )then
            list(1)=b
            list(2)=a
            return
        endif
    else
        border=b
        if(size(list)==2 )then
            list(1)=a
            list(2)=b
            return
        endif
    endif

    last=scope2
    crossed=.false.

    
    do start=scope1,scope2
        if(list(start)>=border)then
            do 
                if(list(last)<border )then
                    ! exchange
                    buf=list(start)
                    list(start)=list(last)
                    list(last)=buf
                    exit
                else
                    if(start >=last )then
                        crossed = .true.
                        exit
                    else
                        last=last-1
                    endif
                endif
            enddo
        else
            cycle
        endif 

        if(crossed .eqv. .true.)then
            call quicksort(list(scope1:start))
            call quicksort(list(last:scope2))
            exit
        else
            cycle
        endif

    enddo

end subroutine
!##################################################

!##################################################
function getifReal(Array,Value) result(list)
    real(real64),intent(in)::Array(:,:),Value
    integer(int32) :: i,j,n,m,countifSame,l
    integer(int32),allocatable :: list(:,:)

    n=countif(Array=Array, Equal=.true., Value=Value)
    allocate(list(n,2))
    l=0
    do i=1,size(Array,1)
        do j=1,size(Array,2)
            if(Value==Array(i,j) )then
                l=l+1
                list(l,1)=i
                list(l,2)=j
            endif
        enddo
    enddo
end function
!##################################################


!##################################################
function getifRealVec(Array,Value) result(list)
    real(real64),intent(in)::Array(:),Value
    integer(int32) :: i,j,n,m,countifSame,l,k
    integer(int32),allocatable :: list(:)

    n=countif(Array=Array, Equal=.true., Value=Value)
    allocate(list(n))
    l=0
    do i=1,size(Array,1)
        if(Value==Array(i) )then
            l=l+1
            list(l)=i
        endif
    enddo
end function
!##################################################

subroutine getKeyAndValueReal(Array, Key, info)
    real(real64),intent(in) :: Array(:,:)
    integer(int32),allocatable,intent(inout) :: Key(:)
    real(real64),allocatable,intent(inout) :: info(:,:)
    integer(int32) :: i,j,n,m,k,cou,cou2,id,sz
    logical :: hit

    n=size(Array,1)
    m=size(Array,2)
    if( allocated(key) )then
        deallocate(key)
    endif
    if( allocated(info) )then
        deallocate(info)
    endif
    allocate(key(n))
    allocate(info(1,m))
    
    key(:)=1
    info(1,1:m)=Array(1,1:m)
    sz=1
    do i=2,n
        ! check list
        hit = .false.
        do j=1,size(info,1)
            cou=0
            do k=1,size(info,2)
                if(Array(i,k)==info(j,k) )then
                    cou=cou+1
                else
                    cycle
                endif
            enddo
            if(cou==m)then
                !hit
                hit = .true.
                key(i)=j
                exit
            else
                cycle
            endif
        enddo
        if(hit .eqv. .true.)then
            cycle
        else
            ! add a new key and info
            call extendArray(mat=info,extend1stColumn=.true.)
            sz=sz+1
            info(sz,1:m)=Array(i,1:m)
        endif
    enddo

end subroutine getKeyAndValueReal


function imcompleteCholosky(mat) result(a)
    real(real64) :: mat(:,:)
    real(real64),allocatable :: a(:,:)
    integer(int32) :: n,i,j,k
    n=size(mat,1)
    allocate(a(n,n) )
    a(:,:)=mat(:,:)
    do k=1,n
        if(a(k,k) ==0.0d0)then
            stop
        endif
        a(k,k)= dsqrt( abs(a(k,k)) )
        do i=k+1, n
            if(a(i,k)/=0.0d0 )then
                if(a(k,k) ==0.0d0)then
                    print *, "ERROR :: a(k,k) ==0.0d0"
                    stop
                endif
                !print *, a(k,k) , "aik",k
                a(i,k) = a(i,k)/a(k,k)
                !print *, a(i,k) , "aik"
            endif
        enddo
        do j=k+1,n
            do i=j,n
                if( a(i,j) /= 0.0d0 )then
                    !print *, a(i,j),a(i,k),a(j,k) ,"dsf",i,j,k
                    a(i,j)=a(i,j)-a(i,k)*a(j,k)
                endif
            enddo
        enddo
        do i=1,n
            do j=i+1,n
                a(i,j)=0.0d0
            enddo
        enddo
    enddo

    call showArray(a)
end function


! ##########################################################
subroutine addListIntVec(vector,val)
    integer(int32),allocatable,intent(inout) :: vector(:)
    integer(int32),intent(in) :: val
    integer(int32),allocatable :: buffer(:)
    integer(int32) :: i,j,k,n
    logical :: ret

    ! search 
    ret = exist(vector,val)
    if(.not. allocated(vector) )then
        allocate(vector(1) )
        vector(1)=Val
        return
    endif

    if(size(vector)==0 )then
        deallocate(vector)
        allocate(vector(1) )
        vector(1)=Val
        return
    endif
    ! if exist, add this
    if(ret .eqv. .true.)then
        n=size(vector) 
        allocate(buffer(n) )
        buffer(:) = vector(:)
        deallocate(vector)
        allocate(vector(n+1) )
        vector(1:n)=buffer(1:n)
        vector(n+1)=val
    endif

end subroutine
! ##########################################################



! ##########################################################
function existIntVec(vector,val) result(ret)
    integer(int32),allocatable,intent(inout) :: vector(:)
    integer(int32),intent(in) :: val
    logical :: ret
    integer(int32) :: i,j,k,n

    if(.not. allocated(vector) )then
        return
    endif
!
    ! search 
    do i=1,size(vector)
        if(vector(i) == val ) then
            ret=.true.
            return
        endif
    enddo

end function
! ##########################################################




! ##########################################################
function existIntArray(vector,val,rowid,columnid) result(ret)
    integer(int32),allocatable,intent(inout) :: vector(:,:)
    integer(int32),intent(in) :: val
    integer(int32),optional,intent(in) :: columnid,rowid
    logical :: ret
    integer(int32) :: i,j,k,n

    if(.not. allocated(vector) )then
        return
    endif
!

    ! search 
    if(present(columnid) )then
        do i=1,size(vector,1)
            if(vector(i,columnid) == val ) then
                ret=.true.
                return
            endif
        enddo    
    endif
    
    if(present(rowid) )then
        do i=1,size(vector,2)
            if(vector(rowid,i) == val ) then
                ret=.true.
                return
            endif
        enddo    
    endif

    do i=1,size(vector,1)
        do j=1,size(vector,2)
            if(vector(i,j) == val ) then
                ret=.true.
                return
            endif
        enddo
    enddo

end function
! ##########################################################



!##################################################
subroutine printArrayInt(Mat,IndexArray,FileHandle,Name,Add)
    integer(int32),intent(in)::Mat(:,:)
    integer(int32),optional,intent(in) :: IndexArray(:,:)
    integer(int32),optional,intent(in)::FileHandle ,Add
    character(*),optional,intent(in)::Name
    integer(int32) :: fh,i,j,k,l,nn

    nn=input(default=0,option=Add)
    if(present(FileHandle) )then
        fh=FileHandle
    else
        fh=10
    endif

    if(present(Name) )then
        open(fh,file=Name)
    endif
    
    if(present(IndexArray))then
        
        do i=1,size(IndexArray,1)
            do j=1,size(IndexArray,2)
                k = IndexArray(i,j)
                if(k <= 0)then
                    cycle
                endif
                
                
                if(present(FileHandle) .or. present(Name) )then
                    do l=1,size(Mat,2)-1
                        write(fh,'(i0)',advance='no') Mat(k,l)+nn
                        write(fh,'(A)',advance='no') "     "
                    enddo
                    write(fh,'(i0)',advance='yes') Mat(k,size(Mat,2) )+nn
                else
                    print *, Mat(k,:)
                endif
            enddo
        enddo
    else

        do j=1,size(Mat,1)
            
            if(present(FileHandle) .or. present(Name) )then
                !write(fh,*) Mat(j,:)
                do k=1,size(Mat,2)-1
                    write(fh,'(i0)',advance='no') Mat(j,k)+nn
                    write(fh,'(A)',advance='no') "     "
                enddo
                write(fh,'(i0)',advance='yes') Mat(j,size(Mat,2) )+nn
            else
                print *, Mat(j,:)
            endif

        enddo
        
    endif
    
    if(present(FileHandle) .or. present(Name) )then
        flush(fh)
    endif


    if(present(Name) )then
        close(fh)
    endif


end subroutine 
!##################################################




!##################################################
subroutine printArrayIntVec(Mat,IndexArray,FileHandle,Name,Add)
    integer(int32),intent(in)::Mat(:)
    integer(int32),optional,intent(in) :: IndexArray(:,:)
    integer(int32),optional,intent(in)::FileHandle ,Add
    character(*),optional,intent(in)::Name
    integer(int32) :: fh,i,j,k,l,nn

    nn=input(default=0,option=Add)
    if(present(FileHandle) )then
        fh=FileHandle
    else
        fh=10
    endif

    if(present(Name) )then
        open(fh,file=Name)
    endif
    
    if(present(IndexArray))then
        
        do i=1,size(IndexArray,1)
            do j=1,size(IndexArray,2)
                k = IndexArray(i,j)
                if(k <= 0)then
                    cycle
                endif
                
                
                if(present(FileHandle) .or. present(Name) )then
                    write(fh,'(i0)') Mat(k)+nn
                else
                    print *, Mat(k)
                endif
            enddo
        enddo
    else

        do j=1,size(Mat,1)
            
            if(present(FileHandle) .or. present(Name) )then
                !write(fh,*) Mat(j,:)
                write(fh,'(i0)') Mat(j)+nn
            else
                print *, Mat(j)
            endif

        enddo
        
    endif
    
    if(present(FileHandle) .or. present(Name) )then
        flush(fh)
    endif


    if(present(Name) )then
        close(fh)
    endif


end subroutine 
!##################################################


!##################################################
subroutine printArrayReal(Mat,IndexArray,FileHandle,Name,Add)
    real(real64),intent(in)::Mat(:,:)
    real(real64),optional,intent(in) :: Add
    integer(int32),optional,intent(in) :: IndexArray(:,:)
    integer(int32),optional,intent(in)::FileHandle
    character(*),optional,intent(in)::Name
    real(real64) :: nn
    integer(int32) :: fh,i,j,k,l

    nn=input(default=0.0d0,option=Add)
    if(present(FileHandle) )then
        fh=FileHandle
    else
        fh=10
    endif

    if(present(Name) )then
        open(fh,file=Name)
    endif

    if(present(IndexArray))then
        
        do i=1,size(IndexArray,1)
            do j=1,size(IndexArray,2)
                k = IndexArray(i,j)
                if(k <= 0)then
                    cycle
                endif
                
                
                if(present(FileHandle) .or. present(Name) )then
                    !write(fh,*) Mat(k,:)
                    do l=1,size(Mat,2)-1
                        write(fh, '(e22.14e3)',advance='no' ) Mat( k,l )+nn
                        write(fh,'(A)',advance='no') "     "
                    enddo
                    write(fh,'(e22.14e3)',advance='yes') Mat( k,size(Mat,2) )+nn
                else
                    print *, Mat(k,:)
                endif
            enddo
        enddo
    else

        do j=1,size(Mat,1)
            
            if(present(FileHandle) .or. present(Name) )then
                !write(fh,*) Mat(j,:)
                do l=1,size(Mat,2)-1
                    write(fh,'(e22.14e3)',advance='no') Mat( j,l )+nn
                    write(fh,'(A)',advance='no') "     "
                enddo
                write(fh,'(e22.14e3)',advance='yes') Mat( j,size(Mat,2) )+nn
            else
                print *, Mat(j,:)
            endif

        enddo
        
    endif
    
    if(present(FileHandle) .or. present(Name) )then
        flush(fh)
    endif



    if(present(Name) )then
        close(fh)
    endif

end subroutine 
!##################################################




!##################################################
subroutine printArrayRealVec(Mat,IndexArray,FileHandle,Name,Add)
    real(real64),intent(in)::Mat(:)
    integer(int32),optional,intent(in) :: IndexArray(:,:)
    integer(int32),optional,intent(in)::FileHandle ,Add
    character(*),optional,intent(in)::Name
    integer(int32) :: fh,i,j,k,l,nn

    nn=input(default=0,option=Add)
    if(present(FileHandle) )then
        fh=FileHandle
    else
        fh=10
    endif

    if(present(Name) )then
        open(fh,file=Name)
    endif
    
    if(present(IndexArray))then
        
        do i=1,size(IndexArray,1)
            do j=1,size(IndexArray,2)
                k = IndexArray(i,j)
                if(k <= 0)then
                    cycle
                endif
                
                
                if(present(FileHandle) .or. present(Name) )then
                    write(fh,'(i0)') Mat(k)+nn
                else
                    print *, Mat(k)
                endif
            enddo
        enddo
    else

        do j=1,size(Mat,1)
            
            if(present(FileHandle) .or. present(Name) )then
                !write(fh,*) Mat(j,:)
                write(fh,'(i0)') Mat(j)+nn
            else
                print *, Mat(j)
            endif

        enddo
        
    endif
    
    if(present(FileHandle) .or. present(Name) )then
        flush(fh)
    endif


    if(present(Name) )then
        close(fh)
    endif


end subroutine 
!##################################################


function getext(char) result(ext)
        
    
    character(*),intent(in) :: char
    character(7) :: ext
    integer(int32) :: n,m

    ext="       "
    n=0
    n = index(char,".", back=.true.)   
    m = len(char)
    if(n==0)then
        print *, "No extention"
        return
    endif

    if(m-n+1 <1)then
        print *, "ERROR :: ArrayClass/extention"
        stop
    endif

    ext(1:m-n+1) = char(n+1:m)

end function

! ############################################################
function anglesReal3D(vector1, vector2) result(angles)
    real(real64), intent(in) :: vector1(3), vector2(3)
    real(real64) :: angles(3),unit1(3),unit2(3),e3(3)
   
    e3(:)=0.0d0
    e3(3)=1.0d0
    unit1(:)=0.0d0
    unit2(:)=0.0d0
    angles(:)=0.0d0
    ! xy-平面で眺める
    unit1(1:2) = vector1(1:2)
    unit2(1:2) = vector2(1:2)
    angles(3)=asin( norm(cross_product(unit1,unit2))/norm(unit1)/norm(unit2) )
    e3(:)=cross_product(unit1,unit2)
    if(e3(3)==0.0d0)then
        angles(3)=0.0d0
    else
        angles(3)=angles(3)*e3(3)
    endif

    
    ! yz-平面で眺める
    unit1(1:2) = vector1(2:3)
    unit2(1:2) = vector2(2:3)
    angles(1)=asin( norm(cross_product(unit1,unit2))/norm(unit1)/norm(unit2) )
    e3(:)=cross_product(unit1,unit2)
    if(e3(3)==0.0d0)then
        angles(1)=0.0d0
    else
        angles(1)=angles(1)*e3(3)
    endif
    ! xz-平面で眺める
    unit1(1) = vector1(1)
    unit2(1) = vector2(1)
    unit1(2) = vector1(3)
    unit2(2) = vector2(3)
    angles(2)=asin( norm(cross_product(unit1,unit2))/norm(unit1)/norm(unit2) )
    e3(:)=cross_product(unit1,unit2)
    angles(2)=angles(2)*signmm(e3(3))
    if(e3(3)==0.0d0)then
        angles(2)=0.0d0
    else
        angles(2)=angles(2)*e3(3)
    endif
end function
! ############################################################


! ############################################################
subroutine jsonArrayReal(array,fh,name,endl)
    real(real64),intent(in) :: array(:,:)
    integer(int32),intent(in) :: fh
    character(*),intent(in) :: name
    integer(int32) :: i,j,n
    logical,optional,intent(in) :: endl

    if(size(array,1)==0 .or. size(array,1)==1)then
        return
    endif

    write(fh,'(a)') '"'//trim(name)//'" : ['
    do i=1,size(array,1)
        write(fh,'(a)',advance='no') '['
        do j=1,size(array,2)
            if(j==size(array,2))then
                if(abs(array(i,j)) < 1.0d0 )then
                    if(array(i,j)<=0.0d0)then
                        write(fh,'(a)',advance='no') "-0"//trim(str(abs(array(i,j))))
                    else
                        write(fh,'(a)',advance='no') "0"//trim(str(array(i,j)))
                    endif
                else
                    write(fh,'(a)',advance='no') trim(str(array(i,j)))
                endif
            else
                if(abs(array(i,j)) < 1.0d0 )then
                    if(array(i,j)<=0.0d0)then
                        write(fh,'(a)',advance='no') "-0"//trim(str(abs(array(i,j))))//','
                    else
                        write(fh,'(a)',advance='no') "0"//trim(str(array(i,j)))//','
                    endif
                else
                    write(fh,'(a)',advance='no') trim(str(array(i,j)))//','
                endif
            endif
        enddo
        if(i==size(array,1))then
            write(fh,'(a)',advance='yes') ']'
        else
            write(fh,'(a)',advance='yes') '],'
        endif
    enddo
    if(present(endl) )then
        if(endl .eqv. .true.)then
            write(fh,'(a)') ']'        
            return
        endif
    endif
    write(fh,'(a)') '],'
end subroutine
! ############################################################


! ############################################################
subroutine jsonArrayInt(array,fh,name,endl)
    integer(int32),intent(in) :: array(:,:)
    integer(int32),intent(in) :: fh
    character(*),intent(in) :: name
    logical,optional,intent(in) :: endl
    integer(int32) :: i,j,n

    if(size(array,1)==0 )then
        return
    endif
    write(fh,'(a)') '"'//trim(name)//'" : ['
    do i=1,size(array,1)
        write(fh,'(a)',advance='no') '['
        do j=1,size(array,2)
            if(j==size(array,2))then
                write(fh,'(a)',advance='no') trim(str(array(i,j)))
            else
                write(fh,'(a)',advance='no') trim(str(array(i,j)))//','
            endif
        enddo
        if(i==size(array,1))then
            write(fh,'(a)',advance='yes') ']'
        else
            write(fh,'(a)',advance='yes') '],'
        endif
    enddo
    if(present(endl) )then
        if(endl .eqv. .true.)then
            write(fh,'(a)') ']'        
            return
        endif
    endif
    write(fh,'(a)') '],'
end subroutine
! ############################################################


! ############################################################
subroutine jsonArrayRealVec(array,fh,name,endl)
    real(real64),intent(in) :: array(:)
    integer(int32),intent(in) :: fh
    character(*),intent(in) :: name
    integer(int32) :: i,j,n
    logical,optional,intent(in) :: endl


    if(size(array,1)==0 .or. size(array,1)==1)then
        return
    endif

    write(fh,'(a)',advance='no') '"'//trim(name)//'" : ['
    do i=1,size(array,1)
        if(abs(array(i)) < 1.0d0 )then
            if(array(i)<=0.0d0)then
                write(fh,'(a)',advance='no') "-0"//trim(str(abs(array(i))))
            else
                write(fh,'(a)',advance='no') "0"//trim(str(array(i)))
            endif
        else
            write(fh,'(a)',advance='no') trim(str(array(i)))
        endif
        if(i/=size(array,1) )then
            write(fh,'(a)',advance='yes') ","
        endif
    enddo
    if(present(endl) )then
        if(endl .eqv. .true.)then
            write(fh,'(a)') ']'        
            return
        endif
    endif
    write(fh,'(a)') '],'
end subroutine
! ############################################################



! ############################################################
subroutine jsonArrayIntVec(array,fh,name,endl)
    integer(int32),intent(in) :: array(:)
    integer(int32),intent(in) :: fh
    character(*),intent(in) :: name
    integer(int32) :: i,j,n
    logical,optional,intent(in) :: endl

    if(size(array,1)==0 )then
        return
    endif
    write(fh,'(a)',advance='no') '"'//trim(name)//'" : ['
    do i=1,size(array,1)
        write(fh,'(a)',advance='no') trim(str(array(i)))
        if(i/=size(array,1) )then
            write(fh,'(a)',advance='yes') ","
        endif
    enddo
    if(present(endl) )then
        if(endl .eqv. .true.)then
            write(fh,'(a)') ']'        
            return
        endif
    endif
    write(fh,'(a)') '],'
end subroutine
! ############################################################



end module ArrayClass