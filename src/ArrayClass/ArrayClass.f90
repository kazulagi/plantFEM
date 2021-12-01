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
    !interface fit
    !    module procedure :: fitReal64
    !end interface


    interface I_dx
        module procedure :: I_dx_real64,I_dx_real32,I_dx_complex64
    end interface

    interface d_dx
        module procedure :: d_dx_real64,d_dx_real32,d_dx_complex64
    end interface

    interface exchange
        module procedure :: exchangeInt32vector,exchangeInt32vector2
    end interface exchange

    interface shift
        module procedure :: shiftInt32vector
    end interface
    ! statistics

    interface matrix
        module procedure :: matrixFromVectorsRe64,matrixFromVectorsint32
    end interface

    
    interface interpolate
        module procedure :: interpolateOneReal64,interpolateOneComplex64,interpolateOneReal32
    end interface
    

    interface refine
        module procedure :: RefineSequenceReal64,RefineSequenceComplex64,RefineSequenceReal32
    end interface

    interface convolve
        module procedure :: convolveComplex64,convolveReal64
    end interface convolve

    interface linspace
        module procedure :: linspace1D, linspace1Dcomplex64,&
                 linspace1Dreal32, linspace2D, linspace3D,linspace4D
    end interface linspace

    interface windowing
        module procedure :: windowingComplex64,windowingReal64
    end interface windowing

    interface rotationMatrix
        module procedure :: rotationMatrixReal64
    end interface

    interface farthestVector
        module procedure :: farthestVectorReal64 
    end interface

    interface hstack
        module procedure :: hstackInt32Vector2,hstackInt32Vector3, hstackReal64Vector2,hstackReal64Vector3
    end interface

    interface dot_product_omp
        module procedure :: dot_product_omp
    end interface

    interface zeros
        module procedure :: zerosRealArray, zerosRealVector,zerosRealArray3,zerosRealArray4,zerosRealArray5,&
            zerosRealArray_64, zerosRealVector_64,zerosRealArray3_64,zerosRealArray4_64,zerosRealArray5_64
    end interface

    interface increment
        module procedure :: incrementRealVector, incrementIntVector,&
            incrementRealArray, incrementIntArray 
    end interface

    interface taper
        module procedure :: taperreal64
    end interface taper

    interface average
        module procedure :: averageInt32, averageReal64
    end interface

    interface arange
        module procedure :: arangeRealVector
    end interface

    interface reshape
        module procedure :: reshapeRealVector,reshapeIntVector
    end interface

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

    interface shape
        module procedure :: shapeVecInt,shapeVecReal,shapeArray2Int,shapeArray2Real,&
            shapeArray3Int,shapeArray3Real,shapeArray4Int,shapeArray4Real
    end interface
    
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

    interface searchAndRemove
        module procedure :: searchAndRemoveInt
    end interface

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

    interface sameAsGroup
        module procedure :: sameAsGroupintVec
    end interface

    interface countif
        module procedure ::countifint,countifintvec,countifReal,countifRealvec,countiflogicVec,countifChar
    end interface

    interface exist
        module procedure :: existIntVec, existIntArray
    end interface


    interface exists
        module procedure :: existIntVec, existIntArray
    end interface

    interface getif
        module procedure ::getifreal,getifrealvec!,getifint,getifintvec
    end interface

    interface quicksort
        module procedure :: quicksortreal,quicksortint,quicksortintArray
    end interface

    interface getKeyAndValue
        module procedure :: getKeyAndValueReal
    end interface
    
    interface addlist
        module procedure :: addListIntVec
    end interface

    interface Angles
        module procedure :: anglesReal3D,anglesReal2D
    end interface

    interface unwindLine
        module procedure :: unwindLineReal
    end interface
    
    type :: FlexibleChar_
        character(len=:),allocatable :: string
    end type

    type :: Array_
        integer(int32),allocatable  :: inta(:,:)
        real(real64),allocatable ::reala(:,:)
        type(FlexibleChar_),allocatable :: list(:,:)
    contains
        procedure, public :: array => arrayarrayReal
        procedure, public :: init => zerosRealArrayArrayClass
        procedure, public :: zeros => zerosRealArrayArrayClass
        procedure, public :: eye => eyeRealArrayArrayClass
        procedure, public :: unit => eyeRealArrayArrayClass
        procedure, public :: random => randomRealArrayArrayClass
        procedure, public :: print => printArrayClass
        
    end type


    public :: operator(+)
    public :: assignment(=)
    public :: operator(*)
    

    interface operator(+)
      module procedure addArrayClass
    end interface

    interface operator(*)
        module procedure multArrayClass
    end interface

    
    interface assignment(=)
      module procedure assignArrayAlloInt,assignArrayAlloReal, assignAlloArrayInt,assignAlloArrayReal,&
        assignVectorFromChar,assignIntVectorFromChar
    end interface

    interface dot
        module procedure dotArrayClass
    end interface

contains
    

! ===============================================
function addArrayClass(x, y) result(z)
    implicit none
    type(Array_), intent(in) :: x, y
    type(Array_) :: z
    
    z%reala = x%reala + y%reala
end function 
! ===============================================


! ===============================================
subroutine assignArrayAlloReal(x, y)
    implicit none
    type(Array_), intent(out) :: x
    real(real64), intent(in)  :: y(:,:)
  
    x%reala = y

end subroutine assignArrayAlloReal
! ===============================================



! ===============================================
subroutine assignAlloArrayReal(x, y)
    implicit none

    real(real64),allocatable,intent(out)  :: x(:,:)
    type(Array_), intent(in) :: y
  
    x = y%reala

end subroutine assignAlloArrayReal
! ===============================================

! ===============================================
subroutine assignVectorFromChar(x,chx)
    implicit none
    real(real64),allocatable,intent(out) :: x(:)
    character(*),intent(in) :: chx
    integer(int32) :: i,j,n,m
  
    ! check format 
    ! if chx = [ 1.000, 2.000, 3.000 ...]then
    if(index(chx,"[")/=0 .and. index(chx,"]")/=0 )then
        n = index(chx,"[")
        m = index(chx,"]")
        allocate(x(1:countif(from=chx(n+1:m-1),keyword="," )+1 ) )
        read(chx(n+1:m-1),*) x(:)
    else
        read(chx(n+1:m-1),*) x(:)
    endif
    
end subroutine
! ===============================================


! ===============================================
subroutine assignIntVectorFromChar(x,chx)
    implicit none
    integer(int32),allocatable,intent(out) :: x(:)
    character(*),intent(in) :: chx
    integer(int32) :: i,j,n,m
  
    ! check format 
    ! if chx = [ 1.000, 2.000, 3.000 ...]then
    if(index(chx,"[")/=0 .and. index(chx,"]")/=0 )then
        n = index(chx,"[")
        m = index(chx,"]")
        allocate(x(1:countif(from=chx(n+1:m-1),keyword="," )+1 ) )
        read(chx(n+1:m-1),*) x(:)
    else
        read(chx(n+1:m-1),*) x(:)
    endif
    
end subroutine
! ===============================================


! ===============================================
!subroutine assignArrayFromChar(x,chx)
!    implicit none
!    real(real64),allocatable,intent(out) :: x(:,:)
!    real(real64),allocatable :: vec(:)
!    character(*),intent(in) :: chx
!    integer(int32) :: i,j,n,m,col,row,head_l,tail_l
!  
!    ! check format 
!    ! if chx = [ 1.000, 2.000, 3.000 ...]then
!    
!    if(index(chx,"[[")/=0 .and. index(chx,"]]")/=0 )then
!        row = countif(from=chx,keyword="[") -1
!        col = (countif(from=chx,keyword=",") - row +1)/row + 1
!
!        ! num(,) = (x - 1)*row + (row-1)
!        !(num(,) - (row-1) )/row +1 = x  
!        x = zeros(row,col)
!        
!        n = index(chx,"[[") ! 1
!        m = index(chx,"]]") ! end
!
!        print *, row, col
!        do i=1, row
!            head_l = n+1
!            tail_l = index(chx(head_l:m),"]" )
!            vec = chx(head_l:tail_l)
!            x(i,:) = vec(:)
!            n = tail_l + 1
!        enddo
!    else
!        read(chx(n+1:m-1),*) x(:,:)
!    endif
!    
!end subroutine
! ===============================================


! ===============================================
subroutine assignArrayAlloint(x, y)
    implicit none
    type(Array_), intent(out) :: x
    integer(int64), intent(in)  :: y(:,:)
  
    x%inta = y

end subroutine assignArrayAlloint
! ===============================================



! ===============================================
subroutine assignAlloArrayint(x, y)
    implicit none

    integer(int64),allocatable,intent(out)  :: x(:,:)
    type(Array_), intent(in) :: y
  
    x = y%inta

end subroutine assignAlloArrayint
! ===============================================


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
    character(*),intent(in) :: path
    character(*),optional,intent(in) :: name,extention
    character(len=:),allocatable :: nm,ex 
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
    if(present(name) )then
        nm =name
    else
        nm=""
    endif
    if(present(extention) )then
        ex =extention
    else
        ex=""
    endif
    
    
    open(fh,file = trim(path)//trim(nm)//trim(ex),status="old" )
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
        val = 0
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

function countifChar(from,keyword) result(count_num)
    character(*),intent(in) :: from, keyword
    integeR(int32) :: count_num,i,j,n,m

    count_num=0
    i=1
    n = len(from)
    m = len(keyword)
    if(index(from(i:n),keyword)==0) return
    
    do j=1,n-m+1
        if(index(from(j:j+m-1),keyword)/=0)then
            count_num = count_num + 1
        else
            cycle
        endif
    enddo

end function

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

!subroutine heapsort(array)
!    
!    real(real64),intent(inout) :: array(:)
!   
!    integer ::i,k,j,l,n
!    real(real64) :: t
!   
!    n=size(array)
!    if(n.le.0)then
!       write(6,*)"Error, at heapsort"; stop
!    endif
!    if(n.eq.1)return
!  
!    l=n/2+1
!    k=n
!    do while(k.ne.1)
!       if(l.gt.1)then
!          l=l-1
!          t=array(L)
!       else
!          t=array(k)
!          array(k)=array(1)
!          k=k-1
!          if(k.eq.1) then
!             array(1)=t
!             exit
!          endif
!       endif
!       i=l
!       j=l+l
!       do while(j.le.k)
!          if(j.lt.k)then
!             if(array(j).lt.array(j+1))j=j+1
!          endif
!          if (t.lt.array(j))then
!             array(i)=array(j)
!             i=j
!             j=j+j
!          else
!             j=k+1
!          endif
!       enddo
!       array(i)=t
!    enddo
!  
!    return
!  end subroutine heapsort

!##################################################
recursive subroutine quicksortint(list,val) 
    integer(int32),intent(inout) :: list(:) 
    real(real64),optional,intent(inout) :: val(:)
    integer(int32) :: border,a,b,buf
    real(real64) :: buf_r
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

            buf_r=val(1)
            val(1)=val(2)
            val(2)=buf_r
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

                    buf_r=val(start)
                    val(start)=val(last)
                    val(last)=buf_r
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
            if(.not.present(val) )then
                call quicksort(list(scope1:start))
                call quicksort(list(last:scope2))
            else
                call quicksort(list(scope1:start),val(scope1:start) )
                call quicksort(list(last:scope2),val(last:scope2) )

            endif
            exit
        else
            cycle
        endif

    enddo
end subroutine
!##################################################


!##################################################
recursive subroutine quicksortintArray(list,val) 
    integer(int32),intent(inout) :: list(:,:) ! rowごとにn個の情報がある．
    real(real64),intent(inout) :: val(:)
    integer(int32),allocatable :: a(:),b(:),border(:),buf(:)
    integer(int32) :: i,j,border_id,n,start,last,scope1_out,scope2_out
    integer(int32) :: scope1,scope2,total_id_a,total_id_b,num_b_win,num_eq
    real(real64) :: val1,val2,v1,v2
    logical :: a_wins,border_wins,list_wins
    logical :: crossed
    ! http://www.ics.kagoshima-u.ac.jp/~fuchida/edu/algorithm/sort-algorithm/quick-sort.html


    
    scope1=1
    scope2=size(list,1)

    n = size(list,2)
    
    allocate(a(n) )
    allocate(b(n) )
    allocate(border(n) )
    allocate(buf(n) )
    
    if(size(list,1)==1)then
        return
    endif

    ! determine border
    a(:)=list(1,:)
    b(:)=list(2,:)



    a_wins=.true.
    do i=1,size(a)
        if(a(i)>b(i) )then
            a_wins = .true.
            exit
        elseif(a(i)<b(i) )then
            a_wins = .false.
            exit
        else
            if(i==size(a) .and. a(i)==b(i))then
                a_wins = .true.
                exit
            endif
            cycle
        endif
    enddo

    if( a_wins )then
        border(:)=a(:)
        if(size(list,1)==2 )then
            list(1,:)=b(:)
            list(2,:)=a(:)
            v1  = val(1)
            v2  = val(2)
            val(1)=v2
            val(2)=v1
            return
        endif
    else
        border(:)=b(:)
        if(size(list,1)==2 )then
            list(1,:)=a(:)
            list(2,:)=b(:)
            return
        endif
    endif

    last=scope2
    crossed=.false.


    
    do start=scope1,scope2

        list_wins=.true.
        do i=1,size(a)
            ! priority :: a(1) >a(2)>a(3)
            if(list(start,i) > border(i) )then
                list_wins = .true.
                exit
            elseif(list(start,i)<border(i) )then
                list_wins = .false.
                exit
            else
                if(i==size(a)  .and. list(start,i)==border(i) )then
                    list_wins = .true.
                    exit
                endif
                cycle
            endif
        enddo


        if(list_wins)then
            do 

                border_wins = .true.
                do i=1,size(border)
                    ! priority :: a(1) >a(2)>a(3)
                    if(list(start,i) <= border(i) )then
                        border_wins = .true.
                        exit
                    elseif(list(start,i) > border(i) )then
                        border_wins = .false.
                        exit
                    else
                        if(i==size(border) .and. list(start,i) == border(i))then
                            border_wins = .false.
                            exit
                        endif
                        cycle
                    endif
                enddo


                if(border_wins )then
                    ! exchange
                    buf(:)=list(start,:)
                    list(start,:)=list(last,:)
                    list(last,:)=buf(:)
                    v1 = val(start)
                    v2 = val(last)
                    val(start) = v2
                    val(last) = v1
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
            call quicksort(list(scope1:start,:),val(scope1:start) )
            call quicksort(list(last:scope2,:),val(last:scope2)  )

            exit
        else
            cycle
        endif

    enddo

end subroutine
!##################################################



!##################################################
recursive subroutine quicksortreal(list,val) 
    real(real64),intent(inout) :: list(:)
    real(real64),optional,intent(inout) :: val(:)
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

            buf=val(1)
            val(1)=val(2)
            val(2)=buf
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

                    buf=val(start)
                    val(start)=val(last)
                    val(last)=buf
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
            if(.not.present(val) )then
                call quicksort(list(scope1:start))
                call quicksort(list(last:scope2))
            else
                call quicksort(list(scope1:start),val(scope1:start) )
                call quicksort(list(last:scope2),val(last:scope2) )

            endif
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
subroutine heapsortArray(array,val)
    real(real64),optional,intent(inout) :: val(:)
      integer(int32),intent(inout) :: array(:,:)
    real(real64) :: t_real
      integer(int32) ::i,k,j,l,n,m,nn
      integer(int32),allocatable :: t(:)
      logical :: a_wins
  
      n=size(array,1)
      m=sizE(array,2)
      allocate(t(m) )
  
      if(n.le.0)then
          write(6,*)"Error, at heapsort"; stop
      endif
      if(n.eq.1)return
  
    l=n/2+1
    k=n
    do while(k.ne.1)
        if(l.gt.1)then
            l=l-1
            t(:)=array(L,: )
            t_real=val(L)
        else
            t(:)=array(k,:)
            t_real=val(k)
  
            array(k,:)=array(1,:)
            val(k) = val(1)
  
            k=k-1
            if(k.eq.1) then
                   array(1,:)=t(:)
                val(1) = t_real
                exit
            endif
        endif
        i=l
        j=l+l
        do while(j.le.k)
            if(j.lt.k)then
  
                  a_wins=.true.
                  do nn=1,size(array,2)
                      if(array(j,nn)<array(j+1,nn) )then
                          a_wins = .true.
                          exit
                      elseif(array(j,nn) > array(j+1,nn)  )then
                          a_wins = .false.
                          exit
                      else
                          if(nn==size(array,2) .and. array(j,nn) == array(j+1,nn)  )then
                              a_wins = .false.
                              exit
                          endif
                          cycle
                      endif
                  enddo
  
                  if( a_wins )then
                      j=j+1
                  endif
            endif
  
  
            a_wins=.true.
            do nn=1,size(array,2)
                if(t(nn)<array(j,nn) )then
                    a_wins = .true.
                    exit
                elseif(t(nn) > array(j,nn)  )then
                    a_wins = .false.
                    exit
                else
                    if(nn==size(array,2) .and. t(nn) == array(j,nn)  )then
                        a_wins = .false.
                        exit
                    endif
                    cycle
                endif
            enddo
  
            !if (t.lt.array(j))then
            if (a_wins)then
                array(i,:)=array(j,:)
                val(i)=val(j)
                i=j
                j=j+j
            else
                j=k+1
            endif
        enddo
         array(i,:)=t(:)
         val(i)=t_real
      enddo
  
  end subroutine heapsortArray
  

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
pure function existIntVec(vector,val) result(ret)
    integer(int32),intent(in) :: vector(:)
    integer(int32),intent(in) :: val
    logical :: ret
    integer(int32) :: i,j,k,n

!
    ! search 
    ret=.false.
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
function anglesReal2D(x) result(ret)
    real(real64), intent(in) :: x(2)
    type(Math_) :: math
    real(real64) :: ret,r

    r = norm(x)
    ! angle from (0,1)
    if(x(1)>=0.0d0 .and. x(2)>=0.0d0 )then
        ret = acos( x(1)/r )
    elseif(x(1)<=0.0d0 .and. x(2)>=0.0d0 )then
        ret = acos( x(1)/r )
    elseif(x(1)<=0.0d0 .and. x(2)<=0.0d0 )then
        ret = 2.0d0*math%PI - acos( x(1)/r )
    else
        ret = 2.0d0*math%PI - acos( x(1)/r )
    endif

end function
! ############################################################

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


! ############################################################
function shapeVecInt(vector) result(ret)
    integer(int32),intent(in) :: vector(:)
    integer(int32) :: ret

    ret = size(vector)
end function
! ############################################################

! ############################################################
function shapeVecReal(vector) result(ret)
    real(real64),intent(in) :: vector(:)
    integer(int32) :: ret

    ret = size(vector)
end function
! ############################################################

! ############################################################
function shapeArray2Int(vector) result(ret)
    integer(int32),intent(in) :: vector(:,:)
    integer(int32) :: ret(2)

    ret(1) = size(vector,1)
    ret(2) = size(vector,2)
    
end function
! ############################################################

! ############################################################
function shapeArray2Real(vector) result(ret)
    real(real64),intent(in) :: vector(:,:)
    integer(int32) :: ret(2)
    
    ret(1) = size(vector,1)
    ret(2) = size(vector,2)
    
end function
! ############################################################


! ############################################################
function shapeArray3Int(vector) result(ret)
    integer(int32),intent(in) :: vector(:,:,:)
    integer(int32) :: ret(3)

    ret(1) = size(vector,1)
    ret(2) = size(vector,2)
    ret(3) = size(vector,3)
    
end function
! ############################################################

! ############################################################
function shapeArray3Real(vector) result(ret)
    real(real64),intent(in) :: vector(:,:,:)
    integer(int32) :: ret(3)

    ret(1) = size(vector,1)
    ret(2) = size(vector,2)
    ret(3) = size(vector,3)
end function
! ############################################################

! ############################################################
function shapeArray4Int(vector) result(ret)
    integer(int32),intent(in) :: vector(:,:,:,:)
    integer(int32) :: ret(4)

    ret(1) = size(vector,1)
    ret(2) = size(vector,2)
    ret(3) = size(vector,3)
    ret(4) = size(vector,4)
    
end function
! ############################################################

! ############################################################
function shapeArray4Real(vector) result(ret)
    real(real64),intent(in) :: vector(:,:,:,:)
    integer(int32) :: ret(4)

    ret(1) = size(vector,1)
    ret(2) = size(vector,2)
    ret(3) = size(vector,3)
    ret(4) = size(vector,4)
end function

! ############################################################

pure function zerosRealVector(size1) result(vector)
    integer(int32),intent(in) :: size1
    real(real64),allocatable :: vector(:)

    allocate(vector(size1) )
    vector(:) = 0.0d0

end function zerosRealVector


! ############################################################
pure function zerosRealArray(size1, size2) result(array)
    integer(int32),intent(in) :: size1, size2
    real(real64),allocatable :: array(:,:)

    allocate(array(size1, size2) )
    array(:,:) = 0.0d0

end function

! ############################################################


! ############################################################
pure function zerosRealArray3(size1, size2,size3) result(array)
    integer(int32),intent(in) :: size1, size2, size3
    real(real64),allocatable :: array(:,:,:)

    allocate(array(size1, size2,size3) )
    array(:,:,:) = 0.0d0

end function

! ############################################################

! ############################################################
pure function zerosRealArray4(size1, size2,size3,size4) result(array)
    integer(int32),intent(in) :: size1, size2,size3,size4
    real(real64),allocatable :: array(:,:,:,:)

    allocate(array(size1, size2, size3, size4) )
    array(:,:, :, :) = 0.0d0

end function

! ############################################################

! ############################################################
pure function zerosRealArray5(size1, size2,size3,size4,size5) result(array)
    integer(int32),intent(in) :: size1, size2,size3,size4,size5
    real(real64),allocatable :: array(:,:,:,:,:)

    allocate(array(size1, size2,size3,size4,size5) )
    array(:,:,:,:,:) = 0.0d0

end function

! ############################################################

! ############################################################

pure function zerosRealVector_64(size1) result(vector)
    integer(int64),intent(in) :: size1
    real(real64),allocatable :: vector(:)

    allocate(vector(size1) )
    vector(:) = 0.0d0

end function zerosRealVector_64

!############################################################
pure function zerosRealArray_64(size1, size2) result(array)
    integer(int64),intent(in) :: size1, size2
    real(real64),allocatable :: array(:,:)

    allocate(array(size1, size2) )
    array(:,:) = 0.0d0

end function

! ############################################################


! ############################################################
pure function zerosRealArray3_64(size1, size2,size3) result(array)
    integer(int64),intent(in) :: size1, size2, size3
    real(real64),allocatable :: array(:,:,:)

    allocate(array(size1, size2,size3) )
    array(:,:,:) = 0.0d0

end function

! ############################################################

! ############################################################
pure function zerosRealArray4_64(size1, size2,size3,size4) result(array)
    integer(int64),intent(in) :: size1, size2,size3,size4
    real(real64),allocatable :: array(:,:,:,:)

    allocate(array(size1, size2, size3, size4) )
    array(:,:, :, :) = 0.0d0

end function

! ############################################################

! ############################################################
pure function zerosRealArray5_64(size1, size2,size3,size4,size5) result(array)
    integer(int64),intent(in) :: size1, size2,size3,size4,size5
    real(real64),allocatable :: array(:,:,:,:,:)

    allocate(array(size1, size2,size3,size4,size5) )
    array(:,:,:,:,:) = 0.0d0

end function

! ############################################################


! ############################################################

function incrementRealVector(vector) result(dvector)
    real(real64),intent(in) :: vector(:)
    real(real64),allocatable ::  dvector(:)
    integer(int32) :: i

    dvector = zeros(size(vector)-1 )

    do i=1,size(dvector)
        dvector(i) = vector(i+1) - vector(i)
    enddo

end function
! ############################################################


! ############################################################
function incrementIntVector(vector) result(dvector)
    integer(int32),intent(in) :: vector(:)
    integer(int32),allocatable ::  dvector(:)
    integer(int32) :: i

    dvector = zeros(size(vector)-1 )

    do i=1,size(dvector)
        dvector(i) = vector(i+1) - vector(i)
    enddo
    
end function
! ############################################################

! ############################################################
function incrementRealArray(matrix,column) result(dmatrix)
    real(real64),intent(in) :: matrix(:,:)
    real(real64),allocatable ::  dmatrix(:,:)
    integer(int32),intent(in) :: column
    integer(int32) :: i

    dmatrix = zeros(size(matrix,1)-1,size(matrix,2)  )

    do i=1,size(dmatrix,1)
        dmatrix(i,:) = matrix(i,:)
        dmatrix(i,column) = matrix(i+1,column) - matrix(i,column)
    enddo

end function
! ############################################################


! ############################################################
function incrementIntArray(matrix,column) result(dmatrix)
    integer(int32),intent(in) :: matrix(:,:)
    integer(int32),allocatable:: dmatrix(:,:)
    integer(int32),intent(in) :: column
    integer(int32) :: i

    dmatrix = zeros(size(matrix,1)-1,size(matrix,2)  )

    do i=1,size(dmatrix,1)
        dmatrix(i,:) = matrix(i,:)
        dmatrix(i,column) = matrix(i+1,column) - matrix(i,column)
    enddo

end function
! ############################################################


! ############################################################
! https://numpy.org/doc/stable/reference/generated/numpy.arange.html
! same as numpy.arange
function arangeRealVector(size1,stop_val,step) result(vector)
    integer(int32),intent(in) :: size1
    integer(int32),optional,intent(in) :: stop_val,step
    real(real64),allocatable :: vector(:)
    integer(int32) :: i,a_size

    if(present(stop_val) .and. present(step) )then
        a_size = stop_val - (size1 -1)
        a_size = a_size/step 
        allocate(vector(a_size) )
        vector(1) = dble(size1)
        do i=2,size(vector,1)
            vector(i) = vector(i-1) + dble(step)
        enddo
    else
        allocate(vector(size1) )
        vector(1) = 0.0d0
        do i=2,size(vector,1)
            vector(i) = vector(i-1) + 1.0d0
        enddo
    endif
end function arangeRealVector
! ############################################################


! ############################################################
function reshapeRealVector(vector,size1,size2) result(matrix)
    real(real64),intent(in) :: vector(:)
    integer(int32),intent(in) ::size1, size2
    integer(int32) :: i,j,n
    real(real64),allocatable :: matrix(:,:) 
    matrix = zeros(size1, size2) 

    n=0
    do i=1, size1
        do j=1, size2
            n=n+1
            if(n>size(vector) ) exit
            matrix(i,j) = vector(n)
        enddo
    enddo

end function
! ############################################################

! ############################################################
function reshapeIntVector(vector,size1,size2) result(matrix)
    integer(int32),intent(in) :: vector(:)
    integer(int32),intent(in) ::size1, size2
    integer(int32) :: i,j,n
    integer(int32),allocatable :: matrix(:,:) 
    matrix = zeros(size1, size2) 

    n=0
    do i=1, size1
        do j=1, size2
            n=n+1
            if(n>size(vector) ) exit
            matrix(i,j) = vector(n)
        enddo
    enddo

end function
! ############################################################




! ############################################################
subroutine zerosRealArrayArrayClass(array,size1, size2)
    class(Array_),intent(inout) :: array
    integer(int32),optional,intent(in) :: size1, size2
    integer(int32) :: n,m

    n=input(default=1,option=size1)
    m=input(default=1,option=size2)
    
    if(allocated(array%reala) ) deallocate(array%reala)

    allocate(array%reala(n,m) )
    array%reala(:,:) = 0.0d0

end subroutine
! ############################################################



! ############################################################
subroutine eyeRealArrayArrayClass(array,size1, size2)
    class(Array_),intent(inout) :: array
    integer(int32),optional,intent(in) :: size1, size2
    integer(int32) :: n,m,i

    n=input(default=1,option=size1)
    m=input(default=1,option=size2)
    
    if(allocated(array%reala) ) deallocate(array%reala)

    allocate(array%reala(n,m) )
    array%reala(:,:) = 0.0d0

    do i=1, size(array%reala,1)
        array%reala(i,i) = 1.0d0
    enddo

end subroutine
! ############################################################



! ############################################################
subroutine randomRealArrayArrayClass(array,size1, size2)
    class(Array_),intent(inout) :: array
    integer(int32),optional,intent(in) :: size1, size2
    integer(int32) :: n,m,i,j
    type(Random_) :: random

    n=input(default=1,option=size1)
    m=input(default=1,option=size2)
    
    if(allocated(array%reala) ) deallocate(array%reala)

    array%reala = random%randn(n,m)
    
end subroutine
! ############################################################

! ############################################################
subroutine printArrayClass(obj)
    class(Array_),intent(in) :: obj
    integer(int32) :: i,j

    if(allocated(obj%list) )then
        do i=1,size(obj%list,1)
            do j=1,size(obj%list,2)-1
                write(*,'(A)',advance='no') trim(obj%list(i,j)%string)// "  "
            enddo
            write(*,'(A)',advance='yes') trim(obj%list(i,size(obj%list,2))%string)
        enddo
        return
    endif
    print *, "size :: ", str(size(obj%reala,1))," x ",str(size(obj%reala,1))
    print *, " "
    do i=1,size(obj%reala,1)
        print *, obj%reala(i,:)
    enddo
    print *, " "  


end subroutine
! ############################################################

! ############################################################
function multArrayClass(x,y) result(z)
    type(Array_),intent(in) ::x,y
    type(Array_) :: z
    
    z%reala = matmul(x%reala, y%reala)
    
end function
! ############################################################



! ############################################################
function dotArrayClass(x,y) result(z)
    type(Array_),intent(in) ::x,y
    real(real64) :: z
    integer(int32) :: i,j
    
    z = 0.0d0
    do i=1,size(x%reala,1)
        do j=1,size(x%reala,2)
        z = z + x%reala(i,j) * y%reala(i,j) 
        enddo
    enddo

end function
! ############################################################

recursive subroutine unwindLineReal(x,itr_tol)
    real(real64),allocatable,intent(inout) :: x(:,:)
    real(real64),allocatable :: x_old(:,:)
    integer(int32),optional,intent(in) :: itr_tol
    integer(int32),allocatable :: cross_list(:,:)
    real(real64):: L_i(2,2),L_j(2,2),x1(2),x2(2)
    integer(int32) :: i,j,n,itr,p1,p2, q1,q2,k,return_sig,itrtol
    ! (x1,y1) --> (x2,y2) --> (x3,y3) --> (x4,y4) --> 
    ! if the path is crossing, remove the crossing

    itrtol=input(default=10000000,option=itr_tol)

    if(itrtol<=0)then
        print *, "ERROR :: unwindLineReal :: did not converge"
        return
    endif
    if(size(x,2)/=2 )then
        print *, "ERROR :: unwind >> this should be 2-D"
        return
    endif
    ! copy
    x_old = x
    allocate(cross_list(size(x,1),4 ) )
    cross_list(:,:) = 0
    itr = 0
    do i=1, size(x,1)-2
        do j=i+2,size(x,1)
            if(j==size(x,1))then
                if(i==1)then
                    ! (#1) ---> (#2) vs (#last) ---> (#1)
                    ! no crossing 
                    cycle
                else
                    L_i(1,:) = x(i,:)
                    L_j(1,:) = x(j,:)
                    L_i(2,:) = x(i+1,:)
                    L_j(2,:) = x(1,:)
                endif
            else
                L_i(1,:) = x(i,:)
                L_j(1,:) = x(j,:)
                L_i(2,:) = x(i+1,:)
                L_j(2,:) = x(j+1,:)
            endif
            if(judgeCrossing2D(L_i,L_j) )then
                ! cross!
                itr=itr+1
                cross_list(itr,1) = i 
                cross_list(itr,2) = i+1
                cross_list(itr,3) = j 
                cross_list(itr,4) = j+1
            endif
        enddo
    enddo
    ! unwind
    if(maxval(cross_list)==0 .or. itr==0)then
        return
    endif

    do i=1, size(cross_list,1)
        if(cross_list(i,1)==0 ) then
            cycle
        else
            p1 = cross_list(i,1)
            p2 = cross_list(i,2)
            q1 = cross_list(i,3)
            q2 = cross_list(i,4)
            do j = p2, (q1+p2)/2
                k = j-p1
                if(j==q2-k)then
                    cycle
                endif
                print *, "flip #"//str(j)//" and #"//str(q2-k)
                
                x1(:) = x(j,:)
                x2(:) = x(q2-k,:)
                ! exchanges
                x(j,:) = x2(:)
                x(q2-k,:) = x1(:)
            enddo
            cross_list(i,:) = 0
        endif
    enddo
    itrtol=itrtol-1
    call unwindline(x,itrtol)
end subroutine
! ############################################################

! ############################################################
function judgeCrossing2D(L1,L2) result(cross)
    real(real64),intent(in) :: L1(2,2),L2(2,2)
    real(real64) :: a, b, c1, c2
    logical :: cross_1to2, cross_2to1, cross

    ! only for 2-D
    
    ! Line #1 : y = a x + b
    ! a = (y2-y1)/(x2-x1)
    a = ( L1(2,2) - L1(1,2) )/( L1(2,1) - L1(1,1) )
    ! b = y1 - a x1
    b = L1(1,2) - a * L1(1,1)

    ! check Line #2
    ! For Line #2, c1 = y1 -ax1 -b 
    ! For Line #2, c2 = y2 -ax2 -b
    c1 = L2(1,2) - a * L2(1,1) - b
    c2 = L2(2,2) - a * L2(2,1) - b

    if(c1*c2 <0.0d0 )then
        cross_1to2 = .True.
    else
        cross_1to2 = .False.
    endif

    ! 
    a = ( L2(2,2) - L2(1,2) )/( L2(2,1) - L2(1,1) )
    ! b = y1 - a x1
    b = L2(1,2) - a * L2(1,1)

    ! check Line #2
    ! For Line #2, c1 = y1 -ax1 -b 
    ! For Line #2, c2 = y2 -ax2 -b
    c1 = L1(1,2) - a * L1(1,1) - b
    c2 = L1(2,2) - a * L1(2,1) - b

    if(c1*c2 <=0.0d0 )then
        cross_2to1 = .True.
    else
        cross_2to1 = .False.
    endif

    if( cross_2to1 .and. cross_1to2)then
        cross = .true.
    else
        cross = .false.
    endif

end function
! ############################################################

function sameAsGroupintVec(vec1, vec2) result(ret)
    integer(int32),intent(in) :: vec1(:)
    integer(int32),intent(in) :: vec2(:)
    logical :: sameValueExists
    integer(int32) :: i,j
    logical :: ret

    if(size(vec1)/=size(vec2) )then
        ret = .false.
        return
    endif


    if(maxval(vec1)/=maxval(vec2) .or. minval(vec1)/=minval(vec2) ) then
        ret = .false.
        return
    endif
    
    if(sum(vec1)/=sum(vec2) ) then
        ret = .false.
        return
    endif



    
    do i=1,size(vec1)
        sameValueExists = .false.
        do j=1,size(vec2)
            if(vec1(i)==vec2(j) )then
                sameValueExists = .true.
                exit
            endif
        enddo
        if(sameValueExists)then
            cycle
        else
            ret = .false.
            return
        endif
    enddo

    ret = .true.


end function

subroutine searchAndRemoveInt(vec,eq,leq,geq)
    integer(int32),allocatable,intent(inout) :: vec(:)
    integer(int32),optional,intent(in) :: eq,leq,geq
    integer(int32),allocatable :: buf(:)
    integer(int32):: countnum,i,k

    if(present(eq) )then
        countnum=0
        do i=1,size(vec)
            if(vec(i)==eq )then
                countnum = countnum + 1
            endif
        enddo

        k = size(vec) - countnum
        allocate(buf( k ) )
        countnum=0
        do i=1,size(vec)
            if(vec(i) /= eq )then
                countnum=countnum+1
                buf(countnum) = vec(i)
            endif
        enddo
        vec = buf
    endif


    if(present(leq) )then
        countnum=0
        do i=1,size(vec)
            if(vec(i)<=leq )then
                countnum = countnum + 1
            endif
        enddo

        k = size(vec) - countnum
        allocate(buf( k ) )
        countnum=0
        do i=1,size(vec)
            if(vec(i) > leq )then
                countnum=countnum+1
                buf(countnum) = vec(i)
            endif
        enddo
        vec = buf
    endif

    if(present(geq) )then
        countnum=0
        do i=1,size(vec)
            if(vec(i)>=geq )then
                countnum = countnum + 1
            endif
        enddo

        k = size(vec) - countnum
        allocate(buf( k ) )
        countnum=0
        do i=1,size(vec)
            if(vec(i) < geq )then
                countnum=countnum+1
                buf(countnum) = vec(i)
            endif
        enddo
        vec = buf
    endif

end subroutine

function dot_product_omp(a, b, omp) result(dp)
    real(real64),intent(in) :: a(:),b(:)
    real(real64) :: dp
    integer(int32) :: i
    logical,intent(in) :: omp

    if(omp)then
        dp = 0.0d0
        !$omp parallel do reduction(+:dp)
        do i=1,size(a)
            dp = dp + a(i)*b(i)
        enddo
        !$omp end parallel do
    else
        dp =dot_product(a,b)
    endif
end function


! ##############################################################
function hstackInt32Vector2(Vec1, vec2) result(ret)
    integer(int32),allocatable,intent(in) :: vec1(:),vec2(:)
    integer(int32),allocatable :: ret(:)

    if(.not.allocated(vec2).and. .not.allocated(vec1) )then
        return
    endif

    if(.not.allocated(vec1) )then
        ret = vec2
        return
    endif


    if(.not.allocated(vec2) )then
        ret = vec1
        return
    endif

    if( size(vec1)==0 .and. size(vec2)==0 )then
        return
    endif


    if( size(vec1)==0  )then
        ret = vec2
        return
    endif

    if( size(vec2)==0  )then
        ret = vec1
        return
    endif
    
    allocate(ret(  size(vec1) + size(vec2) ) )
    ret(1:size(vec1) ) = vec1(:)
    ret(size(vec1)+1: ) = vec2(:)

end function
! ##############################################################

! ##############################################################
function hstackInt32Vector3(vec1, vec2,vec3) result(ret)
    integer(int32),allocatable,intent(in) :: vec1(:),vec2(:),vec3(:)
    integer(int32),allocatable :: ret(:)

    if(.not.allocated(vec2).and. .not.allocated(vec1) )then
        if( .not.allocated(vec3) )then
            return
        endif
    endif

    if(.not.allocated(vec1) )then
        ret =  hstackInt32Vector2(vec2, vec3)
        return
    endif


    if(.not.allocated(vec2) )then
        ret =  hstackInt32Vector2(vec1, vec3)
        return
    endif

    if(.not.allocated(vec3) )then
        ret =  hstackInt32Vector2(vec1, vec2)
        return
    endif

    allocate(ret(  size(vec1) + size(vec2)+ size(vec3) ) )
    ret(           1:size(vec1)            ) = vec1(:)
    ret(size(vec1)+1:size(vec1)+size(vec2) ) = vec2(:)
    ret(size(vec1)+size(vec2)+1:           ) = vec3(:)

end function
! ##############################################################


! ##############################################################
function hstackreal64Vector2(Vec1, vec2) result(ret)
    real(real64),allocatable,intent(in) :: vec1(:),vec2(:)
    real(real64),allocatable :: ret(:)


    if(.not.allocated(vec2).and. .not.allocated(vec1) )then
        return
    endif

    if(.not.allocated(vec1) )then
        ret = vec2
        return
    endif


    if(.not.allocated(vec2) )then
        ret = vec1
        return
    endif

    allocate(ret(  size(vec1) + size(vec2) ) )
    ret(1:size(vec1) ) = vec1(:)
    ret(size(vec1)+1: ) = vec2(:)

end function
! ##############################################################

! ##############################################################
function hstackreal64Vector3(vec1, vec2,vec3) result(ret)
    real(real64),allocatable,intent(in) :: vec1(:),vec2(:),vec3(:)
    real(real64),allocatable :: ret(:)

    if(.not.allocated(vec2).and. .not.allocated(vec1) )then
        if( .not.allocated(vec3) )then
            return
        endif
    endif

    if(.not.allocated(vec1) )then
        ret =  hstackreal64Vector2(vec2, vec3)
        return
    endif


    if(.not.allocated(vec2) )then
        ret =  hstackreal64Vector2(vec1, vec3)
        return
    endif

    if(.not.allocated(vec3) )then
        ret =  hstackreal64Vector2(vec1, vec2)
        return
    endif

    allocate(ret(  size(vec1) + size(vec2)+ size(vec3) ) )
    ret(           1:size(vec1)            ) = vec1(:)
    ret(size(vec1)+1:size(vec1)+size(vec2) ) = vec2(:)
    ret(size(vec1)+size(vec2)+1:           ) = vec3(:)

end function
! ##############################################################


! ##############################################################
function farthestVectorReal64(array,vector) result(ret)
    real(real64),intent(in) :: array(:,:),vector(:)
    real(real64),allocatable :: ret(:),trial(:)
    real(real64) :: dp,dp_tr
    integer(int32) :: i, n

    n = size(array,2)
    allocate(ret(n) )

    ret(:) = array(1,:)
    dp = dot_product(vector-ret,vector-ret )
    do i=2,size(array,1)
        dp_tr = dot_product( array(i,:) -vector(:), array(i,:) -vector(:) )
        if(dp_tr > dp)then
            ret(:) = array(i,:)
            dp = dot_product(vector-ret,vector-ret )
        endif
    enddo

end function
! ##############################################################

function rotationMatrixReal64(rotation_angle1, rotation_angle2) result(ret)
    real(real64),optional,intent(in) :: rotation_angle1,rotation_angle2
    real(real64),allocatable :: ret(:,:)

    if(present(rotation_angle1) .and. .not. present(rotation_angle2)  )then
        ! 2D
        allocate(ret(2,2) )
        ret(1,1) = cos(rotation_angle1)
        ret(2,1) = sin(rotation_angle1)
        ret(1,2) = -sin(rotation_angle1)
        ret(2,2) = cos(rotation_angle1)
    elseif(present(rotation_angle1) .and. present(rotation_angle2) )then
        print *, "Now implementing!"
        stop
    endif

end function


function averageInt32(vec) result(ret)
    integer(Int32),intent(in) :: vec(:)
    integer(Int32) :: ret

    ret = sum(vec)/size(vec)
end function

function averageReal64(vec) result(ret)
    real(Real64),intent(in) :: vec(:)
    real(Real64) :: ret

    ret = sum(vec)/dble(size(vec))

end function


! ###############################################################
function interpolateOneReal64(x, Fx, x_value) result(ret)
    real(real64),intent(inout) :: Fx(:), x(:)
    real(real64),intent(in):: x_value
    real(real64) :: ret,alpha,x1,x2,Fx1,Fx2
    integer(int32) :: i,n,id
    
    ! express F(x_value) by 
    ! Linear interpolation by discreted space (x_i, F(x_i) )
    n = size(x)
    
    call heapsort(n,x,Fx)
    
    id = SearchNearestValueID(dble(x),dble(x_value))
    if( x_value > x(id) )then
        if(id == n)then
            ret = Fx(n)
            return
        else
            x1 = x(id)
            x2 = x(id+1)
            Fx1 = Fx(id)
            Fx2 = Fx(id+1)
        endif
    else
        if(id == 1)then
            ret = Fx(1)
            return
        else
            x1 = x(id-1)
            x2 = x(id)
            Fx1 = Fx(id-1)
            Fx2 = Fx(id)
        endif
    endif
    alpha = (x_value - x2)/(x1- x2)

    ret = alpha*Fx1 + (1.0d0 - alpha)*Fx2

end function
! ###############################################################



! ###############################################################
function interpolateOneReal32(x, Fx, x_value) result(ret)
    real(real32),intent(inout) :: Fx(:), x(:)
    real(real32),intent(in):: x_value
    real(real32) :: ret,alpha,x1,x2,Fx1,Fx2
    integer(int32) :: i,n,id
    
    ! express F(x_value) by 
    ! Linear interpolation by discreted space (x_i, F(x_i) )
    n = size(x)
    
    call heapsort(n,x,Fx)
    
    id = SearchNearestValueID(dble(x),dble(x_value))
    if( x_value > x(id) )then
        if(id == n)then
            ret = Fx(n)
            return
        else
            x1 = x(id)
            x2 = x(id+1)
            Fx1 = Fx(id)
            Fx2 = Fx(id+1)
        endif
    else
        if(id == 1)then
            ret = Fx(1)
            return
        else
            x1 = x(id-1)
            x2 = x(id)
            Fx1 = Fx(id-1)
            Fx2 = Fx(id)
        endif
    endif
    alpha = (x_value - x2)/(x1- x2)

    ret = alpha*Fx1 + (1.0d0 - alpha)*Fx2

end function
! ###############################################################



! ###############################################################
function interpolateOnecomplex64(x_c, Fx_c, x_value_c) result(ret)
    complex(complex64),intent(inout) :: Fx_c(:), x_c(:)
    real(real64),allocatable :: Fx(:), x(:)
    complex(complex64),intent(in):: x_value_c
    complex(complex64) :: ret,alpha,x1,x2,Fx1,Fx2
    real(real64) :: x_value
    integer(int32) :: i,n,id

    Fx = dble(Fx_c)
    x = dble(x_c)
    x_value = dble(x_value_c)
    
    ! express F(x_value) by 
    ! Linear interpolation by discreted space (x_i, F(x_i) )
    n = size(x)
    call heapsort(n,x,Fx)
    
    id = SearchNearestValueID(dble(x),dble(x_value))
    if( x_value > x(id) )then
        if(id == n)then
            ret = Fx(n)
            return
        else
            x1 = x(id)
            x2 = x(id+1)
            Fx1 = Fx(id)
            Fx2 = Fx(id+1)
        endif
    else
        if(id == 1)then
            ret = Fx(1)
            return
        else
            x1 = x(id-1)
            x2 = x(id)
            Fx1 = Fx(id-1)
            Fx2 = Fx(id)
        endif
    endif
    alpha = (x_value - x2)/(x1- x2)

    ret = alpha*Fx1 + (1.0d0 - alpha)*Fx2

    Fx_c = Fx
    x_c = x
end function
! ###############################################################


! ###############################################################
function correlation(x_t,x_s) result(cor)
    real(real64),intent(in) :: x_t(:), x_s(:)
    real(real64) :: cor
    integer(int32) :: i

    cor = 0.0d0
    do i=1,size(x_t)
        cor = cor + x_t(i)*x_s(i)   
    enddo
    cor = cor / dble(size(x_t) )

end function
! ###############################################################

function variance(x) result(ret)
    real(real64),intent(in) :: x(:)
    real(real64) :: ret,x_ave
    integer(int32) :: i

    ret = 0.0d0
    x_ave = average(x)
    do i=1,size(x)
        ret = ret + (x(i) - x_ave)*(x(i) - x_ave)
    enddo
    ret = ret/dble( size(x) )
end function

! ###############################################################
function standardDeviation(x) result(ret)
    real(real64),intent(in) :: x(:)
    real(real64) :: ret

    ret = sqrt(variance(x) )

end function
! ###############################################################

! ###############################################################
function correlationCoefficient(x_t,x_s) result(corc)
    real(real64),intent(in) :: x_t(:), x_s(:)
    real(real64) :: corc,covc,sigma_t,sigma_s
    integer(int32) :: i
    
    sigma_t = standardDeviation(x_t)
    sigma_s = standardDeviation(x_s)
    covc = covariance(x_t,x_s)
    corc= covc/sigma_t/sigma_s

end function
! ###############################################################



! ###############################################################
function covariance(x_t,x_s) result(cov)
    real(real64),intent(in) :: x_t(:), x_s(:)
    real(real64) :: cov,x_t_ave,x_s_ave
    integer(int32) :: i

    cov = 0.0d0
    x_t_ave = average(x_t)
    x_s_ave = average(x_s)
    do i=1,size(x_t)
        cov = cov + (x_t(i)-x_t_ave)*(x_s(i) -x_s_ave)
    enddo
    cov = cov / dble(size(x_t) )

end function
! ###############################################################



! ###############################################################
function averageVector(x_t,n) result(ret)
    real(real64),intent(in) :: x_t(:,:)
    integer(int32),intent(in)::n ! dimension of vector
    real(real64) :: ret(n),x_t_ave,x_s_ave
    integer(int32) :: i, j

    ! for vector process
    ! 1st column :: dimension
    ! 2nd column :: time
    ret(:)=0.0d0
    if(size(x_t,1)==n )then
        do i=1,size(x_t,2)!num of sample
            do j=1,n !dim_num
                ret(j) = ret(j) + x_t(j,i)
            enddo
        enddo
        ret(:) = 1.0d0/size(x_t,2)*ret(:)

    elseif(size(x_t,2)==n )then
        do i=1,size(x_t,1) !num of sample
            do j=1,n !dim_num
                ret(j) = ret(j) + x_t(i,j)
            enddo
        enddo
        ret(:) = 1.0d0/size(x_t,1)*ret(:)
        
    else
        print *, "ERROR :: arrayclass >> invalid dimension size x_t :: size1 or size 2 should be = n"
        stop
    endif

end function
! ###############################################################

! ###############################################################
function covarianceMatrix(x_t,x_s,n) result(ret)
    real(real64),intent(in) :: x_t(:,:), x_s(:,:)
    integer(int32),intent(in)::n ! dimension of vector
    real(real64) :: ret(n,n),x_t_ave,x_s_ave
    integer(int32) :: i,j


    ! for vector process
    ! 1st column :: dimension
    ! 2nd column :: time
    if(size(x_t,1)==n )then
        do i=1,n
            do j=1,n
                ret(i,j) =  covariance(x_t(i,:),x_s(j,:))
            enddo
        enddo
    elseif(size(x_t,2)==n )then
        do i=1,n
            do j=1,n
                ret(i,j) =  covariance(x_t(:,i),x_s(:,j))
            enddo
        enddo
    else
        print *, "ERROR :: arrayclass >> invalid dimension size x_{t,s}:: size1 or size 2 should be = n"
        stop
    endif

end function
! ###############################################################

! ###############################################################
pure function linspace1D(drange,numberOfData) result(ret)
    real(real64),intent(in) :: drange(2)
    integer(int32),intent(in) :: numberOfData
    real(real64) :: dx,x,from,to
    real(real64),allocatable :: ret(:)
    integer(int32) :: i
    
    allocate(ret(numberOfData) )
    from = drange(1)
    to = drange(2)
    dx = (to - from)/dble(numberOfData-1)
    x = from
    do i=1, numberOfData-1
        ret(i) = x
        x = x + dx
    enddo
    ret(numberOfData) = x
end function
! ###############################################################




! ###############################################################
pure function linspace1Dcomplex64(drange,numberOfData) result(ret)
    complex(complex64),intent(in) :: drange(2)
    integer(int32),intent(in) :: numberOfData
    complex(complex64) :: dx,x,from,to
    complex(complex64),allocatable :: ret(:)
    integer(int32) :: i
    
    allocate(ret(numberOfData) )
    from = drange(1)
    to = drange(2)
    dx = (to - from)/dble(numberOfData-1)
    x = from
    do i=1, numberOfData-1
        ret(i) = x
        x = x + dx
    enddo
    ret(numberOfData) = x
end function
! ###############################################################

! ###############################################################
pure function linspace1Dreal32(drange,numberOfData) result(ret)
    real(real32),intent(in) :: drange(2)
    integer(int32),intent(in) :: numberOfData
    real(real32) :: dx,x,from,to
    real(real32),allocatable :: ret(:)
    integer(int32) :: i
    
    allocate(ret(numberOfData) )
    from = drange(1)
    to = drange(2)
    dx = (to - from)/dble(numberOfData-1)
    x = from
    do i=1, numberOfData-1
        ret(i) = x
        x = x + dx
    enddo
    ret(numberOfData) = x
end function
! ###############################################################



! ###############################################################

pure function linspace2D(xrange,yrange,xnum,ynum) result(ret)
    real(real64),intent(in) :: xrange(2),yrange(2)
    integer(int32),intent(in) :: xnum,ynum
    real(real64),allocatable :: ret(:,:),x(:),y(:)
    integer(int32) :: i,j,k,l,n
    
    allocate(ret(xnum*ynum,2) )
    x = linspace1D(xrange,xnum)
    y = linspace1D(yrange,ynum)
    n = 0
    do i=1,xnum
        do j=1,ynum
            n=n+1
            ret(n,1) = x(i) 
            ret(n,2) = y(j)
        enddo
    enddo
    
end function
! ###############################################################

! ###############################################################

pure function linspace3D(xrange,yrange,zrange,xnum,ynum,znum) result(ret)
    real(real64),intent(in) :: xrange(2),yrange(2),zrange(2)
    integer(int32),intent(in) :: xnum,ynum,znum
    real(real64),allocatable :: ret(:,:),x(:),y(:),z(:)
    integer(int32) :: i,j,k,l,n
    
    allocate(ret(xnum*ynum*znum,3) )
    x = linspace1D(xrange,xnum)
    y = linspace1D(yrange,ynum)
    z = linspace1D(zrange,znum)
    n = 0
    do i=1,xnum
        do j=1,ynum
            do k=1,znum
                n=n+1
                ret(n,1) = x(i) 
                ret(n,2) = y(j) 
                ret(n,3) = z(k)         
            enddo
        enddo
    enddo
    
end function
! ###############################################################

! ###############################################################

pure function linspace4D(xrange,yrange,zrange,trange,xnum,ynum,znum,tnum) result(ret)
    real(real64),intent(in) :: xrange(2),yrange(2),zrange(2),trange(2)
    integer(int32),intent(in) :: xnum,ynum,znum,tnum
    real(real64),allocatable :: ret(:,:),x(:),y(:),z(:),t(:)
    integer(int32) :: i,j,k,l,n
    
    allocate(ret(xnum*ynum*znum*tnum,4) )
    x = linspace1D(xrange,xnum)
    y = linspace1D(yrange,ynum)
    z = linspace1D(zrange,znum)
    t = linspace1D(trange,tnum)
    n = 0
    do i=1,xnum
        do j=1,ynum
            do k=1,znum
               do l=1,tnum
                    n=n+1
                    ret(n,1) = x(i) 
                    ret(n,2) = y(j) 
                    ret(n,3) = z(k) 
                    ret(n,4) = t(l) 
                enddo
            enddo
        enddo
    enddo
    
end function
! ###############################################################

! ###############################################################
function convolveReal64(f,g) result(ret)
    real(real64),intent(in) :: f(:),g(:)
    real(real64),allocatable :: ret(:)
    integer(int32) :: tau,t
    ! ret(\tau) = \Sigma f(t)g(\tau-t)
    if(size(f)/=size(g) )then
        print *, "ERROR: convolution"
        stop
    endif
    ret = zeros(size(f)*2 )
    
    !$OMP parallel do private(t)
    do t=1,size(ret)
        do tau=1,size(f)
            if(t-tau<1 .or. t-tau>size(g)) cycle
            ret(t) = ret(t) + f(tau)*g( t - tau )
        enddo
    enddo
    !$OMP end parallel do
end function
! ###############################################################


! ###############################################################
function convolveComplex64(f,g) result(ret)
    complex(complex64),intent(in) :: f(:),g(:)
    complex(complex64),allocatable :: ret(:)
    integer(int32) :: tau,t
    ! ret(\tau) = \Sigma f(t)g(\tau-t)
    if(size(f)/=size(g) )then
        print *, "ERROR: convolution"
        stop
    endif
    ret = zeros(size(f)*2 )
    
    !$OMP parallel do private(t)
    do t=1,size(ret)
        do tau=1,size(f)
            if(t-tau<1 .or. t-tau>size(g)) cycle
            ret(t) = ret(t) + f(tau)*g( t - tau )
        enddo
    enddo
    !$OMP end parallel do
end function
! ###############################################################



! ###############################################################
function windowingReal64(f,g) result(ret)
    real(real64),intent(in) :: f(:),g(:)
    real(real64),allocatable :: ret(:)
    integer(int32) :: tau,t
    ! ret(\tau) = \Sigma f(t)g(\tau-t)
    if(size(f)/=size(g) )then
        print *, "ERROR: convolution"
        stop
    endif
    ret = zeros(size(f))
    !$OMP parallel do private(t)
    do t=1,size(ret)
        ret(t) = f(t)*g(t)
    enddo
    !$OMP end parallel do
end function
! ###############################################################


! ###############################################################
function windowingComplex64(f,g) result(ret)
    complex(complex64),intent(in) :: f(:),g(:)
    complex(complex64),allocatable :: ret(:)
    integer(int32) :: tau,t

    ! ret(\tau) = \Sigma f(t)g(\tau-t)
    if(size(f)/=size(g) )then
        print *, "ERROR: convolution"
        stop
    endif
    ret = zeros(size(f) )
    
    !$OMP parallel do private(t)
    do t=1,size(ret)
        ret(t) = f(t)*g(t)
    enddo
    !$OMP end parallel do
end function
! ###############################################################




! ###############################################################
function EigenValueJacobiMethod(A,x,tol) result(lambda)
    real(real64),intent(inout) :: A(:,:)
    real(real64),allocatable :: lambda(:)
    real(real64),optional,allocatable,intent(inout) :: x(:,:) ! Eigen Vector
    real(real64),optional,intent(in) :: tol
    real(real64),allocatable :: Ak(:,:),apj(:),aqj(:),aip(:),aiq(:),Gk(:,:)
    real(real64)::apq_tr,theta,tan2theta,app,aqq,apq,loop_tol
    integer(int32) :: n,p,q,i,j
    logical :: convergence = .false.

    if(present(tol) )then
        loop_tol = tol
    else
        loop_tol = 1.0e-14
    endif
    n = size(A,1)

    lambda = zeros(n)
    
    apj = zeros(n)
    aqj = zeros(n) 
    aip = zeros(n) 
    aiq = zeros(n) 

    if(present(x) )then
        Gk = zeros(n,n)
        x = zeros(n,n)
        do i=1,n
            Gk(i,i)=1.0d0
            x(i,i)=1.0d0
        enddo
        print *, "Eigen Value & Eigen Vector"
    endif
    
    Ak = A
    ! get eigen vector and eigen values
    ! by Jacobi Method
    do 
        ! find maxval

        apq_tr = 0.0d0
        p=0
        q=0
        do i=1,size(Ak,1)
            do j=1,size(Ak,2)
                if( abs(Ak(i,j))>abs(apq_tr) .and. i/=j )then
                    p = i
                    q = j
                    apq_tr = Ak(i,j)
                endif
            enddo
        enddo

        print *, p,q,apq_tr
        if(abs(apq_tr)<= loop_tol)then
            exit
        endif

        if(p*q==0)then
            print *, "ERROR :: JacobiMethod >> q*p =0"
            return
        endif


        tan2theta = - 2.0d0*Ak(p,q)/( Ak(p,p) - Ak(q,q) )
        theta = 0.50d0*atan(tan2theta)
        !theta = 0.50d0*acos(sqrt(1.0d0/(1.0d0+tan2theta*tan2theta) ) )

        apj(:) = Ak(p,:)*cos(theta) - Ak(q,:)*sin(theta)
        aqj(:) = Ak(p,:)*sin(theta) + Ak(q,:)*cos(theta)
        aip(:) = Ak(:,p)*cos(theta) - Ak(:,q)*sin(theta) 
        aiq(:) = Ak(:,p)*sin(theta) + Ak(:,q)*cos(theta) 
        app = Ak(p,p)*cos(theta)*cos(theta) + Ak(q,q)*sin(theta)*sin(theta)&
            - 2.0d0*Ak(p,q)*cos(theta)*sin(theta)
        aqq = Ak(q,q)*cos(theta)*cos(theta) + Ak(p,p)*sin(theta)*sin(theta)&
            + 2.0d0*Ak(p,q)*cos(theta)*sin(theta)
        !apq = 0.50d0*(Ak(p,p) - Ak(q,q) )*sin(2.0d0*theta) + Ak(p,q)*cos(2.0d0*theta)

        Ak(p,:) = apj(:)
        Ak(q,:) = aqj(:)
        Ak(:,p) = aip(:)
        Ak(:,q) = aiq(:)
        Ak(p,p) = app
        Ak(q,q) = aqq
        Ak(p,q) = 0.0d0
        Ak(q,p) = 0.0d0


        ! use Gk matrix
        if(present(x) )then
            do i=1,n
                Gk(i,i) = 1.0d0
            enddo
            Gk(p,p) = cos(theta)
            Gk(p,q) = sin(theta)
            Gk(q,p) = -sin(theta)
            Gk(q,q) = cos(theta)
            X = matmul(X,Gk)
        endif

    enddo

    do i=1,n
        lambda(i) = Ak(i,i)
    enddo

end function
! ###############################################################
function eigenValue(A,tol,ignore_caution) result(lambda)
    real(real64),intent(inout) :: A(:,:)
    real(real64),allocatable :: lambda(:)
    real(real64),optional,intent(in) :: tol
    logical,optional,intent(in) :: ignore_caution

    if(symmetric(A) )then
        lambda = EigenValueJacobiMethod(A,tol=tol)
    else
        print *, "skew-symmetric [A] will be implemented."
        if(present(ignore_caution) )then
            if(ignore_caution)then
                print *, "ignore_caution is true"
                lambda = EigenValueJacobiMethod(A,tol=tol)
            endif
        endif
        stop
    endif

end function
! ###############################################################

! ###############################################################
function eigenValueCOO(val,indexI,indexJ,tol,ignore_caution) result(lambda)
    real(real64),intent(in) :: val(:),indexI(:),indexJ(:)
    real(real64),allocatable :: lambda(:)
    real(real64),optional,intent(in) :: tol
    logical,optional,intent(in) :: ignore_caution

    print *, "eigenValueCOO is not implemented yet."
    !if( )then
    !    lambda = EigenValueJacobiMethodCOO(val,indexI,indexJ,tol=tol)
    !else
    !    print *, "skew-symmetric [A] will be implemented."
    !    if(present(ignore_caution) )then
    !        if(ignore_caution)then
    !            print *, "ignore_caution is true"
    !            lambda = EigenValueJacobiMethod(A,tol=tol)
    !        endif
    !    endif
    !    stop
    !endif

end function
! ###############################################################


! ###############################################################
function EigenValueJacobiMethodCOO(val,indexI,indexJ,x,tol) result(lambda)
    real(real64),intent(in) :: val(:),indexI(:),indexJ(:)
    real(real64),allocatable :: lambda(:)
    real(real64),optional,allocatable,intent(inout) :: x(:,:) ! Eigen Vector
    real(real64),optional,intent(in) :: tol
    real(real64),allocatable :: Ak(:),apj(:),aqj(:),aip(:),aiq(:),Gk(:,:)
    real(real64)::apq_tr,theta,tan2theta,app,aqq,apq,loop_tol,akpp,akqq
    integer(int32) :: n,p,q,i,j
    logical :: convergence = .false.
!
!    if(present(tol) )then
!        loop_tol = tol
!    else
!        loop_tol = 1.0e-14
!    endif
!    n = size(A,1)
!
!    lambda = zeros(n)
!    
!    apj = zeros(n)
!    aqj = zeros(n) 
!    aip = zeros(n) 
!    aiq = zeros(n) 
!
!    if(present(x) )then
!        Gk = zeros(n,n)
!        x = zeros(n,n)
!        do i=1,n
!            Gk(i,i)=1.0d0
!            x(i,i)=1.0d0
!        enddo
!        print *, "Eigen Value & Eigen Vector"
!    endif
!    
!    Ak = val
!    ! get eigen vector and eigen values
!    ! by Jacobi Method
!    do 
!        ! find maxval
!
!        apq_tr = 0.0d0
!        p=0
!        q=0
!        ! find
!        do i=1,size(Ak,1)
!            if( abs(Ak(i))>abs(apq_tr) .and. indexI(i)/=indexJ(i) )then
!                p = indexI(i)
!                q = indexJ(i)
!                apq_tr = Ak(i)
!            endif
!        enddo
!
!        print *, p,q,apq_tr
!        stop
!
!        if(abs(apq_tr)<= loop_tol)then
!            exit
!        endif
!
!        if(p*q==0)then
!            print *, "ERROR :: JacobiMethod >> q*p =0"
!            return
!        endif
!
!        akpp=0.0d0
!        do i=1,size(indexI)
!            if(indexI(i)==p .and.indexJ(i)==q  )then
!                akpp = val(i)        
!                exit
!            endif
!        enddo
!        akqq=0.0d0
!        do i=1,size(indexI)
!            if(indexI(i)==p .and.indexJ(i)==q  )then
!                akqq = val(i)        
!                exit
!            endif
!        enddo
!        
!        tan2theta = - 2.0d0*Ak(i)/( Akpp - Akqq )
!
!        theta = 0.50d0*atan(tan2theta)
!        !theta = 0.50d0*acos(sqrt(1.0d0/(1.0d0+tan2theta*tan2theta) ) )
!        
!        do i=1,size(indexI)
!            if(indexI(i)==p  )then
!                
!                exit
!            endif
!        enddo
!
!        apj(:) = Ak(p,:)*cos(theta) - Ak(q,:)*sin(theta)
!
!        aqj(:) = Ak(p,:)*sin(theta) + Ak(q,:)*cos(theta)
!        aip(:) = Ak(:,p)*cos(theta) - Ak(:,q)*sin(theta) 
!        aiq(:) = Ak(:,p)*sin(theta) + Ak(:,q)*cos(theta) 
!        app = Ak(p,p)*cos(theta)*cos(theta) + Ak(q,q)*sin(theta)*sin(theta)&
!            - 2.0d0*Ak(p,q)*cos(theta)*sin(theta)
!        aqq = Ak(q,q)*cos(theta)*cos(theta) + Ak(p,p)*sin(theta)*sin(theta)&
!            + 2.0d0*Ak(p,q)*cos(theta)*sin(theta)
!        !apq = 0.50d0*(Ak(p,p) - Ak(q,q) )*sin(2.0d0*theta) + Ak(p,q)*cos(2.0d0*theta)
!
!        Ak(p,:) = apj(:)
!        Ak(q,:) = aqj(:)
!        Ak(:,p) = aip(:)
!        Ak(:,q) = aiq(:)
!        Ak(p,p) = app
!        Ak(q,q) = aqq
!        Ak(p,q) = 0.0d0
!        Ak(q,p) = 0.0d0
!
!
!        ! use Gk matrix
!        if(present(x) )then
!            do i=1,n
!                Gk(i,i) = 1.0d0
!            enddo
!            Gk(p,p) = cos(theta)
!            Gk(p,q) = sin(theta)
!            Gk(q,p) = -sin(theta)
!            Gk(q,q) = cos(theta)
!            X = matmul(X,Gk)
!        endif
!
!    enddo
!
!    do i=1,n
!        lambda(i) = Ak(i,i)
!    enddo

end function
! ###############################################################
subroutine eigenValueAndVector(A,lambda,x,tol) 
    real(real64),intent(inout) :: A(:,:)
    real(real64),allocatable,intent(out) :: lambda(:),x(:,:)
    real(real64),optional,intent(in) :: tol

    if(symmetric(A) )then
        lambda = EigenValueJacobiMethod(A,x=x,tol=tol)
    else
        print *, "skew-symmetric [A] will be implemented."
        stop
    endif

end subroutine
! ###############################################################

!function eigenVector(A,lambda,tol) result(x)
!    real(real64),intent(inout) :: A(:,:)
!    real(real64),allocatable :: x(:,:), lambda_vals(:)
!    real(real64),optional,intent(in) :: lambda(:),tol
!    integer(int32) :: i,n
!
!    n = size(A,1)
!    x = zeros(n)
!    if(symmetric(A) )then
!        if(present(lambda) )then
!            lambda_vals = lambda
!        else
!            lambda_val = eigenValue(A,tol)
!        endif
!        do i=1,n
!            ! for i-th eigen vector
!            x(:,i) =  
!        enddo
!    else
!        print *, "skew-symmetric [A] will be implemented."
!        stop
!    endif
!
!end function
! ###############################################################

function symmetric(A) result(ret)
    real(real64),intent(in) :: A(:,:)
    integer(int32) :: i,j
    logical :: ret

    ret = .true.
    if(size(A,1)/=size(A,2) )then
        ret = .false.
        return
    endif
    do i=1,size(A,1)-1
        do j=i+1,size(A,2)
            if(A(i,j)/=A(j,i))then
                ret = .false.
                return
            endif
        enddo
    enddo

end function
! ###############################################################

! ###############################################################
function d_dx_real64(x,fx) result(d_df)
    real(real64),intent(in) :: x(:),fx(:)
    real(real64),allocatable :: d_df(:)
    !complex(complex64),allocatable :: fx_(:),fw(:),w(:),jwFw(:)
    !complex(complex64) :: j = (0.0d0, 1.0d0)
    integer(int32) :: i,n
    real(real64) :: h

    ! 2nd-order central differential
    d_df = zeros(size(x) )
    n = size(x)
    
    d_df(1) = 0.50d0*(fx(3) - fx(2) )/(x(3)-x(2) )+0.50d0*(fx(2) - fx(1) )/(x(2)-x(1) )
    do i=2,size(x)-1
        d_df(i) = 0.50d0*(fx(i+1) - fx(i) )/(x(i+1)-x(i) )+0.50d0*(fx(i) - fx(i-1) )/(x(i)-x(i-1) )
    enddo
    d_df(n) = 0.50d0*(fx(n) - fx(n-1) )/(x(n)-x(n-1) )+0.50d0*(fx(n-1) - fx(n-2) )/(x(n-1)-x(n-2) )


!    ! take derivative by Fourier Transformation
!    n = size(x)
!    
!    ! n = 2**m
!    ! log_2 n = m
!    a =log2( dble(n)) 
!    if(a ==dble(int(a)))then
!        ! n = 2**m
!        print *, "n = 2**m"
!        m = dble(int(a))
!    else
!        ! n /= 2**m
!        print *, "n /= 2**m"
!        m = dble(int(a)) + 1
!    endif 
!
!
!    fx_ = zeros(2**m)
!    fw  = zeros(2**m)
!    jwfw = zeros(2**m)
!    fx_(1:n) = fx(1:n)
!    d_df = zeros(n)
!
!    ! wmax = m/T, wmin = 0
!    w = linspace([0.0d0 ,dble(m)/(maxval(x)-minval(x)) ],2**m)
!
!    !(1) f(x) -> F(w)
!    Fw = FFT(fx_)
!
!    !(2) F(w) -> i*w*F(w)
!    jwFw = j*w(:)*Fw(:)
!
!    !(3) f(x) <- i*w*F(w) 
!    fx_ = IFFT(jwFw)
!
!    d_df(1:n) = real(fx_(1:n) )
!
!
end function
! ###############################################################

! ###############################################################
function d_dx_real32(x,fx) result(d_df)
    real(real32),intent(in) :: x(:),fx(:)
    real(real32),allocatable :: d_df(:)
    !complex(complex64),allocatable :: fx_(:),fw(:),w(:),jwFw(:)
    !complex(complex64) :: j = (0.0d0, 1.0d0)
    integer(int32) :: i,n
    real(real32) :: h

    ! 2nd-order central differential
    d_df = zeros(size(x) )
    n = size(x)
    
    d_df(1) = 0.50d0*(fx(3) - fx(2) )/(x(3)-x(2) )+0.50d0*(fx(2) - fx(1) )/(x(2)-x(1) )
    do i=2,size(x)-1
        d_df(i) = 0.50d0*(fx(i+1) - fx(i) )/(x(i+1)-x(i) )+0.50d0*(fx(i) - fx(i-1) )/(x(i)-x(i-1) )
    enddo
    d_df(n) = 0.50d0*(fx(n) - fx(n-1) )/(x(n)-x(n-1) )+0.50d0*(fx(n-1) - fx(n-2) )/(x(n-1)-x(n-2) )

end function
! ###############################################################

! ###############################################################
function d_dx_complex64(x,fx) result(d_df)
    complex(complex64),intent(in) :: x(:),fx(:)
    complex(complex64),allocatable :: d_df(:)
    !complex(complex64),allocatable :: fx_(:),fw(:),w(:),jwFw(:)
    !complex(complex64) :: j = (0.0d0, 1.0d0)
    integer(int32) :: i,n
    complex(complex64) :: h

    ! 2nd-order central differential
    d_df = zeros(size(x) )
    n = size(x)
    
    d_df(1) = 0.50d0*(fx(3) - fx(2) )/(x(3)-x(2) )+0.50d0*(fx(2) - fx(1) )/(x(2)-x(1) )
    do i=2,size(x)-1
        d_df(i) = 0.50d0*(fx(i+1) - fx(i) )/(x(i+1)-x(i) )+0.50d0*(fx(i) - fx(i-1) )/(x(i)-x(i-1) )
    enddo
    d_df(n) = 0.50d0*(fx(n) - fx(n-1) )/(x(n)-x(n-1) )+0.50d0*(fx(n-1) - fx(n-2) )/(x(n-1)-x(n-2) )

end function
! ###############################################################

! ###############################################################
function I_dx_real64(x,fx,f0) result(I_df)
    real(real64),intent(in) :: x(:),fx(:)
    real(real64),optional,intent(in) :: f0
    real(real64),allocatable :: I_df(:)
    integer(int32) :: i,n
    real(real64) :: h

    ! integral by Crank-Nicolson
    I_df = zeros(size(x) )
    n = size(x)
    
    if(present(f0) )then
        I_df(1) = f0
    else
        I_df(1) = 0.0d0
    endif
    do i=2,size(x)
        I_df(i) = I_df(i-1) + 0.50d0*(fx(i) + fx(i-1) )*abs( x(i)-x(i-1) )
    enddo

end function
! ###############################################################

! ###############################################################
function I_dx_real32(x,fx,f0) result(I_df)
    real(real32),intent(in) :: x(:),fx(:)
    real(real32),optional,intent(in) :: f0
    real(real32),allocatable :: I_df(:)
    integer(int32) :: i,n
    real(real32) :: h

    ! integral by Crank-Nicolson
    I_df = zeros(size(x) )
    n = size(x)
    
    if(present(f0) )then
        I_df(1) = f0
    else
        I_df(1) = 0.0d0
    endif
    do i=2,size(x)
        I_df(i) = I_df(i-1) + 0.50d0*(fx(i) + fx(i-1) )*abs( x(i)-x(i-1) )
    enddo

end function
! ###############################################################

! ###############################################################
function I_dx_complex64(x,fx,f0) result(I_df)
    complex(complex64),intent(in) :: x(:),fx(:)
    complex(complex64),optional,intent(in) :: f0
    complex(complex64),allocatable :: I_df(:)
    integer(int32) :: i,n
    complex(complex64) :: h

    ! integral by Crank-Nicolson
    I_df = zeros(size(x) )
    n = size(x)
    
    if(present(f0) )then
        I_df(1) = f0
    else
        I_df(1) = 0.0d0
    endif
    do i=2,size(x)
        I_df(i) = I_df(i-1) + 0.50d0*(fx(i) + fx(i-1) )*abs( x(i)-x(i-1) )
    enddo

end function
! ###############################################################

! ###############################################################
function matrixFromVectorsRe64(x1,x2,x3,x4,x5,x6,x7,x8) result(ret)
    real(real64),intent(in) :: x1(:)
    real(real64),optional,intent(in) ::  x2(:),x3(:),x4(:),x5(:),x6(:),x7(:),x8(:)
    real(real64),allocatable :: ret(:,:)
    integer(int32) :: n

    if(present(x2) )then
        if(present(x3) )then
            if(present(x4) )then
                if(present(x5) )then
                    if(present(x6) )then
                        if(present(x7) )then
                            if(present(x8) )then
                                n = maxval([size(x1),size(x2),size(x3),&
                                size(x4),size(x5),size(x6),size(x7),&
                                size(x8) ])
                                allocate(ret(n,8) )
                                ret(:,1) = x1(:)
                                ret(:,2) = x2(:)
                                ret(:,3) = x3(:)
                                ret(:,4) = x4(:)
                                ret(:,5) = x5(:)
                                ret(:,6) = x6(:)
                                ret(:,7) = x7(:)
                                ret(:,8) = x8(:)
                            else
                                n = maxval([size(x1),size(x2),size(x3),&
                                size(x4),size(x5),size(x6),size(x7) ])
                                allocate(ret(n,7) )
                                ret(:,1) = x1(:)
                                ret(:,2) = x2(:)
                                ret(:,3) = x3(:)
                                ret(:,4) = x4(:)
                                ret(:,5) = x5(:)
                                ret(:,6) = x6(:)
                                ret(:,7) = x7(:)
                            endif
                        else
                            n = maxval([size(x1),size(x2),size(x3),&
                            size(x4),size(x5),size(x6) ])
                            allocate(ret(n,6) )
                            ret(:,1) = x1(:)
                            ret(:,2) = x2(:)
                            ret(:,3) = x3(:)
                            ret(:,4) = x4(:)
                            ret(:,5) = x5(:)
                            ret(:,6) = x6(:)
                        endif
                    else
                        n = maxval([size(x1),size(x2),size(x3),&
                        size(x4),size(x5) ])
                        allocate(ret(n,5) )
                        ret(:,1) = x1(:)
                        ret(:,2) = x2(:)
                        ret(:,3) = x3(:)
                        ret(:,4) = x4(:)
                        ret(:,5) = x5(:)
            
                    endif
                else
                    n = maxval([size(x1),size(x2),size(x3),&
                    size(x4) ])
                    allocate(ret(n,4) )
                    ret(:,1) = x1(:)
                    ret(:,2) = x2(:)
                    ret(:,3) = x3(:)
                    ret(:,4) = x4(:)
                endif
            else
                n = maxval([size(x1),size(x2),size(x3)])
                allocate(ret(n,3) )
                ret(:,1) = x1(:)
                ret(:,2) = x2(:)
                ret(:,3) = x3(:)
            endif
        else

            n = maxval([size(x1),size(x2)])
            allocate(ret(n,2) )
            ret(:,1) = x1(:)
            ret(:,2) = x2(:)
        endif
    else
        n = maxval([size(x1)])
        allocate(ret(n,1) )
        ret(:,1) = x1(:)
    endif


end function
! ###############################################################


! ###############################################################
function matrixFromVectorsInt32(x1,x2,x3,x4,x5,x6,x7,x8) result(ret)
    integer(int32),intent(in) :: x1(:)
    integer(int32),optional,intent(in) ::  x2(:),x3(:),x4(:),x5(:),x6(:),x7(:),x8(:)
    real(real64),allocatable :: ret(:,:)
    integer(int32) :: n

    if(present(x2) )then
        if(present(x3) )then
            if(present(x4) )then
                if(present(x5) )then
                    if(present(x6) )then
                        if(present(x7) )then
                            if(present(x8) )then
                                n = maxval([size(x1),size(x2),size(x3),&
                                size(x4),size(x5),size(x6),size(x7),&
                                size(x8) ])
                                allocate(ret(n,8) )
                                ret(:,1) = x1(:)
                                ret(:,2) = x2(:)
                                ret(:,3) = x3(:)
                                ret(:,4) = x4(:)
                                ret(:,5) = x5(:)
                                ret(:,6) = x6(:)
                                ret(:,7) = x7(:)
                                ret(:,8) = x8(:)
                            else
                                n = maxval([size(x1),size(x2),size(x3),&
                                size(x4),size(x5),size(x6),size(x7) ])
                                allocate(ret(n,7) )
                                ret(:,1) = x1(:)
                                ret(:,2) = x2(:)
                                ret(:,3) = x3(:)
                                ret(:,4) = x4(:)
                                ret(:,5) = x5(:)
                                ret(:,6) = x6(:)
                                ret(:,7) = x7(:)
                            endif
                        else
                            n = maxval([size(x1),size(x2),size(x3),&
                            size(x4),size(x5),size(x6) ])
                            allocate(ret(n,6) )
                            ret(:,1) = x1(:)
                            ret(:,2) = x2(:)
                            ret(:,3) = x3(:)
                            ret(:,4) = x4(:)
                            ret(:,5) = x5(:)
                            ret(:,6) = x6(:)
                        endif
                    else
                        n = maxval([size(x1),size(x2),size(x3),&
                        size(x4),size(x5) ])
                        allocate(ret(n,5) )
                        ret(:,1) = x1(:)
                        ret(:,2) = x2(:)
                        ret(:,3) = x3(:)
                        ret(:,4) = x4(:)
                        ret(:,5) = x5(:)
            
                    endif
                else
                    n = maxval([size(x1),size(x2),size(x3),&
                    size(x4) ])
                    allocate(ret(n,4) )
                    ret(:,1) = x1(:)
                    ret(:,2) = x2(:)
                    ret(:,3) = x3(:)
                    ret(:,4) = x4(:)
                endif
            else
                n = maxval([size(x1),size(x2),size(x3)])
                allocate(ret(n,3) )
                ret(:,1) = x1(:)
                ret(:,2) = x2(:)
                ret(:,3) = x3(:)
            endif
        else

            n = maxval([size(x1),size(x2)])
            allocate(ret(n,2) )
            ret(:,1) = x1(:)
            ret(:,2) = x2(:)
        endif
    else
        n = maxval([size(x1)])
        allocate(ret(n,1) )
        ret(:,1) = x1(:)
    endif

end function
! ###############################################################


! ###############################################################
pure function shiftInt32vector(vec) result(ret)
    integeR(int32),intent(in) :: vec(:)
    integeR(int32),allocatable :: ret(:)
    
    allocate(ret(size(vec)) )
    ret(2:size(vec)) = vec(1:size(vec)-1)
    ret(1) = vec(size(vec))

end function
! ###############################################################


! ###############################################################
pure function exchangeInt32vector(vec,a,b) result(ret)
    integeR(int32),intent(in) :: vec(:)
    integeR(int32),intent(in):: a,b
    integeR(int32)::buf
    integeR(int32),allocatable :: ret(:)
    ret = vec
    ret(a) = vec(b)
    ret(b) = vec(a)

end function
! ###############################################################


! ###############################################################
pure function exchangeInt32vector2(vec)  result(ret)
    integeR(int32),intent(in) :: vec(2)
    integeR(int32)::buf
    integeR(int32) :: ret(2)

    ret(2) = vec(1)
    ret(1) = vec(2)

end function
! ###############################################################

! ###############################################################
subroutine RefineSequenceReal64(x,Fx,x_range,num_point) 
    ! in: observation -> out: interpolated
    real(real64),intent(in) :: x_range(2)
    real(real64),allocatable,intent(inout) :: x(:), Fx(:)
    ! observation
    real(real64),allocatable :: x_o(:), Fx_o(:)
    integer(int32),intent(in) :: num_point
    integer(int32) :: i
    
    x_o  = x
    Fx_o = Fx

    x  = linspace(x_range,num_point)
    Fx = zeros(num_point)
    
    do i=1,num_point
        Fx(i) =  interpolateOneReal64(x=x_o, Fx=Fx_o, x_value=x(i) )
    enddo

end subroutine
! ###############################################################


! ###############################################################
subroutine RefineSequenceReal32(x,Fx,x_range,num_point) 
    ! in: observation -> out: interpolated
    real(real32),intent(in) :: x_range(2)
    real(real32),allocatable,intent(inout) :: x(:), Fx(:)
    ! observation
    real(real32),allocatable :: x_o(:), Fx_o(:)
    integer(int32),intent(in) :: num_point
    integer(int32) :: i
    
    x_o  = x
    Fx_o = Fx

    x  = linspace(x_range,num_point)
    Fx = zeros(num_point)
    
    do i=1,num_point
        Fx(i) =  interpolateOneReal32(x=x_o, Fx=Fx_o, x_value=x(i) )
    enddo

end subroutine
! ###############################################################

! ###############################################################
subroutine RefineSequencecomplex64(x,Fx,x_range,num_point) 
    ! in: observation -> out: interpolated
    complex(complex64),intent(in) :: x_range(2)
    complex(complex64),allocatable,intent(inout) :: x(:), Fx(:)
    ! observation
    complex(complex64),allocatable :: x_o(:), Fx_o(:)
    integer(int32),intent(in) :: num_point
    integer(int32) :: i
    
    x_o  = x
    Fx_o = Fx

    x  = linspace(x_range,num_point)
    Fx = zeros(num_point)
    
    do i=1,num_point
        Fx(i) =  interpolateOneComplex64(x_c=x_o, Fx_c=Fx_o, x_value_c=x(i) )
    enddo

end subroutine
! ###############################################################

function taperReal64(x,margin) result(ret)
    use iso_fortran_env
    implicit none
    complex(real64),intent(in) :: x(:)
    complex(real64),allocatable :: ret(:)
    integer(int32) :: i, n, k
    real(real64),intent(in) :: margin
    real(real64) :: rate

    ! triangle taper
    n = size(x)

    ret = x
    ! 
    k = int( margin*dble(n) )
    do i=1,k
        rate = dble(i-1)/dble(k)
        ret(i) = ret(i)*rate
        ret(n-i+1) = ret(n-i+1)*rate
    enddo

end function


function taperComplex64(x,margin) result(ret)
    use iso_fortran_env
    implicit none

    complex(complex64),intent(in) :: x(:)
    complex(complex64),allocatable :: ret(:)
    integer(int32) :: i, n, k
    real(real64),intent(in) :: margin
    real(real64) :: rate

    ! triangle taper
    n = size(x)

    ret = x
    ! 
    k = int( margin*dble(n) )
    do i=1,k
        rate = dble(i-1)/dble(k)
        ret(i) = ret(i)*rate
        ret(n-i+1) = ret(n-i+1)*rate 
    enddo


end function


end module ArrayClass