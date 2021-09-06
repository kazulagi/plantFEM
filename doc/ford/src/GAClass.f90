module GAClass
    use RandomClass
    use ArrayClass
    use MathClass
    implicit none

    type :: GA_annotaton
        character(:),allocatable :: annotation
    end type

    type :: GA_Individual_
        real(real64),allocatable  :: realParameter(:)
        integer(int32),allocatable:: intParameter(:)
        type(GA_annotaton),allocatable :: realAnnotaton(:)
        type(GA_annotaton),allocatable :: intAnnotaton(:)
        logical,allocatable :: realRegistered(:) 
        logical,allocatable :: intRegistered(:)  
    contains
        procedure,public :: init => initGA_Individual
    end type

    type :: GA_
        type(GA_Individual_),allocatable :: plants(:)
        real(real64),allocatable :: score(:)
        integer(int32),allocatable :: selected(:)
        real(real64),allocatable :: selectedScore(:)
        integer(int32) :: num_individual
        logical :: initialized = .false.
        logical :: registered = .false.
        logical :: realRegistered = .false.
        logical :: intRegistered  = .false.  
    contains
        procedure, public :: init => initGA
        procedure, public :: setup => setupGA 
        procedure, public :: show => showGA
        procedure, public :: parse => parseGA
        procedure, public :: select => selectGA
        procedure, public :: cross => crossGA
        procedure, public :: mutate => mutateGA
    end type
contains

! #################################################################
subroutine initGA_Individual(obj,num_real,num_int)
    class(GA_Individual_),intent(inout) :: obj
    integer(int32),intent(in) :: num_real,num_int
    
    if(num_real/=0)then
        obj%realParameter = zeros(num_real)
        allocate(obj%realAnnotaton(num_real) )
        allocate(obj%realRegistered(num_real) )
        obj%realRegistered(:) = .false.
    endif
    

    if(num_int/=0)then
        obj%intParameter = zeros(num_int)
        allocate(obj%intAnnotaton(num_int) )
        allocate(obj%intRegistered(num_int) )
        obj%intRegistered(:) = .false.
    endif
    
    !if(num_real==0) then
    !    obj%realRegistered = .true.
    !endif

    !if(num_int==0) then
    !    obj%intRegistered = .true.
    !endif
end subroutine
! #################################################################
! #################################################################
! #################################################################
! #################################################################




! #################################################################
subroutine initGA(obj,num_individual,num_real,num_int)
    class(GA_),intent(inout) :: obj
    integer(int32),intent(in) :: num_individual
    integer(int32),intent(in) :: num_real,num_int
    integer(int32) :: i


    if(allocated(obj%plants) ) deallocate(obj%plants)

    if(num_real < 0 .or. num_int < 0)then
        print *, "ERROR :: initGA >> invalid num_real/num_int >> both should be >= 0"
        stop
    endif
    ! generate individuals
    allocate(obj%plants(num_individual) )
    obj%score=zeros(num_individual) 
    obj%num_individual = num_individual

    ! fill zero to initialize
    do i=1, num_individual
        call obj%plants(i)%init(num_real=num_real,num_int=num_int)
    enddo


    obj%initialized = .true.
    
end subroutine
! #####################################################################


! #####################################################################
subroutine setupGA(obj,DataType,DataID,DataRange,DataAnnotation)
    class(GA_),intent(inout) :: obj
    integer(int32),intent(in) :: DataType,DataID
    real(real32),intent(in) :: DataRange(2)
    character(*),intent(in) :: DataAnnotation

    real(real32) :: DRange(2)
    real(real64) :: theta
    type(Random_) :: random
    integer(int32):: i

    DRange(1) = minval(DataRange)
    DRange(2) = maxval(DataRange)

    if(.not. obj%initialized)then
        print *, "ERROR :: setupGA >> not initialized. please call %init()"
        stop
    endif
    
    if(DataType==real64)then
        do i=1,size(obj%plants)
            ! set random data
            ! by DRange
            theta = random%random()
            obj%plants(i)%realParameter(DataID) = (1.0d0-theta)*Drange(1) + theta*Drange(2)
            obj%plants(i)%realAnnotaton(DataID)%annotation = DataAnnotation
            obj%plants(i)%realRegistered(DataID) = .true.
        enddo

    elseif(DataType==int32)then
        do i=1,size(obj%plants)
            ! set random data
            ! by DRange
            theta = random%random()
            obj%plants(i)%intParameter(DataID) = int((1.0d0-theta)*Drange(1) + theta*Drange(2))
            obj%plants(i)%intAnnotaton(DataID)%annotation = DataAnnotation
            obj%plants(i)%intRegistered(DataID) = .true.
        enddo

    else
        print *, "ERROR :: setupGA >> unknown datatype. please input real64 or int32"
        stop
    endif
    

end subroutine
! #####################################################################

! #####################################################################
subroutine showGA(obj,KeyWord)
    class(GA_),intent(in) :: obj
    character(*),intent(in) :: KeyWord
    logical :: found=.false.
    integer(int32) :: i,j

    if(.not.obj%initialized)then
        print *, "[CAUTION] >> showGA >>  not initialized"
        return
    endif

    if(allocated(obj%plants(1)%realParameter) )then
        do i=1,size(obj%plants(1)%realAnnotaton)
            if( index(obj%plants(1)%realAnnotaton(i)%annotation,KeyWord)/=0)then
                print *, "Data :: "//obj%plants(1)%realAnnotaton(i)%annotation//" DataType :: Real64"
                do j=1,size(obj%plants)
                    print *, "IndvID: ",j," Value:",obj%plants(j)%realParameter(i)
                enddo
                found = .true.
            endif
        enddo
    endif

    if(allocated(obj%plants(1)%intParameter) )then
        do i=1,size(obj%plants(1)%intParameter)
            if( index(obj%plants(1)%intAnnotaton(i)%annotation,KeyWord)/=0)then
                print *, "Data :: "//obj%plants(1)%intAnnotaton(i)%annotation//" DataType :: int32"
                do j=1,size(obj%plants)
                    print *, "IndvID: ",j," Value:",obj%plants(j)%intParameter(i)
                enddo
                found = .true.
            endif
        enddo
    endif

    if(.not.found)then
        print *, "Not Found."
    endif

end subroutine
! #####################################################################

! #####################################################################
function parseGA(obj,KeyWord) result(ret)
    class(GA_),intent(in) :: obj
    character(*),intent(in) :: KeyWord
    real(real64),allocatable :: ret(:)
    logical :: found=.false.
    integer(int32) :: i,j

    ret = zeros(obj%num_individual)

    if(.not.obj%initialized)then
        print *, "[CAUTION] >> showGA >>  not initialized"
        return
    endif

    if(allocated(obj%plants(1)%realParameter) )then
        do i=1,size(obj%plants(1)%realAnnotaton)
            if( index(obj%plants(1)%realAnnotaton(i)%annotation,KeyWord)/=0)then
                do j=1,size(obj%plants)
                    ret(j) = obj%plants(j)%realParameter(i)
                enddo
                found = .true.
            endif
        enddo
        return
    endif

    if(allocated(obj%plants(1)%intParameter) )then
        do i=1,size(obj%plants(1)%intParameter)
            if( index(obj%plants(1)%intAnnotaton(i)%annotation,KeyWord)/=0)then
                do j=1,size(obj%plants)
                    ret(j) = dble(obj%plants(j)%intParameter(i))
                enddo
                found = .true.
            endif
        enddo
        return
    endif

    if(.not.found)then
        print *, "Not Found."
    endif

end function
! #####################################################################



! #####################################################################
subroutine selectGA(obj,score,SurvivalRate)
    class(GA_),intent(inout) :: obj
    real(real64),intent(in) :: score(:),SurvivalRate
    integer(int32) :: num_selection ,i,n
    real(real64),allocatable :: id(:),score_val(:)

    num_selection = int( SurvivalRate*obj%num_individual )
    if(num_selection==0)then
        num_selection=1
    endif
    id = zeros(obj%num_individual)
    do i=1,size(id)
        id(i) = i
    enddo
    score_val = score
    n = obj%num_individual
    call heapsort(array=score_val,val=id, n=obj%num_individual )
    obj%selected = id(n-num_selection+1:n)
    obj%selectedScore = score_val(n-num_selection+1:n)


end subroutine
! #####################################################################


! #####################################################################
subroutine crossGA(obj)
    class(GA_),intent(inout) :: obj
    type(GA_) :: copy
    type(Random_) :: random
    integer(int32) :: i,j,num_select,itr,parent1, parent2
    real(real64),allocatable :: realbuf(:)
    real(real64) :: theta

    copy = obj
    num_select = size(obj%selected)
    realbuf = zeros(num_select)

    if(allocated(obj%plants(1)%realParameter) )then
        ! real parameter exists
        ! for all new individuals, update info.
        do i=1,obj%num_individual
            ! determine parents
            parent1 = obj%selected( random%randint(from=1,to=num_select) )
            parent2 = obj%selected( random%randint(from=1,to=num_select) )
            ! create new data
            ! weighted averaging
            do j=1,size(obj%plants(i)%realParameter)
                theta = random%random() ! 0 < theta < 1
                obj%plants(i)%realParameter(j) = &
                    (1.0d0-theta)*copy%plants(parent1)%realParameter(j) + &
                    theta*copy%plants(parent2)%realParameter(j) 
            enddo
        enddo
    endif



    if(allocated(obj%plants(1)%intParameter) )then
        ! int parameter exists
        ! for all new individuals, update info.
        do i=1,obj%num_individual
            ! determine parents
            parent1 = obj%selected( random%randint(from=1,to=num_select) )
            parent2 = obj%selected( random%randint(from=1,to=num_select) )
            ! create new data
            ! random selection
            do j=1,size(obj%plants(i)%intParameter)
                theta = random%random()
                if(theta >= 0.50d0)then
                    obj%plants(i)%intParameter(j) = copy%plants(parent1)%intParameter(j)
                else
                    obj%plants(i)%intParameter(j) = copy%plants(parent2)%intParameter(j)
                endif
            enddo
        enddo
    endif

    

end subroutine
! #####################################################################

! #####################################################################
subroutine mutateGA(obj,KeyWord,sigma)
    class(GA_),intent(inout) :: obj
    character(*),intent(in) :: KeyWord
    real(real64),intent(in) :: sigma
    type(Random_) :: random
    logical :: found = .false.
    integer(int32) :: i,j
    

    if(allocated(obj%plants(1)%realParameter) )then
        do i=1,size(obj%plants(1)%realAnnotaton)
            if( index(obj%plants(1)%realAnnotaton(i)%annotation,KeyWord)/=0)then
                do j=1,size(obj%plants)
                    obj%plants(j)%realParameter(i) = &
                    obj%plants(j)%realParameter(i) + random%gauss(mu=0.0d0,sigma=sigma)
                enddo
                found = .true.
            endif
        enddo
        return
    endif

    if(allocated(obj%plants(1)%intParameter) )then
        do i=1,size(obj%plants(1)%intParameter)
            if( index(obj%plants(1)%intAnnotaton(i)%annotation,KeyWord)/=0)then
                do j=1,size(obj%plants)
                    obj%plants(j)%intParameter(i) = &
                    int(dble(obj%plants(j)%intParameter(i))+random%gauss(mu=0.0d0,sigma=sigma))
                enddo
                found = .true.
            endif
        enddo
        return
    endif

    if(.not.found)then
        print *, "Not Found."
    endif

    
end subroutine
! #####################################################################

end module