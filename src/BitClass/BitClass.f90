module BitClass
    use iso_fortran_env
    implicit none

    type :: Bit_
        logical,allocatable :: bitArray(:)
    contains
        procedure, public :: init => initBit
        procedure, public :: int => intBit
        procedure, public :: not => notBit
    end type

    interface not
        procedure notBitfunc
    end interface

    interface reverse
        procedure reverseBitfunc
    end interface

    interface assignment(=)
        module procedure assignIntBit
    end interface

contains

! ##########################################################
subroutine initBit(obj,n) 
    class(Bit_),intent(inout) :: obj
    integer(int32),intent(in) :: n

    if(allocated(obj%bitArray) )then
        deallocate(obj%bitArray)
    endif
    allocate(obj%bitArray(n) )
    obj%bitArray(:) = .false.

end subroutine
! ##########################################################


! ##########################################################
subroutine notBit(obj) 
    class(Bit_),intent(inout) :: obj
    
    obj%bitArray(:) = .not.(obj%bitArray(:) )
    
end subroutine
! ##########################################################

! ##########################################################
function intBit(obj) result(ret) 
    class(Bit_),intent(inout) :: obj
    integer(int32) :: ret, i

    if(.not. allocated(obj%bitArray) )then
        ret = 0
        return
    endif
    
    ret=0
    do i=1,size(obj%bitArray)
        if(obj%bitArray(i) )then
            ret = ret + 2**(i-1)
        endif
    enddo

end function
! ##########################################################


! ##########################################################
function notBitFunc(obj) result(ret)
    type(Bit_),intent(in) :: obj
    type(Bit_) :: ret

    ret = obj
    call ret%not()



end function
! ##########################################################


! ##########################################################
function reverseBitFunc(obj) result(ret)
    type(Bit_),intent(in) :: obj
    type(Bit_) :: ret
    integer(int32) :: i,j
    
    allocate(ret%bitArray(sizE(obj%bitArray)))
    j=0
    do i=sizE(obj%bitArray),1,-1
        j=j+1
        ret%bitArray(j) = obj%bitArray(i)
    enddo

end function
! ##########################################################

! ##########################################################
subroutine assignIntBit(x,y)
    type(Bit_), intent(inout) :: x
    integer(int32),intent(in)  :: y
    integer(int32) :: i,n,m,order
    
    m = y
    order = 1
    do 
        if(m<2)then

            if(mod(m,2)==1)then
                x%bitArray(order) = .true.
            else
                x%bitArray(order) = .false.
            endif
            exit
        endif

        if(size(x%bitArray) < order )then
            print *, "BitClass >> = ERROR :: exceed bit limit:",size(x%bitArray)," You request : ", order
            stop
        endif

        if(mod(m,2)==1)then
            x%bitArray(order) = .true.
        else
            x%bitArray(order) = .false.
        endif

        m = m - mod(m,2)
        m = m/2
        order=order+1
        
        

    enddo
    

end subroutine
! ##########################################################


end module