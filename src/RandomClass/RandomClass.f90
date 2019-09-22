module RandomClass
    use MathClass
    implicit none


    type::Random_
        integer :: random_int
        integer,allocatable  :: random_int_seed(:)
        integer,allocatable :: random_int_vec(:)
        real(8) :: random_real
        real(8),allocatable :: random_real_vec(:)
    contains
        procedure :: init       => initRandom
        procedure :: random     => getRandom
        procedure :: randint    => getRandomInt
        procedure :: choiceInt  => choiceRandomInt
        procedure :: choiceReal => choiceRandomReal
        !procedure :: choiceString => choiceRandomString
        procedure :: uniform    => uniformRandom
        procedure :: save       => saveRandom
    end type

contains



!##########################################
subroutine initRandom(obj)
    class(Random_),intent(inout)::obj
    !integer,optional,intent(in)::SeedSize
    integer::SeedSize
    
    call random_seed(size=SeedSize)
    allocate(obj%random_int_seed(SeedSize) )
    !call random_seed(get=obj%random_real_vec)
    call random_seed(get=obj%random_int_seed)

end subroutine
!##########################################


!##########################################
function getRandom(obj) result(x)
    class(Random_)::obj
    real(8) :: x

    call random_number(x)

end function
!##########################################



!##########################################
subroutine saveRandom(obj)
    class(Random_),intent(inout)::obj

    call random_seed(put=obj%random_int_seed)
end subroutine
!##########################################

!##########################################
function uniformRandom(obj,From,To) result(x)
    class(Random_),intent(in)::obj
    real(8) :: x,a,diff,val(2)
    real(8),intent(in) :: From,To

    val(1)=From
    val(2)=To
    diff=abs(from-to)
    call random_number(a)
    x=a*diff+minval(val)

end function
!##########################################


!##########################################
function getRandomInt(obj,From,To) result(x)
    class(Random_),intent(in)::obj
    real(8) :: xr,a,diff,val(2)
    integer :: x
    integer,intent(in) :: From,To

    val(1)=From
    val(2)=To
    diff=abs(dble(from)- dble(to) )
    
    call random_number(a)
    xr=a*diff+minval(val)
    x=nint(xr)
    if(x==From-1)then
        x=From
    endif
    if(x==To+1)then
        x=To
    endif
    
end function
!##########################################

!##########################################
function choiceRandomInt(obj,Vector,Array) result(val)
    class(Random_),intent(in)::obj
    integer,optional,intent(in) :: Vector(:)
    integer,Optional,intent(in) :: Array(:,:)
    integer :: val,posi,posi2

    ! it should be over-rided
    if(present(Vector) )then
        posi=obj%randint(1,size(Vector) )
        val=Vector(posi)
        return
    endif

    if(present(Array ))then
        print *, size(Array,1)
        posi =obj%randint(1,size(Array,1) )
        posi2=obj%randint(1,size(Array,2) )
        val=Array(posi,posi2)
        return
    endif

    print *, "No list is imported."


end function
!##########################################


!##########################################
function choiceRandomReal(obj,Vector,Array) result(val)
    class(Random_),intent(in)::obj
    real(8),Optional,intent(in) :: Vector(:)
    real(8),Optional,intent(in) :: Array(:,:)
    real(8) :: val
    integer :: posi,posi2

    ! it should be over-rided
    if(present(Vector) )then
        posi=obj%randint(1,size(Vector) )
        val=Vector(posi)
        return
    endif

    if(present(Array ))then
        print *, size(Array,1)
        posi =obj%randint(1,size(Array,1) )
        posi2=obj%randint(1,size(Array,2) )
        val=Array(posi,posi2)
        return
    endif

    print *, "No list is imported."
    
end function
!##########################################


!##########################################
!function choiceRandomString(obj,Str) result(val)
!    class(Random_),intent(in) :: obj
!    character(*),  intent(in) :: Str
!    character(1) :: val
!    integer :: posi,length
!
!    length=len(Str)
!
!    ! it should be over-rided
!    posi=obj%randint(1,length )
!    val=Str(posi)
!    
!
!end function
!##########################################


end module