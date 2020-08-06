module RandomClass
    use, intrinsic :: iso_fortran_env
    use MathClass
    implicit none


    type::Random_
        integer(int32) :: random_int
        integer(int32),allocatable  :: random_int_seed(:)
        integer(int32),allocatable :: random_int_vec(:)
        real(real64) :: random_real
        real(real64),allocatable :: random_real_vec(:)
    contains
        procedure :: init       => initRandom
        procedure :: random     => getRandom
        procedure :: randint    => getRandomInt
        procedure :: choiceInt  => choiceRandomInt
        procedure :: choiceReal => choiceRandomReal
        procedure :: uniform    => uniformRandom
        procedure :: save       => saveRandom
        procedure :: randn      => randnRandom
        procedure :: histogram      => histogramRandom
        !procedure :: choiceString => choiceRandomString
    end type

contains



!##########################################
subroutine initRandom(obj)
    class(Random_),intent(inout)::obj
    !integer(int32),optional,intent(in)::SeedSize
    integer(int32)::SeedSize
    
    call random_seed(size=SeedSize)
    allocate(obj%random_int_seed(SeedSize) )
    !call random_seed(get=obj%random_real_vec)
    call random_seed(get=obj%random_int_seed)

end subroutine
!##########################################


!##########################################
function getRandom(obj,distribution) result(x)
    class(Random_)::obj
    character(*),optional,intent(in)::distribution
    real(real64) :: x,val,y
    integer(int32) :: i


    if(trim(distribution)=="Binomial" .or. trim(distribution)=="binomial")then
        val=0.0d0
        do i=1,20
            call random_number(y)
            val=val+y
        enddo
        x=val-10.0d0
        return
    endif

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
    real(real64) :: x,a,diff,val(2)
    real(real64),intent(in) :: From,To

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
    real(real64) :: xr,a,diff,val(2)
    integer(int32) :: x
    integer(int32),intent(in) :: From,To

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
    integer(int32),optional,intent(in) :: Vector(:)
    integer(int32),Optional,intent(in) :: Array(:,:)
    integer(int32) :: val,posi,posi2

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
    real(real64),Optional,intent(in) :: Vector(:)
    real(real64),Optional,intent(in) :: Array(:,:)
    real(real64) :: val
    integer(int32) :: posi,posi2

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
function randnRandom(obj,d0,d1) result(array)
    class(Random_),intent(inout)::obj
    real(real64),allocatable :: array(:,:)
    integer(int32),optional,intent(in) :: d0,d1
    integer(int32) :: n,m,i,j

    n=input(default=1, option=d0)
    m=input(default=1, option=d1)

    allocate(array(n,m) )

    call obj%init()

    do i=1,n
        do j=1,m
            array(i,j) = obj%random()
        enddo
    enddo
    
end function
!##########################################



!##########################################
!function choiceRandomString(obj,Str) result(val)
!    class(Random_),intent(in) :: obj
!    character(*),  intent(in) :: Str
!    character(1) :: val
!    integer(int32) :: posi,length
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


!##########################################
function histogramRandom(obj,list,division) result(histogram)
    class(Random_),intent(inout) :: obj
    real(real64),intent(in)  :: list(:)
    integer(int32),allocatable :: histogram(:)
    integer(int32),optional,intent(in) :: division
    integer(int32) :: i,j,n
    real(real64) :: maxv, minv,val,intval

    n=input(default=10,option=division)
    maxv=maxval(list)
    minv=minval(list)
    intval=(maxv-minv)/dble(n)

    allocate( histogram(n) )
    histogram(:)=0
    do i=1,size(list,1)
        val = (list(i) - minv )/intval
        if(n < int(val)+1 )then
            val=val-1.0d0
        endif
        if(1 > int(val)+1 )then
            val=1.0d0
        endif
        histogram(int(val)+1) = histogram(int(val)+1) + 1
    enddo

end function
!##########################################

end module