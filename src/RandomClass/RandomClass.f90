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
        procedure :: name    => nameRandom
        procedure :: choiceInt  => choiceRandomInt
        procedure :: choiceReal => choiceRandomReal
        procedure :: uniform    => uniformRandom
        procedure :: gauss    => gaussRandom
        procedure :: ChiSquared => ChiSquaredRandom
        procedure :: Chauchy => ChauchyRandom
        procedure :: Lognormal => LognormalRandom
        procedure :: InverseGauss => InverseGaussRandom
        procedure :: save       => saveRandom
        
        procedure,pass :: randnRandomVector
        procedure,pass :: randnRandomArray
        generic :: randn => randnRandomArray,randnRandomvector

        procedure :: fill       => fillRandom
        procedure :: histogram      => histogramRandom
        procedure :: scalar          => scalarRandom
        procedure :: vector          => vectorRandom
        procedure :: matrix          => matrixRandom
        procedure :: cube            => cubeRandom
        !procedure :: choiceString => choiceRandomString
    end type


    interface randi
        module procedure :: randi_range_rank2,randi_imax,randi_imax_n
    end interface

    interface rand
        module procedure :: rand_n,rand_sz2,rand_sz3
    end interface
contains



!##########################################
subroutine initRandom(obj)
    class(Random_),intent(inout)::obj
    !integer(int32),optional,intent(in)::SeedSize
    integer(int32)::SeedSize
    


    call random_seed(size=SeedSize)
    if(.not.allocated(obj%random_int_seed) )then
        allocate(obj%random_int_seed(SeedSize) )
    endif
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
function randnRandomArray(obj,d0,d1) result(array)
    class(Random_),intent(inout)::obj
    real(real64),allocatable :: array(:,:)
    integer(int32),intent(in) :: d0,d1
    integer(int32) :: n,m,i,j

    n=d0
    m=d1

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
function randnRandomVector(obj,d0) result(array)
    class(Random_),intent(inout)::obj
    real(real64),allocatable :: array(:)
    integer(int32),intent(in) :: d0
    integer(int32) :: n,m,i,j

    n=d0

    allocate(array(n) )

    call obj%init()

    do i=1,n
        array(i) = obj%random() 
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
    real(real64),intent(in)  :: list(:) ! data-list
    real(real64),allocatable :: histogram(:,:)
    integer(int32),optional,intent(in) :: division
    integer(int32) :: i,j,n
    real(real64) :: maxv, minv,val,intval

    n=input(default=10,option=division)

    maxv=maxval(list)
    minv=minval(list)

    intval=(maxv-minv)/dble(n)

    allocate( histogram(n,2) )
    histogram(:,:)=0


    val = minv-0.00000010d0
    do i=1,size(histogram,1)
        histogram(i,1) = val
        val = val + intval
    enddo

    do i=1,size(list,1)
        val = minv-0.00000010d0
        do j=1, size(histogram,1)
            if(val < list(i) .and. list(i) <= val + intval  )then
                histogram(j,2) = histogram(j,2) + 1.0d0
                exit
            endif
            val = val + intval
        enddo
    enddo



end function
!##########################################


!##########################################
function nameRandom(obj) result(str)
    class(Random_),intent(inout) :: obj
    character(200) :: str
    integer(int32) :: n

    call obj%init()
    n=int(obj%random()*1000000)

    str="RandName"//fstring_int(n)
!##########################################

end function

! Reference: omitakahiro.github.io

!##########################################
function gaussRandom(obj,mu,sigma) result(ret)
    class(Random_),intent(inout) :: obj
    real(real64),intent(in) :: mu,sigma
    real(real64) :: ret
    real(real64) :: pi = 3.141592653d0


    ret = sqrt( -2.0d0 * log(obj%random() ) )*sin(2.0d0*pi*obj%random())
    ret = mu + sigma*ret


end function 
!##########################################


!##########################################
function ChiSquaredRandom(obj,k) result(ret)
    class(Random_),intent(inout) :: obj
    real(real64),intent(in) :: k
    real(real64) :: ret,z,w
    real(real64) :: pi = 3.141592653d0
    integer(int32) :: i

    w=0.0d0
    z=0.0d0
    ret=0.0d0
    do i=1, int(k)
        z = sqrt( -2.0d0 * log(obj%random() ) )*sin(2.0d0*pi*obj%random())
        w = w + z*z
    enddo
    ret = w

end function 
!##########################################

!##########################################
function ChauchyRandom(obj,mu,gamma) result(ret)
    class(Random_),intent(inout) :: obj
    real(real64),intent(in) :: mu,gamma
    real(real64) :: ret,z,w
    real(real64) :: pi = 3.141592653d0

    
    ret = mu + gamma*tan(pi*(obj%random()-0.50d0 ) )
    
end function 
!##########################################


!##########################################
function LognormalRandom(obj,mu,sigma) result(ret)
    class(Random_),intent(inout) :: obj
    real(real64),intent(in) :: mu,sigma
    real(real64) :: ret,z,w
    real(real64) :: pi = 3.141592653d0

    
    ret = obj%gauss(mu=mu, sigma=sigma)
    ret = exp(ret)
    
end function 
!##########################################

!##########################################
function InverseGaussRandom(obj,mu,lambda) result(ret)
    class(Random_),intent(inout) :: obj
    real(real64),intent(in) :: mu,lambda
    real(real64) :: ret,x,y,z,w
    real(real64) :: pi = 3.141592653d0
    

    x = obj%gauss(mu=0.0d0,sigma=1.0d0)
    y = x*x
    w = mu+0.50d0*y*mu*mu/lambda - (0.50d0*mu/lambda)*sqrt(4.0d0*mu*lambda*y+mu*mu*y*y)
    z = obj%random()

    if(z < mu/(mu+w))then
        ret=w
    else
        ret=mu*mu/w
    endif
end function 
!##########################################

!##########################################
subroutine fillRandom(obj,array,vector)
    class(Random_),intent(inout) :: obj
    real(real64),optional,intent(inout)  :: array(:,:),vector(:)
    integer(int32) :: i,j

    call obj%init()
    if(present(array) )then
        do i=1,size(Array,2)
            do j=1,sizE(Array,1)
                array(j,i) = obj%random()
            enddo
        enddo
    endif
    if(present(vector) )then
        do i=1,size(vector,1)
            vector(i) = obj%random()
        enddo
    endif
end subroutine
!##########################################


!##########################################
function cubeRandom(obj,size1, size2, size3) result(ret)
    class(Random_) ,intent(inout) :: obj
    integer(int32),intent(in) :: size1, size2, size3
    real(real64),allocatable :: ret(:,:,:)
    integer(int32) :: i,j,k

    allocate( ret(size1,size2, size3) )
    do i=1,size3
        do j=1,size2
            do k=1,size1
                ret(k,j,i) = obj%random()
            enddo
        enddo
    enddo

end function
!##########################################


!##########################################
function matrixRandom(obj,size1, size2) result(ret)
    class(Random_) ,intent(inout) :: obj
    integer(int32),intent(in) :: size1, size2
    real(real64),allocatable :: ret(:,:)
    integer(int32) :: j,k

    allocate( ret(size1,size2) )
    do j=1,size2
        do k=1,size1
            ret(k,j) = obj%random()
        enddo
    enddo

end function
!##########################################


!##########################################
function vectorRandom(obj,size1) result(ret)
    class(Random_) ,intent(inout) :: obj
    integer(int32),intent(in) :: size1
    real(real64),allocatable :: ret(:)
    integer(int32) :: k

    allocate( ret(size1) )
    do k=1,size1
        ret(k) = obj%random()
    enddo
    

end function
!##########################################


!##########################################
function scalarRandom(obj,size1) result(ret)
    class(Random_) ,intent(inout) :: obj
    integer(int32),intent(in) :: size1
    real(real64),allocatable :: ret
    integer(int32) :: k

    ret = obj%random()
    

end function
!##########################################

!##########################################
function randi_range_rank2(valrange,size1,size2) result(ret)
    integer(int32),intent(in) :: valrange(2),size1, size2
    type(Random_) :: random
    integer(int32) :: i,j
    integer(int32),allocatable :: ret(:,:)

    allocate(ret(size1,size2) )
    do i=1,size2
        do j=1,size1
            ret(j,i) = nint(dble(valrange(2)-valrange(1))*random%random()*1.00050d0 + dble(valrange(1)) )
        enddo
    enddo


end function
!##########################################

!##########################################
function randi_imax(imax) result(ret)
    integer(int32),intent(in) :: imax
    type(Random_) :: random
    integer(int32) :: ret

    ret = nint(random%random()*dble(imax-1) + 1.0d0)

end function
!##########################################


!##########################################
function randi_imax_n(imax,n) result(ret)
    integer(int32),intent(in) :: imax, n
    type(Random_) :: random
    integer(int32) :: ret(n),i

    do i=1,n
        ret(i) = nint(random%random()*dble(imax-1) + 1.0d0)
    enddo
end function
!##########################################


!##########################################
function rand_n(n) result(ret)
    integer(int32),intent(in) :: n
    type(Random_) :: random
    real(real64) :: ret(n)
    integer(int32) :: i

    do i=1,n
        ret(i) = random%random()
    enddo

end function
!##########################################


!##########################################
function rand_sz2(sz1,sz2) result(ret)
    integer(int32),intent(in) :: sz1,sz2
    type(Random_) :: random
    real(real64) :: ret(sz1,sz2)
    integer(int32) :: i,j
    do j=1,sz2
        do i=1,sz1
            ret(i,j) = random%random()
        enddo
    enddo

end function
!##########################################


!##########################################
function rand_sz3(sz1,sz2,sz3) result(ret)
    integer(int32),intent(in) :: sz1,sz2,sz3
    type(Random_) :: random
    real(real64) :: ret(sz1,sz2,sz3)
    integer(int32) :: i,j,k
    do k=1,sz3
        do j=1,sz2
            do i=1,sz1
                ret(i,j,k) = random%random()
            enddo
        enddo
    enddo

end function
!##########################################


end module