program sort
    use randomClass
    use ArrayOperationClass
    implicit none

    type(Random_) :: random
    real(8)::a(10000)
    integer :: i

    call random%init()
    do i=1,10000
        a(i)=random%random()
    enddo

    open(10,file="before.txt")
    open(20,file="after.txt")

    do i=1,10000
        write(10,*)  dble(i), a(i)
    enddo
    call quicksort(a)
    
    do i=1,10000
        write(20,*) dble(i), a(i)
    enddo

    close(10)
    close(20)

end program
