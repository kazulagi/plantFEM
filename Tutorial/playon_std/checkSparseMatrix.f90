program main
    use IOClass
    use randomclass
    use ArrayClass
    implicit none

    real(real64),allocatable :: a(:,:)
    integer(int32) :: i
    type(random_) :: random
    a = zeros(2000,2000)

    ! create a matrix
    do i=1, 2000
        a(i,i) = random%random()
    enddo

    do i=2, 1990
        a(i-1,i-1) = random%random()
        a(i-1,i+1) = random%random()
        a(i+1,i+1) = random%random()
        a(i+1,i-1) = random%random()
    enddo

    do i=3, 1990
        a(i-2,i-2) = random%random()
        a(i-2,i+2) = random%random()
        a(i+2,i+2) = random%random()
        a(i+2,i-2) = random%random()
    enddo

    do i=50, 1000
        a(i-40,i-40) = random%random()
        a(i-40,i+40) = random%random()
        a(i+40,i+40) = random%random()
        a(i+40,i-40) = random%random()
    enddo

    ! show sparse matrix (zero=> white, non-zero=>blue)
    call spy(a)

end program main
