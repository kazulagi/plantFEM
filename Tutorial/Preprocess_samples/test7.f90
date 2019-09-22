program main
    use RandomClass
    use ArrayOperationClass

    type(Random_)::random
    integer :: i,j
    Real(8) :: Array(100,3)
    integer :: ArrayInt(100,3)

    call random%init()    
    do i=1,100
        do j=1,3
            ArrayInt(i,j)=random%randint(1,100)
        enddo
    enddo

    call showArray(ArrayInt,Name="test.txt")

    
    
end program