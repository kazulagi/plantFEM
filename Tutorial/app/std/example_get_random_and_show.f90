program main
    use std
    implicit none
    
    type(random_):: random
    real(real64) :: array(4,4), val
    integer(int32) :: i,j

    call random%init()
    do i=1, 4
        do j=1, 4
            array(i,j) =  random%random()
        enddo
    enddo
    ! show array on display
    call showArray(array)
    ! export array to a file named "test.txt"
    call showArray(array,Name="test.txt")
    
end program main