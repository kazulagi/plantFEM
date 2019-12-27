program main
    use iso_fortran_env
    implicit none

    integer(int32) :: i,Mat(3,3)

    Mat(:,1)=0
    Mat(:,2)=1
    Mat(:,3)=2
    open(10,file="test.txt")
    do i=1, 3
        write(10,'((i0.5) , (i0.5), (i0.5), (i0.5) , (i0.5) )') Mat(i,:)
    enddo
    close(10)
    call system("cat test.txt")
end program