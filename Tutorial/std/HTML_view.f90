program main
    use HTMLClass
    use TimeClass
    use RandomClass
    implicit none

    type(HTML_) :: html
    type(Time_) :: time
    type(Random_) :: random
    real(real64),allocatable :: matrix(:,:)
    integer(int32) :: i
    
    ! write info in index.html
    do i=1,100
        call html%init()
        call html%add("Hello, world!")
        call html%add(str(i))
        matrix = random%randn(5,5)
        call html%file%write(matrix)
        call html%show()
        call time%sleep(2)
    enddo
    
end program main