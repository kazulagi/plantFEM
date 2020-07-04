program main
    use fem
    implicit none

    real(real64),allocatable :: a(:,:),b(:,:)
    integer(int32) :: i
    ! load a .txt or .csv file.
    a = loadtxt("./","test",".txt")
    b = loadtxt("./","test",".txt")
    ! It shows the array.
    call showArray(a)
    ! compute 
    do i=1, 10
        a = a+1.0d0
        ! save array as a .txt, .csv, or .html file.
        call savetxt(a,"./","test2",".txt")
        call savetxt(a,"./","test2",".csv")
        call savetxt(a,"./","test2",".html")
        call savetxt(b,"./","test3",".txt")
        call savetxt(b,"./","test3",".csv")
        call savetxt(b,"./","test3",".html")
        call sleep(5)
    enddo

end program main