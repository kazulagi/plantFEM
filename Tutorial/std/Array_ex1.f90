program main
    use fem
    implicit none

    real(real64),allocatable :: a(:,:)

    ! load a .txt or .csv file.
    a = loadtxt("./","test",".txt")
    ! It shows the array.
    call showArray(a)
    a = matmul(a,a)
    ! save array as a .txt, .csv, or .html file.
    call savetxt(a,"./","test2",".txt")
    call savetxt(a,"./","test2",".csv")
    call savetxt(a,"./","test2",".html")
end program main