program main
    use ArrayClass
    implicit none

    type(Array_) :: A, B, C, I
    real(real64),allocatable :: vector(:)

    print *, " "
    call A%random(3,3)
    call B%zeros(3,3)
    call I%unit(3,3)

    C = A * B ! A_ij B_jk

    C = A + B ! A_ij + B_ij

    call A%print()
    call B%print()
    call C%print()
    call I%print()

    print *, dot(C, C)

    ! create vectors
    print *, "zeros"
    vector = zeros(5) ! Five 0
    print *, vector(:)
    
    ! np.arange
    print *, "arange"
    vector = arange(5) ! 1, 2, 3 , 4, 5
    print *, vector(:)
    
    ! np.arange
    print *, "arange : start, stop, step"
    vector = arange(10, 50, 5) ! 1, 2, 3 , 4, 5
    print *, vector(:)

end program