program main
    use ArrayClass
    implicit none

    type(Array_) :: A, B, C, I

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

end program