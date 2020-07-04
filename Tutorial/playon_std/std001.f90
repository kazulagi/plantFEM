program main
    use std
    implicit none

    ! This example utilizes TermClass, where
    ! You can get string of technical terms.

    ! How to use:

    ! First, create the instance
    type(Term_) :: word

    ! then you can print registered words.
    ! (see src/TermClass/TermClass.f90)
    ! for instance, 

    print *, word%gmsh ! >> "Gmsh"

    ! If you want to add some technical term, please send me a pull-request.

    ! Actually, this is not so important.
    ! Importance Index 1 / 10 : [*         ]

end program main