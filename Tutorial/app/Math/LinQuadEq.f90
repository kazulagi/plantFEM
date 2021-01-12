program main
    use std
    implicit none

    type(Equation_) :: eq

    call eq%setup(QuadraticEquation=.true.,a=1.0d0, b=8.0d0, c=1.0d0)
    call eq%solve()
end program