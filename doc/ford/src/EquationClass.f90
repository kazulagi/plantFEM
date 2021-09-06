module EquationClass
    use iso_fortran_env
    use MathClass
    implicit none

    type :: Equation_point_
        real(real64),allocatable :: position(:)
    end type

    type :: Equation_
        real(real64) :: a, b, c, d
        real(real64),allocatable :: x(:)
        type(Equation_point_),allocatable :: points(:)
        logical :: LinearEquation, QuadraticEquation
    contains
        procedure, public :: setup => setupEquation
        procedure, public :: solve => solveEquation
    end type

contains

! ####################################
subroutine setupEquation(obj,LinearEquation,QuadraticEquation,a,b,c,d)
    class(Equation_),intent(inout) :: obj
    logical,optional,intent(in) :: LinearEquation,QuadraticEquation
    real(real64),optional,intent(in) :: a,b,c,d
    obj%LinearEquation=.false.
    obj%QuadraticEquation =.false.
    
    if(present(LinearEquation) )then
        if(LinearEquation .eqv. .true.)then
            if(allocated(obj%x) )then
                deallocate(obj%x)
            endif
            allocate(obj%x(1))
            obj%LinearEquation=.true.
            !allocate(obj%x(2) )
            obj%a = input(default=0.0d0, option=a)        
            obj%b = input(default=0.0d0, option=b)
            print *, "Linear Equation"
            print *, obj%a, "x", "+",obj%b,"= 0"
            ! obj%c = input(default=0.0d0, option=c)
            ! obj%d = input(default=0.0d0, option=d)   
            return
        endif
    endif

    if(present(QuadraticEquation))then
        if(QuadraticEquation .eqv. .true.)then
            if(allocated(obj%x) )then
                deallocate(obj%x)
            endif
            allocate(obj%x(2))
            obj%QuadraticEquation=.true.
            obj%a = input(default=0.0d0, option=a)        
            obj%b = input(default=0.0d0, option=b)        
            obj%c = input(default=0.0d0, option=c)
            print *, "Quadratic Equation"
            print *, obj%a, "x^2", "+",obj%b,"x +",obj%c,"= 0"
        endif
        return
    endif

end subroutine setupEquation
! ####################################

! ####################################
subroutine solveEquation(obj)
    class(Equation_),intent(inout) :: obj
    real(real64) :: Dval

    if(obj%LinearEquation .eqv. .true.)then
        if(obj%a ==0.0d0 .or. obj%a /= obj%a)then
            print *, "solveEquation :: ERROR :: ax +b =0, a=0"
            stop
        else
            obj%x(1) = - obj%b / obj%a
            print *, "x = ", obj%x(1)
        endif
        return
    endif
    if(obj%QuadraticEquation .eqv. .true.)then
        Dval = obj%b * obj%b - 4.0d0*obj%a*obj%c
        if(Dval < 0.0d0)then
            print *, "solveEquation :: ERROR :: D = b^2 - 4 a c < 0"
            stop
        else
            if(Dval == 0.0d0)then
                obj%x =  - obj%b/(2.0d0 * obj%a)
                print *, "x = ", obj%x(1)
            else
                obj%x(1) = (- obj%b + sqrt(Dval))/(2.0d0 * obj%a)
                obj%x(2) = (- obj%b - sqrt(Dval))/(2.0d0 * obj%a)
                print *, "x = ", obj%x(2)
            endif
        endif
        return
    endif
end subroutine
! ####################################


end module EquationClass