module ODEClass
    use MathClass
    use ArrayClass
    implicit none

    type :: ODE_
        real(real64),allocatable :: x(:) ! time-dependent variables
        real(real64),allocatable :: A(:,:) ! coefficient matrix
        real(real64),allocatable :: const(:) ! constant term
        logical :: real_positive_value = .False.
    contains
        procedure,public :: init => init_ODEClass
        procedure,public :: set  => set_ODEClass
        procedure,public :: solve  => solve_ODEClass
    end type

contains


!> initialize variables & coefficient matrix
subroutine init_ODEClass(this,num_variable)
    class(ODE_),intent(inout) :: this
    integer(int32),intent(in) :: num_variable

    this%x = zeros(num_variable)
    this%A = zeros(num_variable,num_variable)
    this%const = zeros(num_variable)

end subroutine


!> set ODE
subroutine set_ODEClass(this,d_dt,params,const)
    class(ODE_),intent(inout) :: this
    integer(int32),intent(in) :: d_dt
    real(real64),intent(in)   :: params(:),const

    ! for example, 
    ! d_dt a_1 = a_1 + 0.2 a_2 - 0.1 a_4, for 4 variable-system of ODEs,
    ! d_dt = 1, params = (1.0d0, 0.20d0, 0.0d0, -0.10d0)
    this%A(d_dt,:) = params(:)
    this%const(d_dt) = const
end subroutine




!> solve ODE
function solve_ODEClass(this,dt) result(x)
    class(ODE_),intent(inout) :: this
    real(real64),intent(in)   :: dt
    real(real64),allocatable  :: x(:)
    integer(int32) :: i

    ! matrix exponential
    x = matmul(exp(dt*this%A,order=100),this%x) + exp(dt)*this%const
    
    if(this%real_positive_value)then
        do i=1,size(x)
            if(x(i) < 0.0d0)then
                x(i) = 0.0d0
            endif
        enddo
    endif

end function

end module ODEClass
