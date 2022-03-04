program main
    use MathClass
    implicit none
    
    ! computing derivative
    print *, d_dx(myfunc      ,x=0.10d0)
    print *, d_dx(my_vec_func ,x=[0.10d0, 2.0d0],dim_num=2)
    
contains

function myfunc(x) result(ret)
    real(real64),parameter :: a=2.0d0
    real(real64),parameter :: b=1.0d0
    real(real64),intent(in) :: x
    real(real64) :: ret
    ret = a*x*x + b
end function


function my_vec_func(x) result(ret)
    real(real64),parameter :: a=2.0d0
    real(real64),parameter :: b=1.0d0
    real(real64),intent(in) :: x(:)
    real(real64),allocatable :: ret(:)
    ret = a*x + b
end function

end program main