program main
use ArrayClass
implicit none


! find minval/maximum value by binary search algorithm
do i_i=1,20,2
    print *, minvalx(fx=square_function,x_range=[1.0d0,4.0d0],depth=i_i)
enddo

contains

function square_function(x) result(ret)
    real(real64),intent(in) :: x
    real(real64) :: ret

    ret = abs(sin(x))

end function

end program
