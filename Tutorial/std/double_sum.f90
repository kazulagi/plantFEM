program main
use SparseClass
implicit none

type(Math_)::math
! compute double sum, bigemimnal binal double sum
print *, "\sum\sum a_i,* b_j for i+j=k,  Exact solution"
print *, sum_sum(a=a,b=a,&
    a_params=[1.0d0,0.0d0]+0*math%i,&
    b_params=[1.0d0,0.0d0]+0*math%i,&
    i_plus_j=1,&
    ir=[0,10],&
    jr=[0,10] &
    ), cmplx(0.0d0)

print *, sum_sum(a=a,b=a,&
    a_params=[1.0d0,0.0d0]+0*math%i,&
    b_params=[1.0d0,0.0d0]+0*math%i,&
    i_plus_j=2,&
    ir=[0,10],&
    jr=[0,10] &
    ), cmplx(1.0d0)


print *, sum_sum(a=a,b=a,&
    a_params=[1.0d0,0.0d0]+0*math%i,&
    b_params=[1.0d0,0.0d0]+0*math%i,&
    i_plus_j=3,&
    ir=[0,10],&
    jr=[0,10] &
    ), cmplx(4.0d0)

print *, sum_sum(a=a,b=a,&
    a_params=[1.0d0,0.0d0]+0*math%i,&
    b_params=[1.0d0,0.0d0]+0*math%i,&
    i_plus_j=4,&
    ir=[0,10],&
    jr=[0,10] &
    ), cmplx(10.0d0)


print *, sum_sum(a=a,b=a,&
    a_params=[1.0d0,0.0d0]+0*math%i,&
    b_params=[1.0d0,0.0d0]+0*math%i,&
    i_plus_j=5,&
    ir=[0,10],&
    jr=[0,10] &
    ), cmplx(20.0d0)
contains

function a(k,params) result(ret)
    integer(int32),intent(in) :: k
    complex(real64),intent(in) :: params(:)
    complex(real64) :: ret

    ret = params(1)*k + params(2)
end function

end program