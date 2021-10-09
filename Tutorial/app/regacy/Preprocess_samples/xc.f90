program main
    use plantfem
    implicit none

    integer,parameter::n=100
    real(8) :: a(n),b(n)
    real(8) :: dp,t0,t1,t2,t3

    a(:)=100.0d0
    b(:)=1.0d0

    call cpu_time(t0)
    dp=c_dot_product(a,b,n)
    print *,dp
    call cpu_time(t1)
    dp=dot_product(a,b)
    print *,dp
    call cpu_time(t2)

    print *, "ref.   : ",t2-t1,"sec."
    print *, "OpenCL : ",t1-t0,"sec."
    
    
    
end program