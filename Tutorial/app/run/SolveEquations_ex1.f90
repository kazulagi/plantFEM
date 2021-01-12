program main 
    use std 
    implicit none 
    real(real64) :: x(3)  
    real(real64) :: A(3,3)  
    real(real64) :: b(3)
    A(1,1)=5.0d0 ; A(1,2)=-2.0d0 ; A(1,3)=5.0d0 ;  
    A(2,1)=4.4d0 ; A(2,2)=2.0d0  ; A(2,3)=20.0d0 ;  
    A(3,1)=-9.0d0; A(3,2)=-8.0d0 ; A(3,3)=1.0d0 ;  
    b(1) = 1.0d0 
    b(2) = 8.0d0 
    b(3) =-1.0d0 
    x(:)=0.0d0
    call gauss_jordan_pv(A, x, b, size(x) )
    call showArray(x)
    print *, "ERROR is ::",sqrt(dot_product(matmul(A,x)-b,matmul(A,x)-b))
end program main