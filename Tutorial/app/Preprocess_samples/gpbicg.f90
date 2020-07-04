program main
    use LinearSolverClass
    implicit none

    real(8),allocatable :: a(:,:),b(:),x(:)
    integer :: i,n,itrmax
    real(8) :: er

    open(10,file="gpbicg.txt")
    read(10,*) n

    allocate(a(n,n),b(n),x(n) )
    do i=1,n
        read(10,* ) a(i,:)
    enddo
    do i=1,n
        read(10,* ) b(i)
    enddo
    close(10)
    
    x(:)=1.0d0
    call GPBiCG(a,b,x,n)

    do i=1,n
        print *,  i,"|value ",x(i),"|residual ",b(i)-dot_product(a(i,:),x(:) )
    enddo
    
    call gauss_jordan_pv(a,x,b,n)
    
    do i=1,n
        print *,  i,"|value ",x(i),"|residual ",b(i)-dot_product(a(i,:),x(:) )
    enddo


    itrmax=1000
    er=1.0e-20
    x(:)=0.0d0
    call bicgstab1d(a,b,x,n,itrmax, er)
    
    
    do i=1,n
        print *,  i,"|value ",x(i),"|residual ",b(i)-dot_product(a(i,:),x(:) )
    enddo

end program main