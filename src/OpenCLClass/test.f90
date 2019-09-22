
program test
    use MPIClass
    use fMathClass
    implicit none
  
    type(MPI_)::mpidata
    real(8),allocatable::a(:),b(:)
    real(8) :: dp,t0,t1,t2
    integer ::From,n
    

    call mpidata%start()
    From=0
    if(mpidata%Myrank==From)then
        print *, "Order! : "
        read(*,*) n
    endif
    call mpidata%Barrier()
    call mpidata%Bcast(From=From,int_val=n)
  
  
    allocate(a(n),b(n) )
    a(:)=1.0d0
    b(:)=2.0d0
  
    call cpu_time(t0)
    dp = dot_product(a,b)
    print *, dp
  
  
    call cpu_time(t1)
    dp = c_dot_product(a,b,n)
    print *, dp
  
  
    call cpu_time(t2)
  
    print *, "fdot_product",t1-t0,"sec."
    print *, "cdot_product",t2-t1,"sec."
    
    call mpidata%end()
   end program test
  