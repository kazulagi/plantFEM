program doTest
    !$use omp_lib
    use plantFEM

    implicit none
    
    type(MPI_) :: mpid
    integer,parameter :: N = 10000
    integer i, a(N),nthreads,myid

    call mpid%start()
    print *, "Running MPI with OpenMP"

    !$omp parallel num_threads(2)
    nthreads = omp_get_num_threads()
    myid = omp_get_thread_num()
    print *, "nprocess :",trim(str(mpid%petot))," myrank :",trim(str(mpid%myrank)), &
        " nthreads : " ,trim(str(nthreads)), " myid :",trim(str(myid))
    do i=1,N
        a(i) = i
    end do

    do i=1,N
        a(i) = a(i)*2.0d0
    enddo
    !$omp end parallel
    
    call mpid%end()

end program
    

