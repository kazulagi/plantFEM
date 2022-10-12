program main
    use MPIClass
    implicit none

    type(MPI_) :: mpid
    real(real64),allocatable :: sendbuf(:),recvbuf(:)
    integer(int32),allocatable :: send_recv_rank(:)
    integer(int32) :: DOF=10000

    call mpid%start()
    sendbuf = mpid%myrank*eyes(DOF)
    recvbuf = -1.0d0*eyes(DOF)
    send_recv_rank = int(zeros(DOF) )

    if(mpid%petot/=3) then
        print *, "This test is for petot=3"
        stop
    endif
    
    if(mpid%myrank==0)then
        send_recv_rank(1:DOF/2  ) = 1
        send_recv_rank(DOF/2+1: ) = 2
    elseif(mpid%myrank==1)then
        send_recv_rank(1:DOF/2  ) = 0
        send_recv_rank(DOF/2+1: ) = 1
    elseif(mpid%myrank==2)then
        send_recv_rank(1:DOF/2  ) = 2
        send_recv_rank(DOF/2+1: ) = 0
    endif
    
    
    call mpid%isend_irecv(sendbuf,recvbuf,send_recv_rank,debug=.true.)
    print *, "Isend & IRECV >> DONE!"
    
    !print *, mpid%myrank,":",sendbuf
    !print *, mpid%myrank,":",recvbuf
    
    print *, "[SCORE] :: error_norm = ",norm( recvbuf - dble(send_recv_rank) )," / myrank : ",mpid%myrank
    
    call mpid%end()
    
end program main