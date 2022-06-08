program main
    use MPIClass
    implicit none

    type(MPI_) :: mpid
    real(real64),allocatable :: sendbuf(:),recvbuf(:)
    integer(int32),allocatable :: send_recv_rank(:)
    integer(int32) :: DOF=1000

    call mpid%start()
    sendbuf = mpid%myrank*eyes(DOF)
    recvbuf = zeros(DOF)
    send_recv_rank = int(zeros(DOF) )

    if(mpid%petot/=2) then
        print *, "This test is for petot=2"
        stop
    endif
    
    if(mpid%myrank==0)then
        send_recv_rank(:) = 1
    elseif(mpid%myrank==1)then
        send_recv_rank(:) = 0
    endif
    call mpid%barrier()
    call mpid%isend_irecv(sendbuf,recvbuf,send_recv_rank,debug=.true.)
    print *, "Isend & IRECV >> DONE!"
    !print *, mpid%myrank,":",sendbuf
    !print *, mpid%myrank,":",recvbuf
    
    print *, "[SCORE] :: error_norm = ",norm( recvbuf - dble(send_recv_rank) )," / myrank : ",mpid%myrank
    
    call mpid%end()
    
end program main