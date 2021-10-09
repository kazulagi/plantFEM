program main
    use std ! use standard libary of plantFEM
    implicit none
    ! start
    type(MPI_) :: mpid
    type(IO_) :: f
    integer(int32) :: fh ! file handle (id)
    integer(int32) :: val, from
    integer(int32) :: send_value(2)
    integer(int32),allocatable :: recv_values(:)

    ! start MPI
    call mpid%start()
    ! >>>>>>>> do parallel

    ! hello, world
    print *, "hello! world, my rank is ", mpid%myrank

    call mpid%initItr(8)
    print *,"myrank",mpid%myrank,"s,e = ",mpid%start_id,mpid%end_id
    print *, mpid%start_end_id(:)
    


    ! create files
    call f%open("./"//"FileFromRank"//trim(str(mpid%myrank))//".txt")
    write(f%fh,*) "MPI is running!"
    write(f%fh,*) "My rank is ",mpid%myrank
    write(f%fh,*) "Number of process is ",mpid%petot
    call f%close()






    ! ----> broadcast (sync) one data => to all nodes.
    val = mpid%myrank
    print *, "my value :: ",val, "my rank is : ", mpid%myrank
    ! broadcast => sync "val" of rank "0" node to all!
    from = 0
    call mpid%bcast(From=from,val=val)
    print *, "Sync! >> my value :: ",val, "my rank is : ", mpid%myrank





    ! ----> gather all values to rank 0 node
    allocate(recv_values(mpid%petot*2) )
    send_value(:) = mpid%myrank
    recv_values(:) = 0
    ! gether send_value of all nodes => to "recv_value(:) "
    call mpid%gather(sendobj=send_value(1:2),&
        recvobj=recv_values(1:),To=0)
    if(mpid%myrank == 0)then
        print *, "Gathered values : "
        print *, recv_values(:)
    endif




    ! Wait all process 
    call mpid%barrier()
    ! <<<<<<<< end do parallel
    call mpid%end()

    ! Importance Index 6 / 10 : [******    ]

end program 