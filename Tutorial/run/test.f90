program main
    use MPIClass

    type(MPI_) :: mpiobj
    real(8),allocatable::glo(:),loc(:)
    integer :: From
    real(8)::val

    From=0
    call mpiobj%start()
    allocate(glo(mpiobj%petot),loc(1) )
    glo(:)=dble(mpiobj%MyRank)
    loc(:)=0.0d0


    call mpiobj%read(val,Msg="Input real value")
    call mpiobj%Bcast(From=From,val=val)
    print *, mpiobj%MyRank,"/",mpiobj%petot,val
    call mpiobj%barrier()
    call mpiobj%AlltoAll(sendobj=glo(:),sendcount=1,&
        recvobj=glo(:),recvcount=1)
    print *, glo(:)
    call mpiobj%end()

end program main
