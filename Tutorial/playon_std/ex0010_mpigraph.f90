program main
    use std
    implicit none

    type(MPI_)   :: mpid
    type(Vertex_):: vertex(4)
    type(Graph_) :: graph
    type(IO_)    :: f
    integer(int32) :: i

    ! start mpi
    call mpid%start()

    ! locally created vertex data
    call vertex(1)%create(Name="process"//trim(str(mpid%myrank)),MyRank=mpid%myrank,&
        reval=dble(mpid%myrank*10+1) )
    call vertex(2)%create(Name="process"//trim(str(mpid%myrank)),MyRank=mpid%myrank,&
        reval=dble(mpid%myrank*10+2) )
    call vertex(3)%create(Name="process"//trim(str(mpid%myrank)),MyRank=mpid%myrank,&
        reval=dble(mpid%myrank*10+3) )
    if(mpid%myrank==0)then
        call vertex(4)%create(Name="process"//trim(str(mpid%myrank)),MyRank=mpid%myrank,&
        reval=dble(mpid%myrank*10+4)  )
    endif

    ! set meta-data (vertex-data)
    call graph%add(vertex=vertex(1) )
    call graph%add(vertex=vertex(2) )
    call graph%add(vertex=vertex(3) )
    if(mpid%myrank==0)then
        call graph%add(vertex=vertex(4) )
    endif

    ! set meta-data (edge-data)
    call graph%add(from=1,to=2)
    call graph%add(from=1,to=3)
    call graph%add(from=2,to=3)
    if(mpid%myrank==0)then
        call graph%add(from=3,to=4)
    endif


    ! sync meta-data
    call mpid%merge(graph)


    ! create edge from local-id 1 to global-id 2
    call graph%add(from=graph%global_ID(1),to=1)

    ! sync meta-data
    call mpid%sync(graph)

    print *, "Myrank: "//trim(str(mpid%myrank))//"| Global IDs",graph%global_id(:)


    call graph%show()
    ! end mpi
    call mpid%end()

end program main