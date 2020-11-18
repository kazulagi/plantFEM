module MPIClass
    use, intrinsic :: iso_fortran_env
    use mpi
    use MathClass
    use ArrayClass
    use GraphClass
    implicit none


    !interface BcastMPI
    !    module procedure BcastMPIReal, BcastMPIInt
    !end interface

    type :: comment_
        character*200 :: comment
    endtype
    
    type:: MPI_
        integer(int32) :: ierr
        integer(int32) :: MyRank
        integer(int32) :: PeTot
        integer(int32) :: Comm1
        integer(int32) :: Comm2
        integer(int32) :: Comm3
        integer(int32) :: Comm4
        integer(int32) :: Comm5
        integer(int32) :: start_id, end_id
        integer(int32),allocatable::start_end_id(:)
        integer(int32),allocatable::Comm(:),key(:)
        integer(int32),allocatable::local_ID(:),Global_ID(:)
        integer(int32),allocatable::Stack(:,:),localstack(:)
        integer(int32) :: LapTimeStep
        real(real64) :: stime
        real(real64) :: etime
        real(real64) :: laptime(1000)
        character(200) :: name
        type(comment_) :: comments(1000)
        type(Graph_) :: graph
    contains
        procedure :: Start => StartMPI
        procedure :: initItr => initItrMPI
        procedure :: Barrier => BarrierMPI
        procedure, Pass ::  readMPIInt
        procedure, Pass ::  readMPIReal
        generic ::  read =>   readMPIInt,readMPIReal
        
        procedure, Pass :: BcastMPIInt
        procedure, Pass :: BcastMPIIntVec
        procedure, Pass :: BcastMPIIntArray
        procedure, Pass :: BcastMPIReal
        procedure, Pass :: BcastMPIRealVec
        procedure, Pass :: BcastMPIRealArray
        procedure, Pass :: BcastMPIChar
        generic  :: Bcast => BcastMPIInt, BcastMPIReal,BcastMPIChar,BcastMPIIntVec,&
            BcastMPIIntArray,BcastMPIRealVec,BcastMPIRealArray

        procedure, Pass :: GatherMPIInt 
        procedure, Pass :: GatherMPIReal 
        generic :: Gather => GatherMPIInt, GatherMPIReal 


        procedure, Pass :: ScatterMPIInt 
        procedure, Pass :: ScatterMPIReal 
        generic :: Scatter => ScatterMPIInt, ScatterMPIReal 
 

        procedure, Pass :: AllGatherMPIInt 
        procedure, Pass :: AllGatherMPIReal 
        procedure, Pass :: AllGatherMPIGraph 
        generic :: AllGather => AllGatherMPIInt, AllGatherMPIReal,AllGatherMPIGraph
        generic :: merge => AllGatherMPIGraph

        procedure, Pass :: AlltoAllMPIInt 
        procedure, Pass :: AlltoAllMPIReal 
        generic :: AlltoAll => AlltoAllMPIInt, AlltoAllMPIReal 
        

        procedure, Pass :: ReduceMPIInt 
        procedure, Pass :: ReduceMPIReal 
        generic :: Reduce => ReduceMPIInt, ReduceMPIReal 

        procedure, Pass :: AllReduceMPIInt 
        procedure, Pass :: AllReduceMPIReal 
        generic :: AllReduce => AllReduceMPIInt, AllReduceMPIReal 

        procedure :: createStack => createStackMPI
        procedure :: showStack   => showStackMPI
        procedure :: free  => freeMPI 
        procedure :: split => splitMPI 
        procedure :: copy  => copyMPI 
        procedure :: End => EndMPI
        procedure :: getLapTime => getLapTimeMPI
        procedure :: showLapTime => showLapTimeMPI
        procedure :: GetInfo => GetMPIInfo
        procedure :: createFileName => createFileNameMPI


        procedure, Pass :: syncGraphMPI
        generic :: sync => syncGraphMPI

    end type    
contains



!################################################################
subroutine StartMPI(obj,NumOfComm)
    class(MPI_),intent(inout)::obj
    integer(int32),optional,intent(in)::NumOfComm

    call mpi_init(obj%ierr)
    call mpi_comm_size(mpi_comm_world,obj%Petot ,obj%ierr)
    call mpi_comm_rank(mpi_comm_world,obj%MyRank,obj%ierr)


    allocate(obj%Comm(input(default=100,option=NumOfComm)  ) )
    allocate(obj%key(input(default=100,option=NumOfComm)  ) )
    obj%Comm(:)=MPI_COMM_WORLD
    obj%key(:)=0.0d0
    obj%stime = mpi_wtime()
    obj%laptime(:) = 0.0d0
    obj%LapTimeStep = 1
    obj%laptime(obj%LapTimeStep)=MPI_Wtime()
    obj%comments%comment(:)="No comment"

    print *, "Number of Core is ",obj%Petot

end subroutine
!################################################################


!################################################################
subroutine initItrMPI(obj,total_iteration)
    class(MPI_),intent(inout) :: obj
    integer(int32),intent(in) :: total_iteration
    integer(int32) :: petot, modval,divval,start_id, end_id,i

    ! from  1 to total_iteration
    modval = mod(total_iteration,obj%petot)
    divval = total_iteration/obj%petot
    if(obj%myrank+1 <= modval)then
        obj%start_id = obj%myrank*(divval+1)+1
        obj%end_id   = obj%myrank*(divval+1)+(divval+1)
    else
        obj%start_id = modval*(divval+1) + (obj%myrank+1-modval-1)*(divval)+1
        obj%end_id   = modval*(divval+1) + (obj%myrank+1-modval)*(divval)
    endif
    
    if(allocated(obj%start_end_id) )then
        deallocate(obj%start_end_id)
    endif

    allocate(obj%start_end_id(total_iteration))
    do i=1,obj%petot
        if(i <= modval)then
            start_id = (i-1)*(divval+1)+1
            end_id   = (i-1)*(divval+1)+(divval+1)
            obj%start_end_id(start_id:end_id)=i
        else
            start_id = modval*(divval+1) + (i-modval-1)*(divval)+1
            end_id   = modval*(divval+1) + (i-modval)*(divval)
            obj%start_end_id(start_id:end_id)=i
        endif
    enddo
end subroutine
!################################################################


!################################################################
subroutine createFileNameMPI(obj,Path,Name)
    class(MPI_),intent(inout) :: obj
    character(*),intent(in) :: Path,Name
    integer :: i, access
    
    i=access(trim(Path)//trim(adjustl(fstring(obj%MyRank)))," ")
    if(i/=0)then
        call system("mkdir "//trim(Path)//trim(adjustl(fstring(obj%MyRank))))
    endif
    obj%name=trim(Path)//trim(adjustl(fstring(obj%MyRank)))//"/"&
        //Name//trim(adjustl(fstring(obj%MyRank)))

end subroutine
!################################################################


!################################################################
subroutine createStackMPI(obj,total)
    class(MPI_),intent(inout) :: obj
    integer(int32),intent(in) :: total
    integer(int32) :: i,j,LocalStacksize,itr,locstacksize

    if(allocated(obj%Stack ))then
        deallocate(obj%Stack)
    endif
    LocalStacksize=int(dble(total)/dble(obj%Petot))+1

    allocate(obj%Stack(obj%petot,LocalStacksize) )

    itr=1
    locstacksize=0
    obj%Stack(:,:)=0
    do j=1,size(obj%Stack,2)
        do i=1,size(obj%Stack,1)
            obj%Stack(i,j)=itr
            itr=itr+1
            if(itr==total+1)then
                exit
            endif
        enddo
        if(itr==total+1)then
            exit
        endif
    enddo

    j= countif(Array=obj%Stack(obj%MyRank+1,:),Equal=.true.,Value=0)

    if(allocated(obj%localstack) )then
        deallocate(obj%localstack)
    endif
    allocate(obj%localstack(LocalStacksize-j))
    do i=1,size(obj%localstack)
        obj%localstack(i)=obj%stack(obj%MyRank+1,i)
    enddo

end subroutine
!################################################################



!################################################################
subroutine showStackMPI(obj)
    class(MPI_),intent(inout) :: obj
    integer(int32) :: i,j,n

    if(.not.allocated(obj%Stack) )then
        print *, "No stack is set"
        return
    else
        call obj%Barrier()
        do i=1,obj%Petot
            if(obj%MyRank+1==i)then
                print *, "MyRank",obj%MyRank,"Stack :: ",obj%localstack(:)
            endif
        enddo
    endif



end subroutine
!################################################################


!################################################################
subroutine readMPIInt(obj,val,ExecRank,Msg)
    class(MPI_),intent(inout)::obj
    integer(int32),optional,intent(in)::ExecRank
    character(*),optional,intent(in)::Msg
    integer(int32),intent(out)::val
    integer(int32) :: i,j,n


    n=input(default=0,option=ExecRank)
    if(obj%MyRank==n)then
        print *, input(default=" ",option=Msg)
        read(*,*) val
    endif
    call obj%Barrier()

end subroutine 
!################################################################


!################################################################
subroutine readMPIReal(obj,val,ExecRank,Msg)
    class(MPI_),intent(inout)::obj
    integer(int32),optional,intent(in)::ExecRank
    character(*),optional,intent(in)::Msg
    real(real64),intent(out)::val
    character*200 :: Massage
    integer(int32) :: i,j,n


    n=input(default=0,option=ExecRank)
    if(obj%MyRank==n)then
        print *, input(default=Massage,option=Msg)
        read(*,*) val
    endif
    call obj%Barrier()

end subroutine 
!################################################################



!################################################################
subroutine GetMPIInfo(obj)
    class(MPI_),intent(inout)::obj
    
    call mpi_comm_size(mpi_comm_world,obj%Petot ,obj%ierr)
    call mpi_comm_rank(mpi_comm_world,obj%MyRank,obj%ierr)
    
end subroutine
!################################################################


!################################################################
subroutine BarrierMPI(obj)
    class(MPI_),intent(inout)::obj
    integer(int32) :: i

    call MPI_barrier(mpi_comm_world,obj%ierr)
end subroutine
!################################################################


! All to All 

!################################################################
subroutine BcastMPIInt(obj,From,val)
    class(MPI_),intent(inout)::obj
    integer(int32),intent(inout)::From,val
    integer(int32) :: i

    call MPI_Bcast(val, 1, MPI_integer, From, MPI_COMM_WORLD, obj%ierr)
end subroutine
!################################################################


!################################################################
subroutine BcastMPIIntVec(obj,From,val)
    class(MPI_),intent(inout)::obj
    integer(int32),intent(inout) :: From
    integer(int32),allocatable,intent(inout)::val(:)
    integer(int32) :: i,j,n,vec_size
    integer(int32) :: sendval

    if(allocated(val) .and. From/=obj%myrank )then
        deallocate(val)
    endif

    vec_size=0
    if(From==obj%myrank )then
        vec_size = size(val)
    endif
    call obj%Bcast(From=From, val=vec_size)

    if(From/=obj%myrank )then
        allocate(val(vec_size) )
    endif

    sendval=0
    do i=1,vec_size
        call MPI_Bcast(val(i), 1, MPI_integer, From, MPI_COMM_WORLD, obj%ierr)
    enddo

end subroutine
!################################################################




!################################################################
subroutine BcastMPIIntArray(obj,From,val)
    class(MPI_),intent(inout)::obj
    integer(int32),intent(inout) :: From
    integer(int32),allocatable,intent(inout)::val(:,:)
    integer(int32) :: i,j,n,vec_size1,vec_size2
    integer(int32) :: sendval

    if(allocated(val) .and. From/=obj%myrank )then
        deallocate(val)
    endif

    vec_size1=0
    vec_size2=0
    if(From==obj%myrank )then
        vec_size1 = size(val,1)
    endif
    call obj%Bcast(From=From, val=vec_size1)
    if(From==obj%myrank )then
        vec_size2 = size(val,2)
    endif
    call obj%Bcast(From=From, val=vec_size2)

    if(From/=obj%myrank )then
        allocate(val(vec_size1, vec_size2) )
    endif

    sendval=0
    do i=1,vec_size1
        do j=1, vec_size2
            call MPI_Bcast(val(i,j), 1, MPI_integer, From, MPI_COMM_WORLD, obj%ierr)
        enddo
    enddo

end subroutine
!################################################################



!################################################################
subroutine BcastMPIReal(obj,From,val)
    class(MPI_),intent(inout)::obj
    integer(int32),intent(inout)::From 
    real(real64),intent(inout)::val
    integer(int32) :: i

    call MPI_Bcast(val, 1, MPI_REAL8, From, MPI_COMM_WORLD, obj%ierr)
end subroutine
!################################################################




!################################################################
subroutine BcastMPIRealVec(obj,From,val)
    class(MPI_),intent(inout)::obj
    integer(int32),intent(inout) :: From
    real(real64),allocatable,intent(inout)::val(:)
    integer(int32) :: i,j,n,vec_size
    

    if(allocated(val) .and. From/=obj%myrank )then
        deallocate(val)
    endif

    vec_size=0
    if(From==obj%myrank )then
        vec_size = size(val)
    endif
    call obj%Bcast(From=From, val=vec_size)

    if(From/=obj%myrank )then
        allocate(val(vec_size) )
    endif

    
    do i=1,vec_size
        call MPI_Bcast(val(i), 1, MPI_REAL8, From, MPI_COMM_WORLD, obj%ierr)
    enddo

end subroutine
!################################################################




!################################################################
subroutine BcastMPIRealArray(obj,From,val)
    class(MPI_),intent(inout)::obj
    integer(int32),intent(inout) :: From
    real(real64),allocatable,intent(inout)::val(:,:)
    integer(int32) :: i,j,n,vec_size1,vec_size2

    if(allocated(val) .and. From/=obj%myrank )then
        deallocate(val)
    endif

    vec_size1=0
    vec_size2=0
    if(From==obj%myrank )then
        vec_size1 = size(val,1)
    endif
    call obj%Bcast(From=From, val=vec_size1)
    if(From==obj%myrank )then
        vec_size2 = size(val,2)
    endif
    call obj%Bcast(From=From, val=vec_size2)

    if(From/=obj%myrank )then
        allocate(val(vec_size1, vec_size2) )
    endif

    do i=1,vec_size1
        do j=1, vec_size2
            call MPI_Bcast(val(i,j), 1, MPI_integer, From, MPI_COMM_WORLD, obj%ierr)
        enddo
    enddo

end subroutine
!################################################################


subroutine BcastMPIChar(obj,From,val)
    class(MPI_),intent(inout)::obj
    integer(int32),intent(inout)::From 
    character(*),intent(inout)::val
    character(200)::val200
    integer(int32) :: i

    val200=trim(val)
    call MPI_Bcast(val200(1:200), 200, MPI_CHARACTER, From, MPI_COMM_WORLD, obj%ierr)
    val=trim(val200)

end subroutine

!################################################################
subroutine GatherMPIInt(obj,sendobj,sendcount,recvobj,recvcount,&
    send_start_id,recv_start_id,To)
    class(MPI_),intent(inout)::obj
    integer(int32),intent(inout)::sendobj(:),recvobj(:)
    integer(int32),optional,intent(in)::sendcount,recvcount
    integer(int32)::sendcountv,recvcountv
    integer(int32),optional,intent(in)::send_start_id,recv_start_id,To
    integer(int32) :: i,s_start_id,r_start_id,ToID

    sendcountv=input(default=size(sendobj),option=sendcount )
    recvcountv=input(default=size(sendobj),option=recvcount )

    s_start_id=input(default=1,option=send_start_id)
    r_start_id=input(default=1,option=recv_start_id)
    ToID=input(default=0,option=To)

    call MPI_Gather(sendobj(s_start_id), sendcountv, MPI_integer, recvobj(r_start_id)&
    , recvcountv, MPI_integer, ToID ,MPI_COMM_WORLD, obj%ierr)
end subroutine
!################################################################


!################################################################
subroutine GatherMPIReal(obj,sendobj,sendcount,recvobj,recvcount,&
    send_start_id,recv_start_id,To)
    class(MPI_),intent(inout)::obj
    real(real64),intent(inout)::sendobj(:),recvobj(:)
    integer(int32),optional,intent(in)::sendcount,recvcount
    integer(int32)::sendcountv,recvcountv
    integer(int32),optional,intent(in)::send_start_id,recv_start_id,To
    integer(int32) :: i,s_start_id,r_start_id,ToID

    sendcountv=input(default=size(sendobj),option=sendcount )
    recvcountv=input(default=size(sendobj),option=recvcount )
    
    s_start_id=input(default=1,option=send_start_id)
    r_start_id=input(default=1,option=recv_start_id)
    ToID=input(default=0,option=To)

    call MPI_Gather(sendobj(s_start_id), sendcountv, MPI_REAL8, recvobj(r_start_id)&
    , recvcountv, MPI_REAL8, ToID, MPI_COMM_WORLD, obj%ierr)
end subroutine
!################################################################





!################################################################
subroutine ScatterMPIInt(obj,sendobj,sendcount,recvobj,recvcount,&
    send_start_id,recv_start_id,From)
    class(MPI_),intent(inout)::obj
    integer(int32),intent(inout)::sendobj(:),recvobj(:)
    integer(int32),intent(in)::sendcount,recvcount
    integer(int32),optional,intent(in)::send_start_id,recv_start_id,From
    integer(int32) :: i,s_start_id,r_start_id,FromID

    s_start_id=input(default=1,option=send_start_id)
    r_start_id=input(default=1,option=recv_start_id)
    FromID=input(default=0,option=From)

    call MPI_Scatter(sendobj(s_start_id), sendcount, MPI_integer, recvobj(r_start_id)&
    , recvcount, MPI_integer, FromID, MPI_COMM_WORLD, obj%ierr)
end subroutine
!################################################################


!################################################################
subroutine ScatterMPIReal(obj,sendobj,sendcount,recvobj,recvcount,&
    send_start_id,recv_start_id,From)
    class(MPI_),intent(inout)::obj
    real(real64),intent(inout)::sendobj(:),recvobj(:)
    integer(int32),intent(in)::sendcount,recvcount
    integer(int32),optional,intent(in)::send_start_id,recv_start_id,From
    integer(int32) :: i,s_start_id,r_start_id,FromID

    s_start_id=input(default=1,option=send_start_id)
    r_start_id=input(default=1,option=recv_start_id)
    FromID=input(default=0,option=From)

    call MPI_Scatter(sendobj(s_start_id), sendcount, MPI_REAL8, recvobj(r_start_id)&
    , recvcount, MPI_REAL8, FromID, MPI_COMM_WORLD, obj%ierr)
end subroutine
!################################################################






!################################################################
subroutine AllGatherMPIInt(obj,sendobj,sendcount,recvobj,recvcount,&
    send_start_id,recv_start_id)
    class(MPI_),intent(inout)::obj
    integer(int32),intent(inout)::sendobj(:),recvobj(:)
    integer(int32),intent(in)::sendcount,recvcount
    integer(int32),optional,intent(in)::send_start_id,recv_start_id
    integer(int32) :: i,s_start_id,r_start_id

    s_start_id=input(default=1,option=send_start_id)
    r_start_id=input(default=1,option=recv_start_id)

    call MPI_AllGather(sendobj(s_start_id), sendcount, MPI_integer, recvobj(r_start_id)&
    , recvcount, MPI_integer, MPI_COMM_WORLD, obj%ierr)
end subroutine
!################################################################


!################################################################
subroutine AllGatherMPIReal(obj,sendobj,sendcount,recvobj,recvcount,&
    send_start_id,recv_start_id)
    class(MPI_),intent(inout)::obj
    real(real64),intent(inout)::sendobj(:),recvobj(:)
    integer(int32),intent(in)::sendcount,recvcount
    integer(int32),optional,intent(in)::send_start_id,recv_start_id
    integer(int32) :: i,s_start_id,r_start_id

    s_start_id=input(default=1,option=send_start_id)
    r_start_id=input(default=1,option=recv_start_id)

    call MPI_AllGather(sendobj(s_start_id), sendcount, MPI_REAL8, recvobj(r_start_id)&
    , recvcount, MPI_REAL8, MPI_COMM_WORLD, obj%ierr)
end subroutine
!################################################################


!################################################################
subroutine AllGatherMPIGraph(obj,graph)
    class(MPI_),intent(inout)::obj
    type(Graph_),intent(inout) :: graph
    type(Vertex_),allocatable :: vertex(:)
    integer(int32),allocatable::sendobj(:),recvobj(:)
    real(real64),allocatable::sendobj_r(:),recvobj_r(:)
    
    real(real64) :: reval,x,y,z
    real(real64),allocatable :: reval_s(:),x_s(:),y_s(:),z_s(:)

    integer(int32) :: intval,ID,MyRank
    integer(int32),allocatable :: intval_s(:),ID_s(:),MyRank_s(:)

    integer(int32),allocatable::num_of_data(:),AdjacencyMatrix(:,:),AdjacencyData(:,:)
    integer(int32)::sendcount,recvcount
    integer(int32)::send_start_id,recv_start_id,sender_rank
    integer(int32) :: i,j,jj,k,s_start_id,r_start_id,numofvertex,totalnumvertex

    character(200) :: name

    ! graph1  => graph1 + graph2 + graph3 +graph4
    ! graph2  => graph1 + graph2 + graph3 +graph4
    ! graph3  => graph1 + graph2 + graph3 +graph4
    ! graph4  => graph1 + graph2 + graph3 +graph4
    
    ! get number of vertex.
    if(allocated(obj%Global_ID ) ) deallocate(obj%Global_ID)
    if(allocated(graph%Global_ID ) ) deallocate(graph%Global_ID)
    allocate(obj%Global_ID(size(graph%vertex)) )
    allocate(graph%Global_ID(size(graph%vertex)) )
    obj%Global_ID(:)=0
    graph%Global_ID(:)=0
    

    totalnumvertex=0
    allocate(num_of_data(obj%petot) )
    do i=1,obj%petot
        numofvertex=size(graph%vertex)
        sender_rank=i-1
        call obj%Bcast(From=sender_rank,val=numofvertex)
        num_of_data(i)=numofvertex
        totalnumvertex=totalnumvertex+numofvertex
    enddo

    print *, "My rank is ",obj%myrank,"/toral vertex is :: ",totalnumvertex
    


    allocate(recvobj(totalnumvertex) )
    allocate(sendobj( size(graph%vertex) ) )
    allocate(recvobj_r(totalnumvertex) )
    allocate(sendobj_r( size(graph%vertex) ) )
    !allocate(reval_s(totalnumvertex) )
    !allocate(x_s(totalnumvertex) )
    !allocate(y_s(totalnumvertex) )
    !allocate(z_s(totalnumvertex) )
    !allocate( intval_s(totalnumvertex) )
    !allocate( ID_s(totalnumvertex) )
    !allocate( MyRank_s(totalnumvertex) )
    allocate( vertex(totalnumvertex) )
    allocate( AdjacencyMatrix(totalnumvertex,totalnumvertex) )
    AdjacencyMatrix(:,:)=0
    !sendobj(:)=1

    ! allgather vertex
    ! vertex%reval
    reval=0.0d0
    k=1
    do i=1, obj%petot
        sender_rank=i-1
        do j=1,num_of_data(i)
            if(i-1==obj%myrank)then
                reval=graph%vertex(j)%reval
            endif
            call obj%Bcast(From=sender_rank,val=reval)
            vertex(k)%reval=reval
            k=k+1
        enddo
    enddo

    ! vertex%x
    x=0.0d0
    k=1
    do i=1, obj%petot
        sender_rank=i-1
        do j=1,num_of_data(i)
            if(i-1==obj%myrank)then
                x=graph%vertex(j)%x
            endif
            call obj%Bcast(From=sender_rank,val=x)
            vertex(k)%x=x
            k=k+1
        enddo
    enddo

    ! vertex%y
    y=0.0d0
    k=1
    do i=1, obj%petot
        sender_rank=i-1
        do j=1,num_of_data(i)
            if(i-1==obj%myrank)then
                y=graph%vertex(j)%y
            endif
            call obj%Bcast(From=sender_rank,val=y)
            vertex(k)%y=y
            k=k+1
        enddo
    enddo

    ! vertex%z
    z=0.0d0
    k=1
    do i=1, obj%petot
        sender_rank=i-1
        do j=1,num_of_data(i)
            if(i-1==obj%myrank)then
                z=graph%vertex(j)%z
            endif
            call obj%Bcast(From=sender_rank,val=z)
            vertex(k)%z=z
            k=k+1
        enddo
    enddo

    ! vertex%intval
    intval=0
    k=1
    do i=1, obj%petot
        sender_rank=i-1
        do j=1,num_of_data(i)
            if(i-1==obj%myrank)then
                intval=graph%vertex(j)%intval
            endif
            call obj%Bcast(From=sender_rank,val=intval)
            vertex(k)%intval=intval
            k=k+1
        enddo
    enddo

    ! vertex%ID
    ID=0
    k=1
    do i=1, obj%petot
        sender_rank=i-1
        do j=1,num_of_data(i)
            if(i-1==obj%myrank)then
                ID=graph%vertex(j)%ID
            endif
            call obj%Bcast(From=sender_rank,val=ID)
            vertex(k)%ID=ID
            k=k+1
        enddo
    enddo

    ! vertex%Myrank
    Myrank=0
    k=1
    do i=1, obj%petot
        sender_rank=i-1
        do j=1,num_of_data(i)
            if(i-1==obj%myrank)then
                Myrank=graph%vertex(j)%Myrank
            endif
            call obj%Bcast(From=sender_rank,val=Myrank)
            vertex(k)%Myrank=Myrank
            k=k+1
        enddo
    enddo

    ! vertex%name and Global_ID
    name="NoName"
    k=1
    do i=1, obj%petot
        sender_rank=i-1
        do j=1,num_of_data(i)
            if(i-1==obj%myrank)then
                name=graph%vertex(j)%name
                obj%Global_ID(j)=k
                graph%Global_ID(j)=k
            endif
            call obj%Bcast(From=sender_rank,val=name)
            vertex(k)%name=name
            k=k+1
        enddo
    enddo

    ! vertex%AdjacencyMatrix(:,:)
    ! あと、IDの振り直しを行うなど。

    intval=0
    k=0
    do i=1, obj%petot
        sender_rank=i-1

        do j=1,num_of_data(i)
            do jj=1,num_of_data(i)
                if(i-1==obj%myrank)then
                    intval=graph%AdjacencyMatrix(j,jj)
                endif
                call obj%Bcast(From=sender_rank,val=intval)
                AdjacencyMatrix(k+j,k+jj)=intval
            enddo
        enddo
        k=k+num_of_data(i)
    enddo

    graph%AdjacencyMatrix = AdjacencyMatrix
    graph%Vertex          = vertex
    graph%NumOfVertex     = totalnumvertex

end subroutine
!################################################################





!################################################################
subroutine AlltoAllMPIInt(obj,sendobj,sendcount,recvobj,recvcount,&
    send_start_id,recv_start_id)
    class(MPI_),intent(inout)::obj
    integer(int32),intent(inout)::sendobj(:),recvobj(:)
    integer(int32),intent(in)::sendcount,recvcount
    integer(int32),optional,intent(in)::send_start_id,recv_start_id
    integer(int32) :: i,s_start_id,r_start_id

    s_start_id=input(default=1,option=send_start_id)
    r_start_id=input(default=1,option=recv_start_id)

    call MPI_AlltoAll(sendobj(s_start_id), sendcount, MPI_integer, recvobj(r_start_id)&
    , recvcount, MPI_integer, MPI_COMM_WORLD, obj%ierr)
end subroutine
!################################################################


!################################################################
subroutine AlltoAllMPIReal(obj,sendobj,sendcount,recvobj,recvcount,&
    send_start_id,recv_start_id)
    class(MPI_),intent(inout)::obj
    real(real64),intent(inout)::sendobj(:),recvobj(:)
    integer(int32),intent(in)::sendcount,recvcount
    integer(int32),optional,intent(in)::send_start_id,recv_start_id
    integer(int32) :: i,s_start_id,r_start_id

    s_start_id=input(default=1,option=send_start_id)
    r_start_id=input(default=1,option=recv_start_id)

    call MPI_AlltoAll(sendobj(s_start_id), sendcount, MPI_REAL8, recvobj(r_start_id)&
    , recvcount, MPI_REAL8, MPI_COMM_WORLD, obj%ierr)
end subroutine
!################################################################




!################################################################
subroutine ReduceMPIInt(obj,sendobj,recvobj,count,start,To,&
    max,min,sum,prod,land,band,lor,bor,lxor,bxor,maxloc,minloc)
    class(MPI_),intent(inout)::obj
    integer(int32),intent(inout)::sendobj(:),recvobj(:)
    integer(int32),intent(in)::count
    integer(int32)  :: ToID,start_id
    integer(int32),optional,intent(in)::start,To
    logical,optional,intent(in)::max,min,sum,prod,land,band,lor
    logical,optional,intent(in)::bor,lxor,bxor,maxloc,minloc

    ToID=input(default=0,option=To)
    start_id=input(default=1,option=start)
    if(present(max) )then
        if(max .eqv. .true.)then

            call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_integer, ToID, MPI_MAX, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(min) )then
        if(min .eqv. .true.)then
            
            call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_integer, ToID, MPI_MIN, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(sum) )then
        if(sum .eqv. .true.)then
            
            call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_integer, ToID, MPI_SUM, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(prod) )then
        if(prod .eqv. .true.)then
            
            call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_integer, ToID, MPI_PROD, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(land) )then
        if(land .eqv. .true.)then
            
            call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_integer, ToID, MPI_LAND, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(band) )then
        if(band .eqv. .true.)then
            
            call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_integer, ToID,MPI_BAND , MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(lor) )then
        if(lor .eqv. .true.)then
            
            call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_integer, ToID, MPI_LOR, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(bor) )then
        if(bor .eqv. .true.)then
            
            call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_integer, ToID,MPI_BOR , MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(lxor) )then
        if(lxor .eqv. .true.)then
            
            call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_integer, ToID, MPI_LXOR, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(bxor) )then
        if(bxor .eqv. .true.)then
            
            call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_integer, ToID, MPI_BXOR, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(maxloc) )then
        if(maxloc .eqv. .true.)then
            
            call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_integer, ToID, MPI_MAXLOC, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(minloc) )then
        if(minloc .eqv. .true.)then
            call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_integer, ToID, MPI_MINLOC, MPI_COMM_WORLD, obj%ierr)
        endif
    endif

end subroutine
!################################################################


!################################################################
subroutine ReduceMPIReal(obj,sendobj,recvobj,count,start,To,&
    max,min,sum,prod,land,band,lor,bor,lxor,bxor,maxloc,minloc)
    class(MPI_),intent(inout)::obj
    real(real64),intent(inout)::sendobj(:),recvobj(:)
    integer(int32),intent(in)::count
    integer(int32)  :: ToID,start_id
    integer(int32),optional,intent(in)::start,To
    logical,optional,intent(in)::max,min,sum,prod,land,band,lor
    logical,optional,intent(in)::bor,lxor,bxor,maxloc,minloc

    ToID=input(default=0,option=To)
    start_id=input(default=1,option=start)
    if(present(max) )then
        if(max .eqv. .true.)then

            call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_REAL8, ToID, MPI_MAX, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(min) )then
        if(min .eqv. .true.)then
            
            call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_REAL8, ToID, MPI_MIN, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(sum) )then
        if(sum .eqv. .true.)then
            
            call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_REAL8, ToID, MPI_SUM, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(prod) )then
        if(prod .eqv. .true.)then
            
            call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_REAL8, ToID, MPI_PROD, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(land) )then
        if(land .eqv. .true.)then
            
            call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_REAL8, ToID, MPI_LAND, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(band) )then
        if(band .eqv. .true.)then
            
            call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_REAL8, ToID,MPI_BAND , MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(lor) )then
        if(lor .eqv. .true.)then
            
            call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_REAL8, ToID, MPI_LOR, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(bor) )then
        if(bor .eqv. .true.)then
            
            call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_REAL8, ToID,MPI_BOR , MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(lxor) )then
        if(lxor .eqv. .true.)then
            
            call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_REAL8, ToID, MPI_LXOR, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(bxor) )then
        if(bxor .eqv. .true.)then
            
            call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_REAL8, ToID, MPI_BXOR, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(maxloc) )then
        if(maxloc .eqv. .true.)then
            
            call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_REAL8, ToID, MPI_MAXLOC, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(minloc) )then
        if(minloc .eqv. .true.)then
            call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_REAL8, ToID, MPI_MINLOC, MPI_COMM_WORLD, obj%ierr)
        endif
    endif

end subroutine
!################################################################



!################################################################
subroutine AllReduceMPIInt(obj,sendobj,recvobj,count,start,&
    max,min,sum,prod,land,band,lor,bor,lxor,bxor,maxloc,minloc)
    class(MPI_),intent(inout)::obj
    integer(int32),intent(inout)::sendobj(:),recvobj(:)
    integer(int32),intent(in)::count
    integer(int32)  :: start_id
    integer(int32),optional,intent(in)::start
    logical,optional,intent(in)::max,min,sum,prod,land,band,lor
    logical,optional,intent(in)::bor,lxor,bxor,maxloc,minloc

    start_id=input(default=1,option=start)
    if(present(max) )then
        if(max .eqv. .true.)then

            call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_integer,  MPI_MAX, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(min) )then
        if(min .eqv. .true.)then
            
            call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_integer,  MPI_MIN, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(sum) )then
        if(sum .eqv. .true.)then
            
            call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_integer,  MPI_SUM, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(prod) )then
        if(prod .eqv. .true.)then
            
            call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_integer,  MPI_PROD, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(land) )then
        if(land .eqv. .true.)then
            
            call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_integer,  MPI_LAND, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(band) )then
        if(band .eqv. .true.)then
            
            call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_integer, MPI_BAND , MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(lor) )then
        if(lor .eqv. .true.)then
            
            call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_integer,  MPI_LOR, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(bor) )then
        if(bor .eqv. .true.)then
            
            call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_integer, MPI_BOR , MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(lxor) )then
        if(lxor .eqv. .true.)then
            
            call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_integer,  MPI_LXOR, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(bxor) )then
        if(bxor .eqv. .true.)then
            
            call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_integer,  MPI_BXOR, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(maxloc) )then
        if(maxloc .eqv. .true.)then
            
            call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_integer,  MPI_MAXLOC, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(minloc) )then
        if(minloc .eqv. .true.)then
            call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_integer,  MPI_MINLOC, MPI_COMM_WORLD, obj%ierr)
        endif
    endif

end subroutine
!################################################################


!################################################################
subroutine AllReduceMPIReal(obj,sendobj,recvobj,count,start,&
    max,min,sum,prod,land,band,lor,bor,lxor,bxor,maxloc,minloc)
    class(MPI_),intent(inout)::obj
    real(real64),intent(inout)::sendobj(:),recvobj(:)
    integer(int32),intent(in)::count
    integer(int32)  :: start_id
    integer(int32),optional,intent(in)::start
    logical,optional,intent(in)::max,min,sum,prod,land,band,lor
    logical,optional,intent(in)::bor,lxor,bxor,maxloc,minloc

    start_id=input(default=1,option=start)
    if(present(max) )then
        if(max .eqv. .true.)then

            call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_REAL8,  MPI_MAX, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(min) )then
        if(min .eqv. .true.)then
            
            call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_REAL8,  MPI_MIN, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(sum) )then
        if(sum .eqv. .true.)then
            
            call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_REAL8,  MPI_SUM, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(prod) )then
        if(prod .eqv. .true.)then
            
            call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_REAL8,  MPI_PROD, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(land) )then
        if(land .eqv. .true.)then
            
            call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_REAL8,  MPI_LAND, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(band) )then
        if(band .eqv. .true.)then
            
            call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_REAL8, MPI_BAND , MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(lor) )then
        if(lor .eqv. .true.)then
            
            call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_REAL8,  MPI_LOR, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(bor) )then
        if(bor .eqv. .true.)then
            
            call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_REAL8, MPI_BOR , MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(lxor) )then
        if(lxor .eqv. .true.)then
            
            call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_REAL8,  MPI_LXOR, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(bxor) )then
        if(bxor .eqv. .true.)then
            
            call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_REAL8,  MPI_BXOR, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(maxloc) )then
        if(maxloc .eqv. .true.)then
            
            call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_REAL8,  MPI_MAXLOC, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(minloc) )then
        if(minloc .eqv. .true.)then
            call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            , count, MPI_REAL8,  MPI_MINLOC, MPI_COMM_WORLD, obj%ierr)
        endif
    endif

end subroutine
!################################################################




!################################################################
subroutine EndMPI(obj)
    class(MPI_),intent(inout)::obj
    integer(int32) :: i

    call MPI_barrier(mpi_comm_world,obj%ierr)
    obj%etime = mpi_wtime()
    
    
    if(obj%MyRank==0)then
        print *, " ############################################ "
    endif
    do i=1,obj%Petot
        if(obj%MyRank+1==obj%Petot)then
            print *, " Computation time (sec.) ::  ", obj%etime - obj%stime
        endif
    enddo
    if(obj%MyRank==0)then
        print *, " Number of cores         ::  ",obj%Petot
        print *, " ############################################ "
    endif
    
    call mpi_finalize(obj%ierr)

end subroutine
!################################################################


!################################################################
subroutine getLapTimeMPI(obj,comment)
    class(MPI_),intent(inout)::obj
    character(*),optional,intent(in)::comment


    obj%LapTimeStep = obj%LapTimeStep+1 
    obj%laptime(obj%LapTimeStep)=MPI_Wtime()
    
    if(present(comment) )then
        obj%comments(obj%LapTimeStep)%comment=comment
    endif

end subroutine
!################################################################


!################################################################
subroutine showLapTimeMPI(obj,clength,rank)
    class(MPI_),intent(inout)::obj
    integer(int32),optional,intent(in)::rank,cLength
    integer(int32) :: i,n
    real(real64) :: rate

    if(present(clength) )then
        n=clength
    else
        n=15
    endif

    if(present(rank) )then
        if(obj%MyRank==rank)then
            print *, " ############################################ "
            do i=2, obj%LapTimeStep
                rate=(obj%LapTime(i)-obj%LapTime(i-1) )/(obj%LapTime(obj%LapTimeStep)-obj%LapTime(1) )
                print *, obj%comments(i)%comment(1:n)," : ",obj%LapTime(i)-obj%LapTime(i-1),"(sec.)",real(rate*100.0d0),"(%)"
            enddo
            print *, " ############################################ "
        endif
    else
        if(obj%MyRank==0)then
            print *, " ############################################ "
            do i=2, obj%LapTimeStep
                rate=(obj%LapTime(i)-obj%LapTime(i-1) )/(obj%LapTime(obj%LapTimeStep)-obj%LapTime(1) )
                print *, obj%comments(i)%comment(1:n) ," : ",obj%LapTime(i)-obj%LapTime(i-1),"(sec.)",real(rate*100.0d0),"(%)"
            enddo
            print *, " ############################################ "
        endif
    endif
    obj%etime = mpi_wtime()
    
    
    if(obj%MyRank==0)then
        print *, " ############################################ "
    endif
    do i=1,obj%Petot
        if(obj%MyRank+1==obj%Petot)then
            print *, " Computation time (sec.) ::  ", obj%etime - obj%stime
        endif
    enddo
    if(obj%MyRank==0)then
        print *, " Number of cores         ::  ",obj%Petot
        print *, " ############################################ "
    endif
    

end subroutine
!################################################################



!################################################################
subroutine CopyMPI(obj,OriginComm,NewCommLayerID)
    class(MPI_),intent(inout)::obj
    integer(int32),optional,intent(in)::OriginComm,NewCommLayerID
    

    call MPI_COMM_DUP(input(default=MPI_COMM_WORLD,option=OriginComm),& 
        obj%Comm(input(default=2,option=NewCommLayerID) ) , obj%ierr)

end subroutine
!################################################################


!################################################################
subroutine SplitMPI(obj,OriginComm,NewCommLayerID,key)
    class(MPI_),intent(inout)::obj
    integer(int32),optional,intent(in)::OriginComm,NewCommLayerID,key
    

    !call MPI_COMM_SPLIT(input(default=MPI_COMM_WORLD,option=OriginComm),& 
    !    obj%key(input(default=0,option=key)),&
    !    obj%Comm(input(default=2,option=NewCommLayerID) ) , obj%ierr)
    
    
end subroutine
!################################################################



!################################################################
subroutine FreeMPI(obj,CommLayerID)
    class(MPI_),intent(inout)::obj
    integer(int32),optional,intent(in) :: CommLayerID
    
    !call MPI_COMM_FREE(input(default=MPI_COMM_WORLD,option=obj%Comm(CommLayerID) ), obj%ierr)
    
    !call MPI_COMM_FREE(MPI_COMM_WORLD, obj%ierr)
    
end subroutine
!################################################################


!################################################################
subroutine syncGraphMPI(obj,graph)
    class(MPI_),intent(inout) ::obj
    type(Graph_) ,intent(inout) ::graph
    integer(int32),allocatable :: AdjacencyMatrix(:,:)
    integer(int32) :: i,j,size1,size2,n

    size1 = size(graph%AdjacencyMatrix,1)
    size2 = size(graph%AdjacencyMatrix,2)
    allocate(AdjacencyMatrix(size1,size2))
    AdjacencyMatrix(:,:) = 0

    ! sync only edge
    do i=1,obj%petot
        n=i-1
        if( n==obj%myrank )then
            AdjacencyMatrix = graph%AdjacencyMatrix
        endif
        call obj%Bcast(From=n,val= AdjacencyMatrix)
        call graph%sync(AdjacencyMatrix)
    enddo


end subroutine
!################################################################

end module