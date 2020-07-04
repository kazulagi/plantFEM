module MPIClass
    use, intrinsic :: iso_fortran_env
    use mpi
    use MathClass
    use ArrayClass
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
        integer(int32),allocatable::Comm(:),key(:)
        integer(int32),allocatable::Stack(:,:),localstack(:)
        integer(int32) :: LapTimeStep
        real(real64) :: stime
        real(real64) :: etime
        real(real64) :: laptime(1000)
        character(200) :: name
        type(comment_) :: comments(1000)
        
    contains
        procedure :: Start => StartMPI
        procedure :: Barrier => BarrierMPI
        procedure, Pass ::  readMPIInt
        procedure, Pass ::  readMPIReal
        generic ::  read =>   readMPIInt,readMPIReal
        
        procedure, Pass :: BcastMPIInt
        procedure, Pass :: BcastMPIReal
        generic  :: Bcast => BcastMPIInt, BcastMPIReal

        procedure, Pass :: GatherMPIInt 
        procedure, Pass :: GatherMPIReal 
        generic :: Gather => GatherMPIInt, GatherMPIReal 


        procedure, Pass :: ScatterMPIInt 
        procedure, Pass :: ScatterMPIReal 
        generic :: Scatter => ScatterMPIInt, ScatterMPIReal 
 

        procedure, Pass :: AllGatherMPIInt 
        procedure, Pass :: AllGatherMPIReal 
        generic :: AllGather => AllGatherMPIInt, AllGatherMPIReal 

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
subroutine BcastMPIReal(obj,From,val)
    class(MPI_),intent(inout)::obj
    integer(int32),intent(inout)::From 
    real(real64),intent(inout)::val
    integer(int32) :: i

    call MPI_Bcast(val, 1, MPI_REAL8, From, MPI_COMM_WORLD, obj%ierr)
end subroutine
!################################################################


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

end module