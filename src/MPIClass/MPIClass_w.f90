module MPIClass
    
    use MathClass
    implicit none


    !interface BcastMPI
    !    module procedure BcastMPIReal, BcastMPIInt
    !end interface

    type :: comment
        character*200 :: comment
    endtype
    
    type:: MPI_

    
        integer :: ierr
        integer :: MyRank
        integer :: PeTot
        integer :: Comm1
        integer :: Comm2
        integer :: Comm3
        integer :: Comm4
        integer :: Comm5
        integer,allocatable::Comm(:),key(:)
        integer :: LapTimeStep
        real(8) :: stime
        real(8) :: etime
        real(8) :: laptime(1000)
        type(comment) :: comments(1000)

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

        procedure :: free  => freeMPI 
        procedure :: split => splitMPI 
        procedure :: copy  => copyMPI 
        procedure :: End => EndMPI
        procedure :: getLapTime => getLapTimeMPI
        procedure :: showLapTime => showLapTimeMPI
        procedure :: GetInfo => GetMPIInfo
    end type    
contains



!################################################################
subroutine StartMPI(obj,NumOfComm)
    class(MPI_),intent(inout)::obj
    integer,optional,intent(in)::NumOfComm

    !call mpi_init(obj%ierr)
    !call mpi_comm_size(mpi_comm_world,obj%Petot ,obj%ierr)
    !call mpi_comm_rank(mpi_comm_world,obj%MyRank,obj%ierr)

    allocate(obj%Comm(input(default=100,option=NumOfComm)  ) )
    allocate(obj%key(input(default=100,option=NumOfComm)  ) )
    !obj%Comm(:)=MPI_COMM_WORLD
    obj%key(:)=0.0d0
    !obj%stime = mpi_wtime()
    obj%laptime(:) = 0.0d0
    obj%LapTimeStep = 1
    !obj%laptime(obj%LapTimeStep)=MPI_Wtime()
    obj%comments%comment(:)="No comment"
end subroutine
!################################################################



!################################################################
subroutine readMPIInt(obj,val,ExecRank,Msg)
    class(MPI_),intent(inout)::obj
    integer,optional,intent(in)::ExecRank
    character(*),optional,intent(in)::Msg
    integer,intent(out)::val
    integer :: i,j,n


    n=input(default=0,option=ExecRank)
    if(obj%MyRank==n)then
        print *, input(default=" ",option=Msg)
        read(*,*) val
    endif
    !call obj%Barrier()

end subroutine 
!################################################################


!################################################################
subroutine readMPIReal(obj,val,ExecRank,Msg)
    class(MPI_),intent(inout)::obj
    integer,optional,intent(in)::ExecRank
    character(*),optional,intent(in)::Msg
    real(8),intent(out)::val
    character*200 :: Massage
    integer :: i,j,n


    n=input(default=0,option=ExecRank)
    if(obj%MyRank==n)then
        print *, input(default=Massage,option=Msg)
        read(*,*) val
    endif
    !call obj%Barrier()

end subroutine 
!################################################################



!################################################################
subroutine GetMPIInfo(obj)
    class(MPI_),intent(inout)::obj
    
    !call mpi_comm_size(mpi_comm_world,obj%Petot ,obj%ierr)
    !call mpi_comm_rank(mpi_comm_world,obj%MyRank,obj%ierr)
    
end subroutine
!################################################################


!################################################################
subroutine BarrierMPI(obj)
    class(MPI_),intent(inout)::obj
    integer :: i

    !call MPI_barrier(mpi_comm_world,obj%ierr)
end subroutine
!################################################################


! All to All 

!################################################################
subroutine BcastMPIInt(obj,From,val)
    class(MPI_),intent(inout)::obj
    integer,intent(inout)::From,val
    integer :: i

    !call MPI_Bcast(val, 1, MPI_INTEGER, From, MPI_COMM_WORLD, obj%ierr)
end subroutine
!################################################################

!################################################################
subroutine BcastMPIReal(obj,From,val)
    class(MPI_),intent(inout)::obj
    integer,intent(inout)::From 
    Real(8),intent(inout)::val
    integer :: i

    !call MPI_Bcast(val, 1, MPI_REAL8, From, MPI_COMM_WORLD, obj%ierr)
end subroutine
!################################################################


!################################################################
subroutine GatherMPIInt(obj,sendobj,sendcount,recvobj,recvcount,&
    send_start_id,recv_start_id,To)
    class(MPI_),intent(inout)::obj
    integer,intent(inout)::sendobj(:),recvobj(:)
    integer,intent(in)::sendcount,recvcount
    integer,optional,intent(in)::send_start_id,recv_start_id,To
    integer :: i,s_start_id,r_start_id,ToID

    s_start_id=input(default=1,option=send_start_id)
    r_start_id=input(default=1,option=recv_start_id)
    ToID=input(default=0,option=To)

    !call MPI_Gather(sendobj(s_start_id), sendcount, MPI_INTEGER, recvobj(r_start_id)&
    !, recvcount, MPI_INTEGER, ToID ,MPI_COMM_WORLD, obj%ierr)
end subroutine
!################################################################


!################################################################
subroutine GatherMPIReal(obj,sendobj,sendcount,recvobj,recvcount,&
    send_start_id,recv_start_id,To)
    class(MPI_),intent(inout)::obj
    Real(8),intent(inout)::sendobj(:),recvobj(:)
    integer,intent(in)::sendcount,recvcount
    integer,optional,intent(in)::send_start_id,recv_start_id,To
    integer :: i,s_start_id,r_start_id,ToID

    s_start_id=input(default=1,option=send_start_id)
    r_start_id=input(default=1,option=recv_start_id)
    ToID=input(default=0,option=To)

    !call MPI_Gather(sendobj(s_start_id), sendcount, MPI_REAL8, recvobj(r_start_id)&
    !, recvcount, MPI_REAL8, ToID, MPI_COMM_WORLD, obj%ierr)
end subroutine
!################################################################





!################################################################
subroutine ScatterMPIInt(obj,sendobj,sendcount,recvobj,recvcount,&
    send_start_id,recv_start_id,From)
    class(MPI_),intent(inout)::obj
    integer,intent(inout)::sendobj(:),recvobj(:)
    integer,intent(in)::sendcount,recvcount
    integer,optional,intent(in)::send_start_id,recv_start_id,From
    integer :: i,s_start_id,r_start_id,FromID

    s_start_id=input(default=1,option=send_start_id)
    r_start_id=input(default=1,option=recv_start_id)
    FromID=input(default=0,option=From)

    !call MPI_Scatter(sendobj(s_start_id), sendcount, MPI_INTEGER, recvobj(r_start_id)&
    !, recvcount, MPI_INTEGER, FromID, MPI_COMM_WORLD, obj%ierr)
end subroutine
!################################################################


!################################################################
subroutine ScatterMPIReal(obj,sendobj,sendcount,recvobj,recvcount,&
    send_start_id,recv_start_id,From)
    class(MPI_),intent(inout)::obj
    Real(8),intent(inout)::sendobj(:),recvobj(:)
    integer,intent(in)::sendcount,recvcount
    integer,optional,intent(in)::send_start_id,recv_start_id,From
    integer :: i,s_start_id,r_start_id,FromID

    s_start_id=input(default=1,option=send_start_id)
    r_start_id=input(default=1,option=recv_start_id)
    FromID=input(default=0,option=From)

    !call MPI_Scatter(sendobj(s_start_id), sendcount, MPI_REAL8, recvobj(r_start_id)&
    !, recvcount, MPI_REAL8, FromID, MPI_COMM_WORLD, obj%ierr)
end subroutine
!################################################################






!################################################################
subroutine AllGatherMPIInt(obj,sendobj,sendcount,recvobj,recvcount,&
    send_start_id,recv_start_id)
    class(MPI_),intent(inout)::obj
    integer,intent(inout)::sendobj(:),recvobj(:)
    integer,intent(in)::sendcount,recvcount
    integer,optional,intent(in)::send_start_id,recv_start_id
    integer :: i,s_start_id,r_start_id

    s_start_id=input(default=1,option=send_start_id)
    r_start_id=input(default=1,option=recv_start_id)

    !call MPI_AllGather(sendobj(s_start_id), sendcount, MPI_INTEGER, recvobj(r_start_id)&
    !, recvcount, MPI_INTEGER, MPI_COMM_WORLD, obj%ierr)
end subroutine
!################################################################


!################################################################
subroutine AllGatherMPIReal(obj,sendobj,sendcount,recvobj,recvcount,&
    send_start_id,recv_start_id)
    class(MPI_),intent(inout)::obj
    Real(8),intent(inout)::sendobj(:),recvobj(:)
    integer,intent(in)::sendcount,recvcount
    integer,optional,intent(in)::send_start_id,recv_start_id
    integer :: i,s_start_id,r_start_id

    s_start_id=input(default=1,option=send_start_id)
    r_start_id=input(default=1,option=recv_start_id)

    !call MPI_AllGather(sendobj(s_start_id), sendcount, MPI_REAL8, recvobj(r_start_id)&
    !, recvcount, MPI_REAL8, MPI_COMM_WORLD, obj%ierr)
end subroutine
!################################################################







!################################################################
subroutine AlltoAllMPIInt(obj,sendobj,sendcount,recvobj,recvcount,&
    send_start_id,recv_start_id)
    class(MPI_),intent(inout)::obj
    integer,intent(inout)::sendobj(:),recvobj(:)
    integer,intent(in)::sendcount,recvcount
    integer,optional,intent(in)::send_start_id,recv_start_id
    integer :: i,s_start_id,r_start_id

    s_start_id=input(default=1,option=send_start_id)
    r_start_id=input(default=1,option=recv_start_id)

    !call MPI_AlltoAll(sendobj(s_start_id), sendcount, MPI_INTEGER, recvobj(r_start_id)&
    !, recvcount, MPI_INTEGER, MPI_COMM_WORLD, obj%ierr)
end subroutine
!################################################################


!################################################################
subroutine AlltoAllMPIReal(obj,sendobj,sendcount,recvobj,recvcount,&
    send_start_id,recv_start_id)
    class(MPI_),intent(inout)::obj
    Real(8),intent(inout)::sendobj(:),recvobj(:)
    integer,intent(in)::sendcount,recvcount
    integer,optional,intent(in)::send_start_id,recv_start_id
    integer :: i,s_start_id,r_start_id

    s_start_id=input(default=1,option=send_start_id)
    r_start_id=input(default=1,option=recv_start_id)

    !call MPI_AlltoAll(sendobj(s_start_id), sendcount, MPI_REAL8, recvobj(r_start_id)&
    !, recvcount, MPI_REAL8, MPI_COMM_WORLD, obj%ierr)
end subroutine
!################################################################




!################################################################
subroutine ReduceMPIInt(obj,sendobj,recvobj,count,start,To,&
    max,min,sum,prod,land,band,lor,bor,lxor,bxor,maxloc,minloc)
    class(MPI_),intent(inout)::obj
    integer,intent(inout)::sendobj(:),recvobj(:)
    integer,intent(in)::count
    integer  :: ToID,start_id
    integer,optional,intent(in)::start,To
    logical,optional,intent(in)::max,min,sum,prod,land,band,lor
    logical,optional,intent(in)::bor,lxor,bxor,maxloc,minloc

    ToID=input(default=0,option=To)
    start_id=input(default=1,option=start)
    if(present(max) )then
        if(max .eqv. .true.)then

            !call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            !!, count, MPI_INTEGER, ToID, MPI_MAX, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(min) )then
        if(min .eqv. .true.)then
            
            !call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            !!, count, MPI_INTEGER, ToID, MPI_MIN, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(sum) )then
        if(sum .eqv. .true.)then
            
            !call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            !!, count, MPI_INTEGER, ToID, MPI_SUM, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(prod) )then
        if(prod .eqv. .true.)then
            
            !call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            !!, count, MPI_INTEGER, ToID, MPI_PROD, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(land) )then
        if(land .eqv. .true.)then
            
            !call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            !!, count, MPI_INTEGER, ToID, MPI_LAND, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(band) )then
        if(band .eqv. .true.)then
            
            !call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            !!, count, MPI_INTEGER, ToID,MPI_BAND , MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(lor) )then
        if(lor .eqv. .true.)then
            
            !call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            !!, count, MPI_INTEGER, ToID, MPI_LOR, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(bor) )then
        if(bor .eqv. .true.)then
            
            !call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            !!, count, MPI_INTEGER, ToID,MPI_BOR , MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(lxor) )then
        if(lxor .eqv. .true.)then
            
            !call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            !!, count, MPI_INTEGER, ToID, MPI_LXOR, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(bxor) )then
        if(bxor .eqv. .true.)then
            
            !call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            !!, count, MPI_INTEGER, ToID, MPI_BXOR, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(maxloc) )then
        if(maxloc .eqv. .true.)then
            
            !call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            !!, count, MPI_INTEGER, ToID, MPI_MAXLOC, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(minloc) )then
        if(minloc .eqv. .true.)then
            !call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            !!, count, MPI_INTEGER, ToID, MPI_MINLOC, MPI_COMM_WORLD, obj%ierr)
        endif
    endif

end subroutine
!################################################################


!################################################################
subroutine ReduceMPIReal(obj,sendobj,recvobj,count,start,To,&
    max,min,sum,prod,land,band,lor,bor,lxor,bxor,maxloc,minloc)
    class(MPI_),intent(inout)::obj
    real(8),intent(inout)::sendobj(:),recvobj(:)
    integer,intent(in)::count
    integer  :: ToID,start_id
    integer,optional,intent(in)::start,To
    logical,optional,intent(in)::max,min,sum,prod,land,band,lor
    logical,optional,intent(in)::bor,lxor,bxor,maxloc,minloc

    ToID=input(default=0,option=To)
    start_id=input(default=1,option=start)
    if(present(max) )then
        if(max .eqv. .true.)then

            !call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            !!, count, MPI_REAL8, ToID, MPI_MAX, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(min) )then
        if(min .eqv. .true.)then
            
            !call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            !!, count, MPI_REAL8, ToID, MPI_MIN, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(sum) )then
        if(sum .eqv. .true.)then
            
            !call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            !!, count, MPI_REAL8, ToID, MPI_SUM, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(prod) )then
        if(prod .eqv. .true.)then
            
            !call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            !!, count, MPI_REAL8, ToID, MPI_PROD, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(land) )then
        if(land .eqv. .true.)then
            
            !call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            !!, count, MPI_REAL8, ToID, MPI_LAND, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(band) )then
        if(band .eqv. .true.)then
            
            !call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            !!, count, MPI_REAL8, ToID,MPI_BAND , MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(lor) )then
        if(lor .eqv. .true.)then
            
            !call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            !!, count, MPI_REAL8, ToID, MPI_LOR, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(bor) )then
        if(bor .eqv. .true.)then
            
            !call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            !!, count, MPI_REAL8, ToID,MPI_BOR , MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(lxor) )then
        if(lxor .eqv. .true.)then
            
            !call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            !!, count, MPI_REAL8, ToID, MPI_LXOR, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(bxor) )then
        if(bxor .eqv. .true.)then
            
            !call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            !!, count, MPI_REAL8, ToID, MPI_BXOR, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(maxloc) )then
        if(maxloc .eqv. .true.)then
            
            !call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            !!, count, MPI_REAL8, ToID, MPI_MAXLOC, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(minloc) )then
        if(minloc .eqv. .true.)then
            !call MPI_Reduce(sendobj(start_id), recvobj(start_id)&
            !!, count, MPI_REAL8, ToID, MPI_MINLOC, MPI_COMM_WORLD, obj%ierr)
        endif
    endif

end subroutine
!################################################################



!################################################################
subroutine AllReduceMPIInt(obj,sendobj,recvobj,count,start,&
    max,min,sum,prod,land,band,lor,bor,lxor,bxor,maxloc,minloc)
    class(MPI_),intent(inout)::obj
    integer,intent(inout)::sendobj(:),recvobj(:)
    integer,intent(in)::count
    integer  :: start_id
    integer,optional,intent(in)::start
    logical,optional,intent(in)::max,min,sum,prod,land,band,lor
    logical,optional,intent(in)::bor,lxor,bxor,maxloc,minloc

    start_id=input(default=1,option=start)
    if(present(max) )then
        if(max .eqv. .true.)then

            !call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            !!, count, MPI_INTEGER,  MPI_MAX, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(min) )then
        if(min .eqv. .true.)then
            
            !call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            !, count, MPI_INTEGER,  MPI_MIN, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(sum) )then
        if(sum .eqv. .true.)then
            
            !call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            !, count, MPI_INTEGER,  MPI_SUM, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(prod) )then
        if(prod .eqv. .true.)then
            
            !call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            !, count, MPI_INTEGER,  MPI_PROD, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(land) )then
        if(land .eqv. .true.)then
            
            !call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            !, count, MPI_INTEGER,  MPI_LAND, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(band) )then
        if(band .eqv. .true.)then
            
            !call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            !, count, MPI_INTEGER, MPI_BAND , MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(lor) )then
        if(lor .eqv. .true.)then
            
            !call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            !, count, MPI_INTEGER,  MPI_LOR, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(bor) )then
        if(bor .eqv. .true.)then
            
            !call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            !, count, MPI_INTEGER, MPI_BOR , MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(lxor) )then
        if(lxor .eqv. .true.)then
            
            !call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            !, count, MPI_INTEGER,  MPI_LXOR, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(bxor) )then
        if(bxor .eqv. .true.)then
            
            !call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            !, count, MPI_INTEGER,  MPI_BXOR, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(maxloc) )then
        if(maxloc .eqv. .true.)then
            
            !call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            !, count, MPI_INTEGER,  MPI_MAXLOC, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(minloc) )then
        if(minloc .eqv. .true.)then
            !call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            !, count, MPI_INTEGER,  MPI_MINLOC, MPI_COMM_WORLD, obj%ierr)
        endif
    endif

end subroutine
!################################################################


!################################################################
subroutine AllReduceMPIReal(obj,sendobj,recvobj,count,start,&
    max,min,sum,prod,land,band,lor,bor,lxor,bxor,maxloc,minloc)
    class(MPI_),intent(inout)::obj
    real(8),intent(inout)::sendobj(:),recvobj(:)
    integer,intent(in)::count
    integer  :: start_id
    integer,optional,intent(in)::start
    logical,optional,intent(in)::max,min,sum,prod,land,band,lor
    logical,optional,intent(in)::bor,lxor,bxor,maxloc,minloc

    start_id=input(default=1,option=start)
    if(present(max) )then
        if(max .eqv. .true.)then

            !call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            !, count, MPI_REAL8,  MPI_MAX, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(min) )then
        if(min .eqv. .true.)then
            
            !call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            !, count, MPI_REAL8,  MPI_MIN, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(sum) )then
        if(sum .eqv. .true.)then
            
            !call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            !, count, MPI_REAL8,  MPI_SUM, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(prod) )then
        if(prod .eqv. .true.)then
            
            !call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            !, count, MPI_REAL8,  MPI_PROD, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(land) )then
        if(land .eqv. .true.)then
            
            !call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            !, count, MPI_REAL8,  MPI_LAND, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(band) )then
        if(band .eqv. .true.)then
            
            !call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            !, count, MPI_REAL8, MPI_BAND , MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(lor) )then
        if(lor .eqv. .true.)then
            
            !call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            !, count, MPI_REAL8,  MPI_LOR, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(bor) )then
        if(bor .eqv. .true.)then
            
            !call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            !, count, MPI_REAL8, MPI_BOR , MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(lxor) )then
        if(lxor .eqv. .true.)then
            
            !call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            !, count, MPI_REAL8,  MPI_LXOR, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(bxor) )then
        if(bxor .eqv. .true.)then
            
            !call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            !, count, MPI_REAL8,  MPI_BXOR, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(maxloc) )then
        if(maxloc .eqv. .true.)then
            
            !call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            !, count, MPI_REAL8,  MPI_MAXLOC, MPI_COMM_WORLD, obj%ierr)
        endif
    endif
    if(present(minloc) )then
        if(minloc .eqv. .true.)then
            !call MPI_AllReduce(sendobj(start_id), recvobj(start_id)&
            !, count, MPI_REAL8,  MPI_MINLOC, MPI_COMM_WORLD, obj%ierr)
        endif
    endif

end subroutine
!################################################################




!################################################################
subroutine EndMPI(obj)
    class(MPI_),intent(inout)::obj
    integer :: i

    !call MPI_barrier(mpi_comm_world,obj%ierr)
    !obj%etime = mpi_wtime()
    
    
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
    
    !call mpi_finalize(obj%ierr)

end subroutine
!################################################################


!################################################################
subroutine getLapTimeMPI(obj,comment)
    class(MPI_),intent(inout)::obj
    character(*),optional,intent(in)::comment


    obj%LapTimeStep = obj%LapTimeStep+1 
    !obj%laptime(obj%LapTimeStep)=MPI_Wtime()
    
    if(present(comment) )then
        obj%comments(obj%LapTimeStep)%comment=comment
    endif

end subroutine
!################################################################


!################################################################
subroutine showLapTimeMPI(obj,clength,rank)
    class(MPI_),intent(inout)::obj
    integer,optional,intent(in)::rank,cLength
    integer :: i,n
    real(8) :: rate

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
    !obj%etime = mpi_wtime()
    
    
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
    integer,optional,intent(in)::OriginComm,NewCommLayerID
    

    !call MPI_COMM_DUP(input(default=MPI_COMM_WORLD,option=OriginComm),& 
    !    obj%Comm(input(default=2,option=NewCommLayerID) ) , obj%ierr)

end subroutine
!################################################################


!################################################################
subroutine SplitMPI(obj,OriginComm,NewCommLayerID,key)
    class(MPI_),intent(inout)::obj
    integer,optional,intent(in)::OriginComm,NewCommLayerID,key
    

    !!call MPI_COMM_SPLIT(input(default=MPI_COMM_WORLD,option=OriginComm),& 
    !    obj%key(input(default=0,option=key)),&
    !    obj%Comm(input(default=2,option=NewCommLayerID) ) , obj%ierr)
    
    
end subroutine
!################################################################



!################################################################
subroutine FreeMPI(obj,CommLayerID)
    class(MPI_),intent(inout)::obj
    integer,optional,intent(in) :: CommLayerID
    
    !!call MPI_COMM_FREE(input(default=MPI_COMM_WORLD,option=obj%Comm(CommLayerID) ), obj%ierr)
    
    !!call MPI_COMM_FREE(MPI_COMM_WORLD, obj%ierr)
    
end subroutine
!################################################################

end module