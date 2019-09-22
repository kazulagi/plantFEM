module MPIClass
    use mpi
    implicit none
!    include 'mpif.h'
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
        integer :: LapTimeStep
        real(8) :: stime
        real(8) :: etime
        real(8) :: laptime(1000)
        type(comment) :: comments(1000)

    contains
        procedure :: Start => StartMPI
        procedure :: Barrier => BarrierMPI
        procedure :: Bcast => BcastMPI
        procedure :: End => EndMPI
        procedure :: getLapTime => getLapTimeMPI
        procedure :: showLapTime => showLapTimeMPI
        procedure :: GetInfo => GetMPIInfo
    end type    
contains
!################################################################
subroutine StartMPI(obj)
    class(MPI_),intent(inout)::obj
    call mpi_init(obj%ierr)
    call mpi_comm_size(mpi_comm_world,obj%Petot ,obj%ierr)
    call mpi_comm_rank(mpi_comm_world,obj%MyRank,obj%ierr)
    obj%stime = mpi_wtime()
    obj%laptime(:) = 0.0d0
    obj%LapTimeStep = 1
    obj%laptime(obj%LapTimeStep)=MPI_Wtime()
    obj%comments%comment(:)="No comment"
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
    integer :: i

    call MPI_barrier(mpi_comm_world,obj%ierr)
end subroutine
!################################################################


!################################################################
subroutine BcastMPI(obj,From,int_val)
    class(MPI_),intent(inout)::obj
    integer,intent(inout)::From,int_val
    integer :: i

    call MPI_Bcast(int_val, 1, MPI_INTEGER, From, MPI_COMM_WORLD, obj%ierr)
end subroutine
!################################################################


!################################################################
subroutine EndMPI(obj)
    class(MPI_),intent(inout)::obj
    integer :: i

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

end module