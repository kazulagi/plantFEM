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
   end type

   type :: MPI_JOB_
      real(real64), pointer :: var => null()
      real(real64), allocatable :: var_list(:)
   end type

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
      integer(int32) :: mpi_restart_idf

      ! >>> job scheduler
      type(MPI_JOB_), allocatable :: MPI_JOB(:)
      integer(int32) :: MPI_MAX_JOB_NUMBER = 10
      integer(int32) :: MPI_LAST_JOB_NUMBER = 0
      real(real64), allocatable  :: EP_MY_VARIABLE_LIST(:, :)
      logical, allocatable  :: EP_MY_TASK_SCHEDULE(:)
      integer(int32)            :: EP_MY_CURRENT_TASK_ID = 0
      real(real64), allocatable  :: EP_ALL_VARIABLE_LIST(:, :)
      real(real64), allocatable  :: EP_MY_RESULT_LIST(:, :)
      type(IO_) :: EP_result_summary

      ! <<< job scheduler

      type(IO_) :: file
      character(:), allocatable :: filename
      integer(int32), allocatable::start_end_id(:)
      integer(int32), allocatable::Comm(:), key(:)
      integer(int32), allocatable::local_ID(:), Global_ID(:)
      integer(int32), allocatable::Stack(:, :), localstack(:)
      integer(int32) :: LapTimeStep
      real(real64) :: stime
      real(real64) :: etime
      real(real64) :: laptime(1000)
      character(200) :: name
      type(comment_) :: comments(1000)
      type(Graph_) :: graph

   contains
      procedure :: Start => StartMPI
      procedure :: init => StartMPI

      ! >>>> Embarrassingly parallel (自明並列)

      procedure, pass :: EP_set_variable_by_rangeMPI! Embarrassingly parallel (自明並列)
      procedure, pass :: EP_set_variable_by_listMPI! Embarrassingly parallel (自明並列)
      generic :: EP_set_variable => EP_set_variable_by_rangeMPI, EP_set_variable_by_listMPI

      procedure :: EP_set_result => EP_set_resultMPI   ! Embarrassingly parallel (自明並列)

      procedure :: EP_write_result => EP_write_resultMPI   ! Embarrassingly parallel (自明並列)
      procedure :: EP_get_variable => EP_get_variableMPI
      procedure :: EP_num_variavle => EP_num_variavleMPI
      procedure :: EP_min_var => EP_min_varMPI   ! minimal value
      procedure :: EP_max_var => EP_max_varMPI   ! maximum value

      ! <<<< Embarrassingly parallel (自明並列)

      procedure :: initItr => initItrMPI
      procedure :: Barrier => BarrierMPI
      procedure, Pass ::  readMPIInt
      procedure, Pass ::  readMPIReal
      generic ::  read => readMPIInt, readMPIReal

      procedure, Pass :: BcastMPIInt
      procedure, Pass :: BcastMPIIntVec
      procedure ::  BcastMPIIntVecFixedSize
      procedure, Pass :: BcastMPIIntArray
      procedure, Pass :: BcastMPIIntArray3
      procedure, Pass :: BcastMPIReal
      procedure, Pass :: BcastMPIRealVec
      procedure ::  BcastMPIRealVecFixedSize
      procedure, Pass :: BcastMPIRealArray
      procedure, Pass :: BcastMPIRealArray3
      procedure, Pass :: BcastMPIChar
      procedure, Pass :: BcastMPICharN
      procedure, Pass :: BcastMPILogical
      generic  :: Bcast => BcastMPIInt, BcastMPIReal, BcastMPIChar, BcastMPIIntVec, &
         BcastMPIIntArray, BcastMPIRealVec, BcastMPIRealArray, BcastMPICharN, BcastMPILogical, &
         BcastMPIRealArray3, BcastMPIIntArray3

      procedure, Pass :: GatherMPIInt
      procedure, Pass :: GatherMPIReal
      generic :: Gather => GatherMPIInt, GatherMPIReal

      procedure, Pass :: ScatterMPIInt
      procedure, Pass :: ScatterMPIReal
      generic :: Scatter => ScatterMPIInt, ScatterMPIReal

      procedure, Pass :: AllGatherMPIInt
      procedure, Pass :: AllGatherMPIReal
      procedure, Pass :: AllGatherMPIGraph
      generic :: AllGather => AllGatherMPIInt, AllGatherMPIReal, AllGatherMPIGraph
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

      procedure, pass :: isend_irecvRealVectorMPI
      generic :: isend_irecv => isend_irecvRealVectorMPI

      procedure, pass :: isend_Int32MPI
      procedure, pass :: isend_Int32VectorMPI
      procedure, pass :: isend_Real64MPI
      procedure, pass :: isend_Real64VectorMPI

      generic :: isend => isend_Int32MPI, isend_Real64MPI, &
         isend_Int32VectorMPI, isend_Real64VectorMPI

      procedure, pass :: irecv_Int32MPI
      procedure, pass :: irecv_Int32VectorMPI
      procedure, pass :: irecv_Real64MPI
      procedure, pass :: irecv_Real64VectorMPI

      generic :: irecv => irecv_Int32MPI, irecv_Real64MPI, &
         irecv_Int32VectorMPI, irecv_Real64VectorMPI

      procedure, pass :: WaitAll_Int32MPI
      procedure, pass :: WaitAll_Int32VectorMPI
      generic :: WaitAll => WaitAll_Int32MPI, WaitAll_Int32VectorMPI

      procedure :: createStack => createStackMPI
      procedure :: showStack => showStackMPI
      procedure :: free => freeMPI
      procedure :: split => splitMPI
      procedure :: copy => copyMPI
      procedure :: End => EndMPI
      procedure :: finalize => EndMPI
      procedure :: getLapTime => getLapTimeMPI
      procedure :: showLapTime => showLapTimeMPI
      procedure :: GetInfo => GetMPIInfo
      procedure :: createFileName => createFileNameMPI

      procedure :: num_images => num_imagesMPI
      procedure :: this_image => this_imageMPI

      procedure, pass :: restart_point_intmat64_MPI
      procedure, pass :: restart_point_realmat64_MPI
      procedure, pass :: restart_point_intvec32_MPI
      procedure, pass :: restart_point_realvec64_MPI

      generic :: restart_point => restart_point_intmat64_MPI &
                                  , restart_point_realmat64_MPI &
                                  , restart_point_intvec32_MPI &
                                  , restart_point_realvec64_MPI

      procedure, Pass :: syncGraphMPI
      generic :: sync => syncGraphMPI

      procedure :: open => fopen_MPI
      procedure :: fopen => fopen_MPI
      procedure :: close => fclose_MPI
      procedure :: fclose => fclose_MPI

   end type
contains

!################################################################
   subroutine StartMPI(obj, NumOfComm)
      class(MPI_), intent(inout)::obj
      integer(int32), optional, intent(in)::NumOfComm

      call mpi_init(obj%ierr)
      call mpi_comm_size(mpi_comm_world, obj%Petot, obj%ierr)
      call mpi_comm_rank(mpi_comm_world, obj%MyRank, obj%ierr)

      obj%mpi_restart_idf = 0

      allocate (obj%Comm(input(default=100, option=NumOfComm)))
      allocate (obj%key(input(default=100, option=NumOfComm)))
      obj%Comm(:) = MPI_COMM_WORLD
      obj%key(:) = 0.0d0
      obj%stime = mpi_wtime()
      obj%laptime(:) = 0.0d0
      obj%LapTimeStep = 1
      obj%laptime(obj%LapTimeStep) = MPI_Wtime()
      obj%comments%comment(:) = "No comment"

      print *, "Number of Core is ", obj%Petot

      if (allocated(obj%MPI_JOB)) deallocate (obj%MPI_JOB)
      allocate (obj%MPI_JOB(obj%MPI_MAX_JOB_NUMBER))

   end subroutine
!################################################################

!################################################################
   subroutine initItrMPI(obj, total_iteration)
      class(MPI_), intent(inout) :: obj
      integer(int32), intent(in) :: total_iteration
      integer(int32) :: petot, modval, divval, start_id, end_id, i

      ! from  1 to total_iteration
      modval = mod(total_iteration, obj%petot)
      divval = total_iteration/obj%petot
      if (obj%myrank + 1 <= modval) then
         obj%start_id = obj%myrank*(divval + 1) + 1
         obj%end_id = obj%myrank*(divval + 1) + (divval + 1)
      else
         obj%start_id = modval*(divval + 1) + (obj%myrank + 1 - modval - 1)*(divval) + 1
         obj%end_id = modval*(divval + 1) + (obj%myrank + 1 - modval)*(divval)
      end if

      if (allocated(obj%start_end_id)) then
         deallocate (obj%start_end_id)
      end if

      allocate (obj%start_end_id(total_iteration))
      do i = 1, obj%petot
         if (i <= modval) then
            start_id = (i - 1)*(divval + 1) + 1
            end_id = (i - 1)*(divval + 1) + (divval + 1)
            obj%start_end_id(start_id:end_id) = i
         else
            start_id = modval*(divval + 1) + (i - modval - 1)*(divval) + 1
            end_id = modval*(divval + 1) + (i - modval)*(divval)
            obj%start_end_id(start_id:end_id) = i
         end if
      end do
   end subroutine
!################################################################

!################################################################
   subroutine createFileNameMPI(obj, Path, Name)
      class(MPI_), intent(inout) :: obj
      character(*), intent(in) :: Path, Name
      integer :: i

      !i=access(Path//adjustl(fstring(obj%MyRank))," ")
      !if(i/=0)then
      call execute_command_line("mkdir -p "//Path//adjustl(fstring(obj%MyRank)))
      !endif
      obj%name = Path//adjustl(fstring(obj%MyRank))//"/" &
                 //Name//adjustl(fstring(obj%MyRank))

   end subroutine
!################################################################

!################################################################
   subroutine createStackMPI(obj, total)
      class(MPI_), intent(inout) :: obj
      integer(int32), intent(in) :: total
      integer(int32) :: i, j, LocalStacksize, itr, locstacksize

      if (allocated(obj%Stack)) then
         deallocate (obj%Stack)
      end if
      LocalStacksize = int(dble(total)/dble(obj%Petot)) + 1

      allocate (obj%Stack(obj%petot, LocalStacksize))

      itr = 1
      locstacksize = 0
      obj%Stack(:, :) = 0
      do j = 1, size(obj%Stack, 2)
         do i = 1, size(obj%Stack, 1)
            obj%Stack(i, j) = itr
            itr = itr + 1
            if (itr == total + 1) then
               exit
            end if
         end do
         if (itr == total + 1) then
            exit
         end if
      end do

      j = countif(Array=obj%Stack(obj%MyRank + 1, :), Equal=.true., Value=0)

      if (allocated(obj%localstack)) then
         deallocate (obj%localstack)
      end if
      allocate (obj%localstack(LocalStacksize - j))
      do i = 1, size(obj%localstack)
         obj%localstack(i) = obj%stack(obj%MyRank + 1, i)
      end do

   end subroutine
!################################################################

!function whoseLocalStack(obj,id) result(process_id)
!    class(MPI_),intent(inout) :: obj
!    integer(int32),intent(in) :: id
!    integer(int32) :: process_id
!
!    if(.not.allocated(obj%localstack) )then
!        process_id  = -1
!        return
!    endif
!
!    do i=1,size(obj%stack)
!
!    enddo
!
!end function
!!################################################################

!################################################################
   subroutine showStackMPI(obj)
      class(MPI_), intent(inout) :: obj
      integer(int32) :: i, j, n

      if (.not. allocated(obj%Stack)) then
         print *, "No stack is set"
         return
      else
         call obj%Barrier()
         do i = 1, obj%Petot
            if (obj%MyRank + 1 == i) then
               print *, "MyRank", obj%MyRank, "Stack :: ", obj%localstack(:)
            end if
         end do
      end if

   end subroutine
!################################################################

!################################################################
   subroutine readMPIInt(obj, val, ExecRank, Msg)
      class(MPI_), intent(inout)::obj
      integer(int32), optional, intent(in)::ExecRank
      character(*), optional, intent(in)::Msg
      integer(int32), intent(out)::val
      integer(int32) :: i, j, n

      n = input(default=0, option=ExecRank)
      if (obj%MyRank == n) then
         print *, input(default=" ", option=Msg)
         read (*, *) val
      end if
      call obj%Barrier()

   end subroutine
!################################################################

!################################################################
   subroutine readMPIReal(obj, val, ExecRank, Msg)
      class(MPI_), intent(inout)::obj
      integer(int32), optional, intent(in)::ExecRank
      character(*), optional, intent(in)::Msg
      real(real64), intent(out)::val
      character*200 :: Massage
      integer(int32) :: i, j, n

      n = input(default=0, option=ExecRank)
      if (obj%MyRank == n) then
         print *, input(default=Massage, option=Msg)
         read (*, *) val
      end if
      call obj%Barrier()

   end subroutine
!################################################################

!################################################################
   subroutine GetMPIInfo(obj)
      class(MPI_), intent(inout)::obj

      call mpi_comm_size(mpi_comm_world, obj%Petot, obj%ierr)
      call mpi_comm_rank(mpi_comm_world, obj%MyRank, obj%ierr)

   end subroutine
!################################################################

!################################################################
   subroutine BarrierMPI(obj)
      class(MPI_), intent(inout)::obj
      integer(int32) :: i

      call MPI_barrier(mpi_comm_world, obj%ierr)
   end subroutine
!################################################################

! All to All

!################################################################
   recursive subroutine BcastMPIInt(obj, From, val)
      class(MPI_), intent(inout)::obj
      integer(int32), intent(in)::From
      integer(int32), intent(inout)::val
      integer(int32) :: i

      call MPI_Bcast(val, 1, MPI_integer, From, MPI_COMM_WORLD, obj%ierr)
   end subroutine
!################################################################

!################################################################
   recursive subroutine BcastMPILogical(obj, From, val)
      class(MPI_), intent(inout)::obj
      integer(int32), intent(in)::From
      logical, intent(inout)::val

      call MPI_Bcast(val, 1, MPI_LOGICAL, From, MPI_COMM_WORLD, obj%ierr)

   end subroutine
!################################################################

!################################################################
   recursive subroutine BcastMPIIntVec(obj, From, val)
      class(MPI_), intent(inout)::obj
      integer(int32), intent(in) :: From
      integer(int32), allocatable, intent(inout)::val(:)
      integer(int32) :: i, j, n, vec_size
      integer(int32) :: sendval

      if (allocated(val)) then
         if (From /= obj%myrank) then
            deallocate (val)
         end if
      end if

      vec_size = 0
      if (From == obj%myrank) then
         if (allocated(val)) then
            vec_size = size(val)
         else
            vec_size = -1
         end if
      end if
      call obj%Bcast(From=From, val=vec_size)

      ! if array is empty, return
      if (vec_size < 1) then
         return
      end if

      if (From /= obj%myrank) then
         allocate (val(vec_size))
      end if

      sendval = 0
      do i = 1, vec_size
         call MPI_Bcast(val(i), 1, MPI_integer, From, MPI_COMM_WORLD, obj%ierr)
      end do

   end subroutine
!################################################################

!################################################################
   subroutine BcastMPIIntVecFixedSize(obj, From, val)
      class(MPI_), intent(inout)::obj
      integer(int32), intent(in) :: From
      integer(int32), intent(in)::val(:)
      integer(int32) :: i, j, n, vec_size
      integer(int32) :: sendval

      do i = 1, vec_size
         call MPI_Bcast(val(i), 1, MPI_integer, From, MPI_COMM_WORLD, obj%ierr)
      end do

   end subroutine
!################################################################

!################################################################
   subroutine BcastMPIRealVecFixedSize(obj, From, val)
      class(MPI_), intent(inout)::obj
      integer(int32), intent(in) :: From
      real(real64), intent(in)::val(:)
      integer(int32) :: i, j, n, vec_size
      integer(int32) :: sendval

      do i = 1, vec_size
         call MPI_Bcast(val(i), 1, MPI_REAL8, From, MPI_COMM_WORLD, obj%ierr)
      end do

   end subroutine
!################################################################

!################################################################
   recursive subroutine BcastMPIIntArray(obj, From, val)
      class(MPI_), intent(inout)::obj
      integer(int32), intent(in) :: From
      integer(int32), allocatable, intent(inout)::val(:, :)
      integer(int32) :: i, j, n, vec_size1, vec_size2
      integer(int32) :: sendval

      if (allocated(val) .and. From /= obj%myrank) then
         deallocate (val)
      end if

      vec_size1 = 0
      vec_size2 = 0
      if (From == obj%myrank) then
         if (.not. allocated(val)) then
            vec_size1 = -1
         else
            vec_size1 = size(val, 1)
         end if
      end if
      call obj%Bcast(From=From, val=vec_size1)
      if (From == obj%myrank) then
         vec_size2 = size(val, 2)
      end if
      call obj%Bcast(From=From, val=vec_size2)

      ! if array is empty, return
      if (vec_size1 < 1) then
         return
      end if

      if (From /= obj%myrank) then
         allocate (val(vec_size1, vec_size2))
      end if

      sendval = 0
      do i = 1, vec_size1
         do j = 1, vec_size2
            call MPI_Bcast(val(i, j), 1, MPI_integer, From, MPI_COMM_WORLD, obj%ierr)
         end do
      end do

   end subroutine
!################################################################

!################################################################
   recursive subroutine BcastMPIReal(obj, From, val)
      class(MPI_), intent(inout)::obj
      integer(int32), intent(in)::From
      real(real64), intent(inout)::val
      integer(int32) :: i

      call MPI_Bcast(val, 1, MPI_REAL8, From, MPI_COMM_WORLD, obj%ierr)
   end subroutine
!################################################################

!################################################################
   recursive subroutine BcastMPIRealVec(obj, From, val)
      class(MPI_), intent(inout)::obj
      integer(int32), intent(in) :: From
      real(real64), allocatable, intent(inout)::val(:)
      integer(int32) :: i, j, n, vec_size

      if (allocated(val)) then
         if (From /= obj%myrank) then
            deallocate (val)
         end if
      end if

      vec_size = 0
      if (From == obj%myrank) then
         if (.not. allocated(val)) then
            vec_size = -1
         else
            vec_size = size(val)
         end if
      end if
      call obj%Bcast(From=From, val=vec_size)
! if array is empty, return
      if (vec_size < 1) then
         return
      end if

      if (From /= obj%myrank) then
         allocate (val(vec_size))
      end if

      do i = 1, vec_size
         call MPI_Bcast(val(i), 1, MPI_REAL8, From, MPI_COMM_WORLD, obj%ierr)
      end do

   end subroutine
!################################################################

!################################################################
   recursive subroutine BcastMPIRealArray(obj, From, val)
      class(MPI_), intent(inout)::obj
      integer(int32), intent(in) :: From
      real(real64), allocatable, intent(inout)::val(:, :)
      integer(int32) :: i, j, n, vec_size1, vec_size2

      if (allocated(val)) then
         if (From /= obj%myrank) then
            deallocate (val)
         end if
      end if

      vec_size1 = 0
      vec_size2 = 0
      if (From == obj%myrank) then
         if (.not. allocated(val)) then
            vec_size1 = -1
         else
            vec_size1 = size(val, 1)
         end if
      end if
      call obj%Bcast(From=From, val=vec_size1)
      ! if array is empty, return
      if (vec_size1 < 1) then
         return
      end if

      if (From == obj%myrank) then
         vec_size2 = size(val, 2)
      end if
      call obj%Bcast(From=From, val=vec_size2)

      ! if array is empty, return
      if (vec_size2 < 1) then
         return
      end if

      if (From /= obj%myrank) then
         allocate (val(vec_size1, vec_size2))
      end if

      do i = 1, vec_size1
         do j = 1, vec_size2
            call MPI_Bcast(val(i, j), 1, MPI_REAL8, From, MPI_COMM_WORLD, obj%ierr)
         end do
      end do

   end subroutine
!################################################################

!################################################################
   recursive subroutine BcastMPIRealArray3(obj, From, val)
      class(MPI_), intent(inout)::obj
      integer(int32), intent(in) :: From
      real(real64), allocatable, intent(inout)::val(:, :, :)
      integer(int32) :: i, j, k, n, vec_size1, vec_size2, vec_size3

      !if(allocated(val) .and. From/=obj%myrank )then
      !    deallocate(val)
      !endif

      if (allocated(val)) then
         if (From /= obj%myrank) then
            deallocate (val)
         end if
      end if

      vec_size1 = 0
      vec_size2 = 0
      vec_size3 = 0
      if (From == obj%myrank) then
         if (.not. allocated(val)) then
            vec_size1 = -1
         else
            vec_size1 = size(val, 1)
         end if
      end if
      call obj%Bcast(From=From, val=vec_size1)
      ! if array is empty, return
      if (vec_size1 < 1) then
         return
      end if

      if (From == obj%myrank) then
         vec_size2 = size(val, 2)
      end if
      call obj%Bcast(From=From, val=vec_size2)

      if (From == obj%myrank) then
         vec_size3 = size(val, 3)
      end if
      call obj%Bcast(From=From, val=vec_size3)

      ! if array is empty, return
      if (vec_size2 < 1) then
         return
      end if
      if (vec_size3 < 1) then
         return
      end if

      if (From /= obj%myrank) then
         allocate (val(vec_size1, vec_size2, vec_size3))
      end if

      do i = 1, vec_size1
         do j = 1, vec_size2
            do k = 1, vec_size3
               call MPI_Bcast(val(i, j, k), 1, MPI_REAL8, From, MPI_COMM_WORLD, obj%ierr)
            end do
         end do
      end do

   end subroutine
!################################################################

!################################################################
   recursive subroutine BcastMPIIntArray3(obj, From, val)
      class(MPI_), intent(inout)::obj
      integer(int32), intent(in) :: From
      integer(int32), allocatable, intent(inout)::val(:, :, :)
      integer(int32) :: i, j, k, n, vec_size1, vec_size2, vec_size3

      !if(allocated(val) .and. From/=obj%myrank )then
      !    deallocate(val)
      !endif

      if (allocated(val)) then
         if (From /= obj%myrank) then
            deallocate (val)
         end if
      end if

      vec_size1 = 0
      vec_size2 = 0
      vec_size3 = 0
      if (From == obj%myrank) then
         if (.not. allocated(val)) then
            vec_size1 = -1
         else
            vec_size1 = size(val, 1)
         end if
      end if
      call obj%Bcast(From=From, val=vec_size1)
      ! if array is empty, return
      if (vec_size1 < 1) then
         return
      end if

      if (From == obj%myrank) then
         vec_size2 = size(val, 2)
      end if
      call obj%Bcast(From=From, val=vec_size2)

      if (From == obj%myrank) then
         vec_size3 = size(val, 3)
      end if
      call obj%Bcast(From=From, val=vec_size3)

      ! if array is empty, return
      if (vec_size2 < 1) then
         return
      end if
      if (vec_size3 < 1) then
         return
      end if

      if (From /= obj%myrank) then
         allocate (val(vec_size1, vec_size2, vec_size3))
      end if

      do i = 1, vec_size1
         do j = 1, vec_size2
            do k = 1, vec_size3
               call MPI_Bcast(val(i, j, k), 1, MPI_Integer, From, MPI_COMM_WORLD, obj%ierr)
            end do
         end do
      end do

   end subroutine
!################################################################

   recursive subroutine BcastMPIChar(obj, From, val)
      class(MPI_), intent(inout)::obj
      integer(int32), intent(inout)::From
      character(*), intent(inout)::val
      character(:), allocatable::val200
      integer(int32) :: i

      val200 = val
      call MPI_Bcast(val200(1:200), 200, MPI_CHARACTER, From, MPI_COMM_WORLD, obj%ierr)
      val = val200

   end subroutine

!################################################################

   recursive subroutine BcastMPICharN(obj, N, From, val)
      class(MPI_), intent(inout)::obj
      integer(int32), intent(in) :: N
      integer(int32), intent(in)::From
      character(len=N), intent(inout)::val(1:N)
      character(len=N)::valN(1:N)
      integer(int32) :: i

      call MPI_Bcast(valN(1:N), N, MPI_CHARACTER, From, MPI_COMM_WORLD, obj%ierr)

   end subroutine

!################################################################

   subroutine GatherMPIInt(obj, sendobj, sendcount, recvobj, recvcount, &
                           send_start_id, recv_start_id, To)
      class(MPI_), intent(inout)::obj
      integer(int32), intent(inout)::sendobj(:), recvobj(:)
      integer(int32), optional, intent(in)::sendcount, recvcount
      integer(int32)::sendcountv, recvcountv
      integer(int32), optional, intent(in)::send_start_id, recv_start_id, To
      integer(int32) :: i, s_start_id, r_start_id, ToID

      sendcountv = input(default=size(sendobj), option=sendcount)
      recvcountv = input(default=size(sendobj), option=recvcount)

      s_start_id = input(default=1, option=send_start_id)
      r_start_id = input(default=1, option=recv_start_id)
      ToID = input(default=0, option=To)

      call MPI_Gather(sendobj(s_start_id), sendcountv, MPI_integer, recvobj(r_start_id) &
                      , recvcountv, MPI_integer, ToID, MPI_COMM_WORLD, obj%ierr)
   end subroutine
!################################################################

!################################################################
   subroutine GatherMPIReal(obj, sendobj, sendcount, recvobj, recvcount, &
                            send_start_id, recv_start_id, To)
      class(MPI_), intent(inout)::obj
      real(real64), intent(inout)::sendobj(:), recvobj(:)
      integer(int32), optional, intent(in)::sendcount, recvcount
      integer(int32)::sendcountv, recvcountv
      integer(int32), optional, intent(in)::send_start_id, recv_start_id, To
      integer(int32) :: i, s_start_id, r_start_id, ToID

      sendcountv = input(default=size(sendobj), option=sendcount)
      recvcountv = input(default=size(sendobj), option=recvcount)

      s_start_id = input(default=1, option=send_start_id)
      r_start_id = input(default=1, option=recv_start_id)
      ToID = input(default=0, option=To)

      call MPI_Gather(sendobj(s_start_id), sendcountv, MPI_REAL8, recvobj(r_start_id) &
                      , recvcountv, MPI_REAL8, ToID, MPI_COMM_WORLD, obj%ierr)
   end subroutine
!################################################################

!################################################################
   subroutine ScatterMPIInt(obj, sendobj, sendcount, recvobj, recvcount, &
                            send_start_id, recv_start_id, From)
      class(MPI_), intent(inout)::obj
      integer(int32), intent(inout)::sendobj(:), recvobj(:)
      integer(int32), intent(in)::sendcount, recvcount
      integer(int32), optional, intent(in)::send_start_id, recv_start_id, From
      integer(int32) :: i, s_start_id, r_start_id, FromID

      s_start_id = input(default=1, option=send_start_id)
      r_start_id = input(default=1, option=recv_start_id)
      FromID = input(default=0, option=From)

      call MPI_Scatter(sendobj(s_start_id), sendcount, MPI_integer, recvobj(r_start_id) &
                       , recvcount, MPI_integer, FromID, MPI_COMM_WORLD, obj%ierr)
   end subroutine
!################################################################

!################################################################
   subroutine ScatterMPIReal(obj, sendobj, sendcount, recvobj, recvcount, &
                             send_start_id, recv_start_id, From)
      class(MPI_), intent(inout)::obj
      real(real64), intent(inout)::sendobj(:), recvobj(:)
      integer(int32), intent(in)::sendcount, recvcount
      integer(int32), optional, intent(in)::send_start_id, recv_start_id, From
      integer(int32) :: i, s_start_id, r_start_id, FromID

      s_start_id = input(default=1, option=send_start_id)
      r_start_id = input(default=1, option=recv_start_id)
      FromID = input(default=0, option=From)

      call MPI_Scatter(sendobj(s_start_id), sendcount, MPI_REAL8, recvobj(r_start_id) &
                       , recvcount, MPI_REAL8, FromID, MPI_COMM_WORLD, obj%ierr)
   end subroutine
!################################################################

!################################################################
   subroutine AllGatherMPIInt(obj, sendobj, sendcount, recvobj, recvcount, &
                              send_start_id, recv_start_id)
      class(MPI_), intent(inout)::obj
      integer(int32), intent(inout)::sendobj(:), recvobj(:)
      integer(int32), intent(in)::sendcount, recvcount
      integer(int32), optional, intent(in)::send_start_id, recv_start_id
      integer(int32) :: i, s_start_id, r_start_id

      s_start_id = input(default=1, option=send_start_id)
      r_start_id = input(default=1, option=recv_start_id)

      call MPI_AllGather(sendobj(s_start_id), sendcount, MPI_integer, recvobj(r_start_id) &
                         , recvcount, MPI_integer, MPI_COMM_WORLD, obj%ierr)
   end subroutine
!################################################################

!################################################################
   subroutine AllGatherMPIReal(obj, sendobj, sendcount, recvobj, recvcount, &
                               send_start_id, recv_start_id)
      class(MPI_), intent(inout)::obj
      real(real64), intent(inout)::sendobj(:), recvobj(:)
      integer(int32), intent(in)::sendcount, recvcount
      integer(int32), optional, intent(in)::send_start_id, recv_start_id
      integer(int32) :: i, s_start_id, r_start_id

      s_start_id = input(default=1, option=send_start_id)
      r_start_id = input(default=1, option=recv_start_id)

      call MPI_AllGather(sendobj(s_start_id), sendcount, MPI_REAL8, recvobj(r_start_id) &
                         , recvcount, MPI_REAL8, MPI_COMM_WORLD, obj%ierr)
   end subroutine
!################################################################

!################################################################
   subroutine AllGatherMPIGraph(obj, graph)
      class(MPI_), intent(inout)::obj
      type(Graph_), intent(inout) :: graph
      type(Vertex_), allocatable :: vertex(:)
      integer(int32), allocatable::sendobj(:), recvobj(:)
      real(real64), allocatable::sendobj_r(:), recvobj_r(:)

      real(real64) :: reval, x, y, z
      real(real64), allocatable :: reval_s(:), x_s(:), y_s(:), z_s(:)

      integer(int32) :: intval, ID, MyRank
      integer(int32), allocatable :: intval_s(:), ID_s(:), MyRank_s(:)

      integer(int32), allocatable::num_of_data(:), AdjacencyMatrix(:, :), AdjacencyData(:, :)
      integer(int32)::sendcount, recvcount
      integer(int32)::send_start_id, recv_start_id, sender_rank
      integer(int32) :: i, j, jj, k, s_start_id, r_start_id, numofvertex, totalnumvertex

      character(200) :: name

      ! graph1  => graph1 + graph2 + graph3 +graph4
      ! graph2  => graph1 + graph2 + graph3 +graph4
      ! graph3  => graph1 + graph2 + graph3 +graph4
      ! graph4  => graph1 + graph2 + graph3 +graph4

      ! get number of vertex.
      if (allocated(obj%Global_ID)) deallocate (obj%Global_ID)
      if (allocated(graph%Global_ID)) deallocate (graph%Global_ID)
      allocate (obj%Global_ID(size(graph%vertex)))
      allocate (graph%Global_ID(size(graph%vertex)))
      obj%Global_ID(:) = 0
      graph%Global_ID(:) = 0

      totalnumvertex = 0
      allocate (num_of_data(obj%petot))
      do i = 1, obj%petot
         numofvertex = size(graph%vertex)
         sender_rank = i - 1
         call obj%Bcast(From=sender_rank, val=numofvertex)
         num_of_data(i) = numofvertex
         totalnumvertex = totalnumvertex + numofvertex
      end do

      print *, "My rank is ", obj%myrank, "/toral vertex is :: ", totalnumvertex

      allocate (recvobj(totalnumvertex))
      allocate (sendobj(size(graph%vertex)))
      allocate (recvobj_r(totalnumvertex))
      allocate (sendobj_r(size(graph%vertex)))
      !allocate(reval_s(totalnumvertex) )
      !allocate(x_s(totalnumvertex) )
      !allocate(y_s(totalnumvertex) )
      !allocate(z_s(totalnumvertex) )
      !allocate( intval_s(totalnumvertex) )
      !allocate( ID_s(totalnumvertex) )
      !allocate( MyRank_s(totalnumvertex) )
      allocate (vertex(totalnumvertex))
      allocate (AdjacencyMatrix(totalnumvertex, totalnumvertex))
      AdjacencyMatrix(:, :) = 0
      !sendobj(:)=1

      ! allgather vertex
      ! vertex%reval
      reval = 0.0d0
      k = 1
      do i = 1, obj%petot
         sender_rank = i - 1
         do j = 1, num_of_data(i)
            if (i - 1 == obj%myrank) then
               reval = graph%vertex(j)%reval
            end if
            call obj%Bcast(From=sender_rank, val=reval)
            vertex(k)%reval = reval
            k = k + 1
         end do
      end do

      ! vertex%x
      x = 0.0d0
      k = 1
      do i = 1, obj%petot
         sender_rank = i - 1
         do j = 1, num_of_data(i)
            if (i - 1 == obj%myrank) then
               x = graph%vertex(j)%x
            end if
            call obj%Bcast(From=sender_rank, val=x)
            vertex(k)%x = x
            k = k + 1
         end do
      end do

      ! vertex%y
      y = 0.0d0
      k = 1
      do i = 1, obj%petot
         sender_rank = i - 1
         do j = 1, num_of_data(i)
            if (i - 1 == obj%myrank) then
               y = graph%vertex(j)%y
            end if
            call obj%Bcast(From=sender_rank, val=y)
            vertex(k)%y = y
            k = k + 1
         end do
      end do

      ! vertex%z
      z = 0.0d0
      k = 1
      do i = 1, obj%petot
         sender_rank = i - 1
         do j = 1, num_of_data(i)
            if (i - 1 == obj%myrank) then
               z = graph%vertex(j)%z
            end if
            call obj%Bcast(From=sender_rank, val=z)
            vertex(k)%z = z
            k = k + 1
         end do
      end do

      ! vertex%intval
      intval = 0
      k = 1
      do i = 1, obj%petot
         sender_rank = i - 1
         do j = 1, num_of_data(i)
            if (i - 1 == obj%myrank) then
               intval = graph%vertex(j)%intval
            end if
            call obj%Bcast(From=sender_rank, val=intval)
            vertex(k)%intval = intval
            k = k + 1
         end do
      end do

      ! vertex%ID
      ID = 0
      k = 1
      do i = 1, obj%petot
         sender_rank = i - 1
         do j = 1, num_of_data(i)
            if (i - 1 == obj%myrank) then
               ID = graph%vertex(j)%ID
            end if
            call obj%Bcast(From=sender_rank, val=ID)
            vertex(k)%ID = ID
            k = k + 1
         end do
      end do

      ! vertex%Myrank
      Myrank = 0
      k = 1
      do i = 1, obj%petot
         sender_rank = i - 1
         do j = 1, num_of_data(i)
            if (i - 1 == obj%myrank) then
               Myrank = graph%vertex(j)%Myrank
            end if
            call obj%Bcast(From=sender_rank, val=Myrank)
            vertex(k)%Myrank = Myrank
            k = k + 1
         end do
      end do

      ! vertex%name and Global_ID
      name = "NoName"
      k = 1
      do i = 1, obj%petot
         sender_rank = i - 1
         do j = 1, num_of_data(i)
            if (i - 1 == obj%myrank) then
               name = graph%vertex(j)%name
               obj%Global_ID(j) = k
               graph%Global_ID(j) = k
            end if
            call obj%Bcast(From=sender_rank, val=name)
            vertex(k)%name = name
            k = k + 1
         end do
      end do

      ! vertex%AdjacencyMatrix(:,:)
      ! あと、IDの振り直しを行うなど。

      intval = 0
      k = 0
      do i = 1, obj%petot
         sender_rank = i - 1

         do j = 1, num_of_data(i)
            do jj = 1, num_of_data(i)
               if (i - 1 == obj%myrank) then
                  intval = graph%AdjacencyMatrix(j, jj)
               end if
               call obj%Bcast(From=sender_rank, val=intval)
               AdjacencyMatrix(k + j, k + jj) = intval
            end do
         end do
         k = k + num_of_data(i)
      end do

      graph%AdjacencyMatrix = AdjacencyMatrix
      graph%Vertex = vertex
      graph%NumOfVertex = totalnumvertex

   end subroutine
!################################################################

!################################################################
   subroutine AlltoAllMPIInt(obj, sendobj, sendcount, recvobj, recvcount, &
                             send_start_id, recv_start_id)
      class(MPI_), intent(inout)::obj
      integer(int32), intent(inout)::sendobj(:), recvobj(:)
      integer(int32), intent(in)::sendcount, recvcount
      integer(int32), optional, intent(in)::send_start_id, recv_start_id
      integer(int32) :: i, s_start_id, r_start_id

      s_start_id = input(default=1, option=send_start_id)
      r_start_id = input(default=1, option=recv_start_id)

      call MPI_AlltoAll(sendobj(s_start_id), sendcount, MPI_integer, recvobj(r_start_id) &
                        , recvcount, MPI_integer, MPI_COMM_WORLD, obj%ierr)
   end subroutine
!################################################################

!################################################################
   subroutine AlltoAllMPIReal(obj, sendobj, sendcount, recvobj, recvcount, &
                              send_start_id, recv_start_id)
      class(MPI_), intent(inout)::obj
      real(real64), intent(inout)::sendobj(:), recvobj(:)
      integer(int32), intent(in)::sendcount, recvcount
      integer(int32), optional, intent(in)::send_start_id, recv_start_id
      integer(int32) :: i, s_start_id, r_start_id

      s_start_id = input(default=1, option=send_start_id)
      r_start_id = input(default=1, option=recv_start_id)

      call MPI_AlltoAll(sendobj(s_start_id), sendcount, MPI_REAL8, recvobj(r_start_id) &
                        , recvcount, MPI_REAL8, MPI_COMM_WORLD, obj%ierr)
   end subroutine
!################################################################

!################################################################
   subroutine ReduceMPIInt(obj, sendobj, recvobj, count, start, To, &
                           max, min, sum, prod, land, band, lor, bor, lxor, bxor, maxloc, minloc)
      class(MPI_), intent(inout)::obj
      integer(int32), intent(inout)::sendobj(:), recvobj(:)
      integer(int32), intent(in)::count
      integer(int32)  :: ToID, start_id
      integer(int32), optional, intent(in)::start, To
      logical, optional, intent(in)::max, min, sum, prod, land, band, lor
      logical, optional, intent(in)::bor, lxor, bxor, maxloc, minloc

      ToID = input(default=0, option=To)
      start_id = input(default=1, option=start)
      if (present(max)) then
         if (max .eqv. .true.) then

            call MPI_Reduce(sendobj(start_id), recvobj(start_id) &
                            , count, MPI_integer, ToID, MPI_MAX, MPI_COMM_WORLD, obj%ierr)
         end if
      end if
      if (present(min)) then
         if (min .eqv. .true.) then

            call MPI_Reduce(sendobj(start_id), recvobj(start_id) &
                            , count, MPI_integer, ToID, MPI_MIN, MPI_COMM_WORLD, obj%ierr)
         end if
      end if
      if (present(sum)) then
         if (sum .eqv. .true.) then

            call MPI_Reduce(sendobj(start_id), recvobj(start_id) &
                            , count, MPI_integer, ToID, MPI_SUM, MPI_COMM_WORLD, obj%ierr)
         end if
      end if
      if (present(prod)) then
         if (prod .eqv. .true.) then

            call MPI_Reduce(sendobj(start_id), recvobj(start_id) &
                            , count, MPI_integer, ToID, MPI_PROD, MPI_COMM_WORLD, obj%ierr)
         end if
      end if
      if (present(land)) then
         if (land .eqv. .true.) then

            call MPI_Reduce(sendobj(start_id), recvobj(start_id) &
                            , count, MPI_integer, ToID, MPI_LAND, MPI_COMM_WORLD, obj%ierr)
         end if
      end if
      if (present(band)) then
         if (band .eqv. .true.) then

            call MPI_Reduce(sendobj(start_id), recvobj(start_id) &
                            , count, MPI_integer, ToID, MPI_BAND, MPI_COMM_WORLD, obj%ierr)
         end if
      end if
      if (present(lor)) then
         if (lor .eqv. .true.) then

            call MPI_Reduce(sendobj(start_id), recvobj(start_id) &
                            , count, MPI_integer, ToID, MPI_LOR, MPI_COMM_WORLD, obj%ierr)
         end if
      end if
      if (present(bor)) then
         if (bor .eqv. .true.) then

            call MPI_Reduce(sendobj(start_id), recvobj(start_id) &
                            , count, MPI_integer, ToID, MPI_BOR, MPI_COMM_WORLD, obj%ierr)
         end if
      end if
      if (present(lxor)) then
         if (lxor .eqv. .true.) then

            call MPI_Reduce(sendobj(start_id), recvobj(start_id) &
                            , count, MPI_integer, ToID, MPI_LXOR, MPI_COMM_WORLD, obj%ierr)
         end if
      end if
      if (present(bxor)) then
         if (bxor .eqv. .true.) then

            call MPI_Reduce(sendobj(start_id), recvobj(start_id) &
                            , count, MPI_integer, ToID, MPI_BXOR, MPI_COMM_WORLD, obj%ierr)
         end if
      end if
      if (present(maxloc)) then
         if (maxloc .eqv. .true.) then

            call MPI_Reduce(sendobj(start_id), recvobj(start_id) &
                            , count, MPI_integer, ToID, MPI_MAXLOC, MPI_COMM_WORLD, obj%ierr)
         end if
      end if
      if (present(minloc)) then
         if (minloc .eqv. .true.) then
            call MPI_Reduce(sendobj(start_id), recvobj(start_id) &
                            , count, MPI_integer, ToID, MPI_MINLOC, MPI_COMM_WORLD, obj%ierr)
         end if
      end if

   end subroutine
!################################################################

!################################################################
   subroutine ReduceMPIReal(obj, sendobj, recvobj, count, start, To, &
                            max, min, sum, prod, land, band, lor, bor, lxor, bxor, maxloc, minloc)
      class(MPI_), intent(inout)::obj
      real(real64), intent(inout)::sendobj(:), recvobj(:)
      integer(int32), intent(in)::count
      integer(int32)  :: ToID, start_id
      integer(int32), optional, intent(in)::start, To
      logical, optional, intent(in)::max, min, sum, prod, land, band, lor
      logical, optional, intent(in)::bor, lxor, bxor, maxloc, minloc

      ToID = input(default=0, option=To)
      start_id = input(default=1, option=start)
      if (present(max)) then
         if (max .eqv. .true.) then

            call MPI_Reduce(sendobj(start_id), recvobj(start_id) &
                            , count, MPI_REAL8, ToID, MPI_MAX, MPI_COMM_WORLD, obj%ierr)
         end if
      end if
      if (present(min)) then
         if (min .eqv. .true.) then

            call MPI_Reduce(sendobj(start_id), recvobj(start_id) &
                            , count, MPI_REAL8, ToID, MPI_MIN, MPI_COMM_WORLD, obj%ierr)
         end if
      end if
      if (present(sum)) then
         if (sum .eqv. .true.) then

            call MPI_Reduce(sendobj(start_id), recvobj(start_id) &
                            , count, MPI_REAL8, ToID, MPI_SUM, MPI_COMM_WORLD, obj%ierr)
         end if
      end if
      if (present(prod)) then
         if (prod .eqv. .true.) then

            call MPI_Reduce(sendobj(start_id), recvobj(start_id) &
                            , count, MPI_REAL8, ToID, MPI_PROD, MPI_COMM_WORLD, obj%ierr)
         end if
      end if
      if (present(land)) then
         if (land .eqv. .true.) then

            call MPI_Reduce(sendobj(start_id), recvobj(start_id) &
                            , count, MPI_REAL8, ToID, MPI_LAND, MPI_COMM_WORLD, obj%ierr)
         end if
      end if
      if (present(band)) then
         if (band .eqv. .true.) then

            call MPI_Reduce(sendobj(start_id), recvobj(start_id) &
                            , count, MPI_REAL8, ToID, MPI_BAND, MPI_COMM_WORLD, obj%ierr)
         end if
      end if
      if (present(lor)) then
         if (lor .eqv. .true.) then

            call MPI_Reduce(sendobj(start_id), recvobj(start_id) &
                            , count, MPI_REAL8, ToID, MPI_LOR, MPI_COMM_WORLD, obj%ierr)
         end if
      end if
      if (present(bor)) then
         if (bor .eqv. .true.) then

            call MPI_Reduce(sendobj(start_id), recvobj(start_id) &
                            , count, MPI_REAL8, ToID, MPI_BOR, MPI_COMM_WORLD, obj%ierr)
         end if
      end if
      if (present(lxor)) then
         if (lxor .eqv. .true.) then

            call MPI_Reduce(sendobj(start_id), recvobj(start_id) &
                            , count, MPI_REAL8, ToID, MPI_LXOR, MPI_COMM_WORLD, obj%ierr)
         end if
      end if
      if (present(bxor)) then
         if (bxor .eqv. .true.) then

            call MPI_Reduce(sendobj(start_id), recvobj(start_id) &
                            , count, MPI_REAL8, ToID, MPI_BXOR, MPI_COMM_WORLD, obj%ierr)
         end if
      end if
      if (present(maxloc)) then
         if (maxloc .eqv. .true.) then

            call MPI_Reduce(sendobj(start_id), recvobj(start_id) &
                            , count, MPI_REAL8, ToID, MPI_MAXLOC, MPI_COMM_WORLD, obj%ierr)
         end if
      end if
      if (present(minloc)) then
         if (minloc .eqv. .true.) then
            call MPI_Reduce(sendobj(start_id), recvobj(start_id) &
                            , count, MPI_REAL8, ToID, MPI_MINLOC, MPI_COMM_WORLD, obj%ierr)
         end if
      end if

   end subroutine
!################################################################

!################################################################
   subroutine AllReduceMPIInt(obj, sendobj, recvobj, count, start, &
                              max, min, sum, prod, land, band, lor, bor, lxor, bxor, maxloc, minloc)
      class(MPI_), intent(inout)::obj
      integer(int32), intent(inout)::sendobj(:), recvobj(:)
      integer(int32), intent(in)::count
      integer(int32)  :: start_id
      integer(int32), optional, intent(in)::start
      logical, optional, intent(in)::max, min, sum, prod, land, band, lor
      logical, optional, intent(in)::bor, lxor, bxor, maxloc, minloc

      start_id = input(default=1, option=start)
      if (present(max)) then
         if (max .eqv. .true.) then

            call MPI_AllReduce(sendobj(start_id), recvobj(start_id) &
                               , count, MPI_integer, MPI_MAX, MPI_COMM_WORLD, obj%ierr)
         end if
      end if
      if (present(min)) then
         if (min .eqv. .true.) then

            call MPI_AllReduce(sendobj(start_id), recvobj(start_id) &
                               , count, MPI_integer, MPI_MIN, MPI_COMM_WORLD, obj%ierr)
         end if
      end if
      if (present(sum)) then
         if (sum .eqv. .true.) then

            call MPI_AllReduce(sendobj(start_id), recvobj(start_id) &
                               , count, MPI_integer, MPI_SUM, MPI_COMM_WORLD, obj%ierr)
         end if
      end if
      if (present(prod)) then
         if (prod .eqv. .true.) then

            call MPI_AllReduce(sendobj(start_id), recvobj(start_id) &
                               , count, MPI_integer, MPI_PROD, MPI_COMM_WORLD, obj%ierr)
         end if
      end if
      if (present(land)) then
         if (land .eqv. .true.) then

            call MPI_AllReduce(sendobj(start_id), recvobj(start_id) &
                               , count, MPI_integer, MPI_LAND, MPI_COMM_WORLD, obj%ierr)
         end if
      end if
      if (present(band)) then
         if (band .eqv. .true.) then

            call MPI_AllReduce(sendobj(start_id), recvobj(start_id) &
                               , count, MPI_integer, MPI_BAND, MPI_COMM_WORLD, obj%ierr)
         end if
      end if
      if (present(lor)) then
         if (lor .eqv. .true.) then

            call MPI_AllReduce(sendobj(start_id), recvobj(start_id) &
                               , count, MPI_integer, MPI_LOR, MPI_COMM_WORLD, obj%ierr)
         end if
      end if
      if (present(bor)) then
         if (bor .eqv. .true.) then

            call MPI_AllReduce(sendobj(start_id), recvobj(start_id) &
                               , count, MPI_integer, MPI_BOR, MPI_COMM_WORLD, obj%ierr)
         end if
      end if
      if (present(lxor)) then
         if (lxor .eqv. .true.) then

            call MPI_AllReduce(sendobj(start_id), recvobj(start_id) &
                               , count, MPI_integer, MPI_LXOR, MPI_COMM_WORLD, obj%ierr)
         end if
      end if
      if (present(bxor)) then
         if (bxor .eqv. .true.) then

            call MPI_AllReduce(sendobj(start_id), recvobj(start_id) &
                               , count, MPI_integer, MPI_BXOR, MPI_COMM_WORLD, obj%ierr)
         end if
      end if
      if (present(maxloc)) then
         if (maxloc .eqv. .true.) then

            call MPI_AllReduce(sendobj(start_id), recvobj(start_id) &
                               , count, MPI_integer, MPI_MAXLOC, MPI_COMM_WORLD, obj%ierr)
         end if
      end if
      if (present(minloc)) then
         if (minloc .eqv. .true.) then
            call MPI_AllReduce(sendobj(start_id), recvobj(start_id) &
                               , count, MPI_integer, MPI_MINLOC, MPI_COMM_WORLD, obj%ierr)
         end if
      end if

   end subroutine
!################################################################

!################################################################
   subroutine AllReduceMPIReal(obj, sendobj, recvobj, count, start, &
                               max, min, sum, prod, land, band, lor, bor, lxor, bxor, maxloc, minloc)
      class(MPI_), intent(inout)::obj
      real(real64), intent(inout)::sendobj(:), recvobj(:)
      integer(int32), intent(in)::count
      integer(int32)  :: start_id
      integer(int32), optional, intent(in)::start
      logical, optional, intent(in)::max, min, sum, prod, land, band, lor
      logical, optional, intent(in)::bor, lxor, bxor, maxloc, minloc

      start_id = input(default=1, option=start)
      if (present(max)) then
         if (max .eqv. .true.) then

            call MPI_AllReduce(sendobj(start_id), recvobj(start_id) &
                               , count, MPI_REAL8, MPI_MAX, MPI_COMM_WORLD, obj%ierr)
         end if
      end if
      if (present(min)) then
         if (min .eqv. .true.) then

            call MPI_AllReduce(sendobj(start_id), recvobj(start_id) &
                               , count, MPI_REAL8, MPI_MIN, MPI_COMM_WORLD, obj%ierr)
         end if
      end if
      if (present(sum)) then
         if (sum .eqv. .true.) then

            call MPI_AllReduce(sendobj(start_id), recvobj(start_id) &
                               , count, MPI_REAL8, MPI_SUM, MPI_COMM_WORLD, obj%ierr)
         end if
      end if
      if (present(prod)) then
         if (prod .eqv. .true.) then

            call MPI_AllReduce(sendobj(start_id), recvobj(start_id) &
                               , count, MPI_REAL8, MPI_PROD, MPI_COMM_WORLD, obj%ierr)
         end if
      end if
      if (present(land)) then
         if (land .eqv. .true.) then

            call MPI_AllReduce(sendobj(start_id), recvobj(start_id) &
                               , count, MPI_REAL8, MPI_LAND, MPI_COMM_WORLD, obj%ierr)
         end if
      end if
      if (present(band)) then
         if (band .eqv. .true.) then

            call MPI_AllReduce(sendobj(start_id), recvobj(start_id) &
                               , count, MPI_REAL8, MPI_BAND, MPI_COMM_WORLD, obj%ierr)
         end if
      end if
      if (present(lor)) then
         if (lor .eqv. .true.) then

            call MPI_AllReduce(sendobj(start_id), recvobj(start_id) &
                               , count, MPI_REAL8, MPI_LOR, MPI_COMM_WORLD, obj%ierr)
         end if
      end if
      if (present(bor)) then
         if (bor .eqv. .true.) then

            call MPI_AllReduce(sendobj(start_id), recvobj(start_id) &
                               , count, MPI_REAL8, MPI_BOR, MPI_COMM_WORLD, obj%ierr)
         end if
      end if
      if (present(lxor)) then
         if (lxor .eqv. .true.) then

            call MPI_AllReduce(sendobj(start_id), recvobj(start_id) &
                               , count, MPI_REAL8, MPI_LXOR, MPI_COMM_WORLD, obj%ierr)
         end if
      end if
      if (present(bxor)) then
         if (bxor .eqv. .true.) then

            call MPI_AllReduce(sendobj(start_id), recvobj(start_id) &
                               , count, MPI_REAL8, MPI_BXOR, MPI_COMM_WORLD, obj%ierr)
         end if
      end if
      if (present(maxloc)) then
         if (maxloc .eqv. .true.) then

            call MPI_AllReduce(sendobj(start_id), recvobj(start_id) &
                               , count, MPI_REAL8, MPI_MAXLOC, MPI_COMM_WORLD, obj%ierr)
         end if
      end if
      if (present(minloc)) then
         if (minloc .eqv. .true.) then
            call MPI_AllReduce(sendobj(start_id), recvobj(start_id) &
                               , count, MPI_REAL8, MPI_MINLOC, MPI_COMM_WORLD, obj%ierr)
         end if
      end if

   end subroutine
!################################################################

!################################################################
   subroutine EndMPI(obj)
      class(MPI_), intent(inout)::obj
      integer(int32) :: i

      call MPI_barrier(mpi_comm_world, obj%ierr)
      obj%etime = mpi_wtime()

      if (obj%MyRank == 0) then
         print *, " ############################################ "
      end if
      do i = 1, obj%Petot
         if (obj%MyRank + 1 == obj%Petot) then
            print *, " Computation time (sec.) ::  ", obj%etime - obj%stime
         end if
      end do
      if (obj%MyRank == 0) then
         print *, " Number of cores         ::  ", obj%Petot
         print *, " ############################################ "
      end if

      if (obj%EP_result_summary%active) then
         call obj%EP_result_summary%close()
      end if

      call mpi_finalize(obj%ierr)

   end subroutine
!################################################################

!################################################################
   subroutine getLapTimeMPI(obj, comment)
      class(MPI_), intent(inout)::obj
      character(*), optional, intent(in)::comment

      obj%LapTimeStep = obj%LapTimeStep + 1
      obj%laptime(obj%LapTimeStep) = MPI_Wtime()

      if (present(comment)) then
         obj%comments(obj%LapTimeStep)%comment = comment
      end if

   end subroutine
!################################################################

!################################################################
   subroutine showLapTimeMPI(obj, clength, rank)
      class(MPI_), intent(inout)::obj
      integer(int32), optional, intent(in)::rank, cLength
      integer(int32) :: i, n
      real(real64) :: rate

      if (present(clength)) then
         n = clength
      else
         n = 15
      end if

      if (present(rank)) then
         if (obj%MyRank == rank) then
            print *, " ############################################ "
            do i = 2, obj%LapTimeStep
               rate = (obj%LapTime(i) - obj%LapTime(i - 1))/(obj%LapTime(obj%LapTimeStep) - obj%LapTime(1))
              print *, obj%comments(i)%comment(1:n), " : ", obj%LapTime(i) - obj%LapTime(i - 1), "(sec.)", real(rate*100.0d0), "(%)"
            end do
            print *, " ############################################ "
         end if
      else
         if (obj%MyRank == 0) then
            print *, " ############################################ "
            do i = 2, obj%LapTimeStep
               rate = (obj%LapTime(i) - obj%LapTime(i - 1))/(obj%LapTime(obj%LapTimeStep) - obj%LapTime(1))
              print *, obj%comments(i)%comment(1:n), " : ", obj%LapTime(i) - obj%LapTime(i - 1), "(sec.)", real(rate*100.0d0), "(%)"
            end do
            print *, " ############################################ "
         end if
      end if
      obj%etime = mpi_wtime()

      if (obj%MyRank == 0) then
         print *, " ############################################ "
      end if
      do i = 1, obj%Petot
         if (obj%MyRank + 1 == obj%Petot) then
            print *, " Computation time (sec.) ::  ", obj%etime - obj%stime
         end if
      end do
      if (obj%MyRank == 0) then
         print *, " Number of cores         ::  ", obj%Petot
         print *, " ############################################ "
      end if

   end subroutine
!################################################################

!################################################################
   subroutine CopyMPI(obj, OriginComm, NewCommLayerID)
      class(MPI_), intent(inout)::obj
      integer(int32), optional, intent(in)::OriginComm, NewCommLayerID

      call MPI_COMM_DUP(input(default=MPI_COMM_WORLD, option=OriginComm), &
                        obj%Comm(input(default=2, option=NewCommLayerID)), obj%ierr)

   end subroutine
!################################################################

!################################################################
   subroutine SplitMPI(obj, OriginComm, NewCommLayerID, key)
      class(MPI_), intent(inout)::obj
      integer(int32), optional, intent(in)::OriginComm, NewCommLayerID, key

      !call MPI_COMM_SPLIT(input(default=MPI_COMM_WORLD,option=OriginComm),&
      !    obj%key(input(default=0,option=key)),&
      !    obj%Comm(input(default=2,option=NewCommLayerID) ) , obj%ierr)

   end subroutine
!################################################################

!################################################################
   subroutine FreeMPI(obj, CommLayerID)
      class(MPI_), intent(inout)::obj
      integer(int32), optional, intent(in) :: CommLayerID

      !call MPI_COMM_FREE(input(default=MPI_COMM_WORLD,option=obj%Comm(CommLayerID) ), obj%ierr)

      !call MPI_COMM_FREE(MPI_COMM_WORLD, obj%ierr)

   end subroutine
!################################################################

!################################################################
   subroutine syncGraphMPI(obj, graph)
      class(MPI_), intent(inout) ::obj
      type(Graph_), intent(inout) ::graph
      integer(int32), allocatable :: AdjacencyMatrix(:, :)
      integer(int32) :: i, j, size1, size2, n

      size1 = size(graph%AdjacencyMatrix, 1)
      size2 = size(graph%AdjacencyMatrix, 2)
      allocate (AdjacencyMatrix(size1, size2))
      AdjacencyMatrix(:, :) = 0

      ! sync only edge
      do i = 1, obj%petot
         n = i - 1
         if (n == obj%myrank) then
            AdjacencyMatrix = graph%AdjacencyMatrix
         end if
         call obj%Bcast(From=n, val=AdjacencyMatrix)
         call graph%sync(AdjacencyMatrix)
      end do

   end subroutine
!################################################################

   pure function num_imagesMPI(obj) result(ret)
      class(MPI_), intent(in) :: obj
      integer(int32) :: ret
      ret = obj%petot
   end function
!################################################################

   pure function this_imageMPI(obj) result(ret)
      class(MPI_), intent(in) :: obj
      integer(int32) :: ret
      ret = obj%myrank + 1
   end function
!################################################################

   subroutine isend_irecvRealVectorMPI(this, sendobj, recvobj, send_recv_rank, debug)
      class(MPI_), intent(inout) :: this
      real(real64), intent(in)    :: sendobj(:)
      real(real64), intent(inout) :: recvobj(:)
      integer(int32), intent(in)  :: send_recv_rank(:)
      integer(int32) :: i, n, ireq, ierr, tag
      integer(int32) :: mpistat(MPI_STATUS_SIZE)
      logical, optional, intent(in) :: debug

      call this%barrier()
      if (present(debug)) then
         if (debug) then
            if (size(send_recv_rank) > 100000) then
               print *, "[CAUTION!] isend_irecv >> communication cost increases O(N^2)/ "
               print *, "           For hevy workflow, please consider to use MPI_BCAST"
            end if
         end if
      end if

      ! ISEND :: >>> NON-BLOCKING
      n = size(sendobj)
      tag = 0

      do i = 1, n
         !tag = tag+1
         if (send_recv_rank(i) > this%petot - 1) cycle
         call MPI_IRECV(recvobj(i), 1, MPI_REAL8, send_recv_rank(i), &
                        0, MPI_COMM_WORLD, ireq, ierr)
      end do

      if (present(debug)) then
         if (debug) then
            print *, "[ok] isend_irecv >> RANK :: ", this%myrank, "[IRECV:DONE]"
         end if
      end if

      do i = 1, n
         !tag = tag+1
         if (send_recv_rank(i) > this%petot - 1) cycle
         call MPI_ISEND(sendobj(i), 1, MPI_REAL8, send_recv_rank(i), &
                        tag, MPI_COMM_WORLD, ireq, ierr)

      end do

      if (present(debug)) then
         if (debug) then
            print *, "[ok] isend_isend >> RANK :: ", this%myrank, "[ISEND:DONE]"
         end if
      end if

      call MPI_WAIT(ireq, mpistat, ierr)

      call this%barrier()
      this%ierr = ierr
   end subroutine
! ###################################################

!################################################################

   subroutine isend_irecvInt32VectorMPI(this, sendobj, recvobj, send_recv_rank, debug)
      class(MPI_), intent(inout) :: this
      integer(int32), intent(in)    :: sendobj(:)
      integer(int32), intent(inout) :: recvobj(:)
      integer(int32), intent(in)  :: send_recv_rank(:)
      integer(int32) :: i, n, ireq, ierr, tag
      integer(int32) :: mpistat(MPI_STATUS_SIZE)
      logical, optional, intent(in) :: debug

      call this%barrier()
      if (present(debug)) then
         if (debug) then
            if (size(send_recv_rank) > 100000) then
               print *, "[CAUTION!] isend_irecv >> communication cost increases O(N^2)/ "
               print *, "           For hevy workflow, please consider to use MPI_BCAST"
            end if
         end if
      end if

      ! ISEND :: >>> NON-BLOCKING
      n = size(sendobj)
      tag = 0

      do i = 1, n
         !tag = tag+1
         if (send_recv_rank(i) > this%petot - 1) cycle
         call MPI_IRECV(recvobj(i), 1, MPI_REAL8, send_recv_rank(i), &
                        0, MPI_COMM_WORLD, ireq, ierr)
      end do

      if (present(debug)) then
         if (debug) then
            print *, "[ok] isend_irecv >> RANK :: ", this%myrank, "[IRECV:DONE]"
         end if
      end if

      do i = 1, n
         !tag = tag+1
         if (send_recv_rank(i) > this%petot - 1) cycle
         call MPI_ISEND(sendobj(i), 1, MPI_REAL8, send_recv_rank(i), &
                        tag, MPI_COMM_WORLD, ireq, ierr)

      end do

      if (present(debug)) then
         if (debug) then
            print *, "[ok] isend_isend >> RANK :: ", this%myrank, "[ISEND:DONE]"
         end if
      end if

      call MPI_WAIT(ireq, mpistat, ierr)

      call this%barrier()
      this%ierr = ierr
   end subroutine
! ###################################################

   subroutine EP_set_variable_by_rangeMPI(this, var, var_range, N)
      class(MPI_), intent(inout) :: this
      real(real64), target, intent(in)   :: var
      real(real64), intent(in)   :: var_range(1:2)
      integer(int32), intent(in) :: N
      integer(int32) :: i
      type(Random_) :: random

      this%MPI_LAST_JOB_NUMBER = this%MPI_LAST_JOB_NUMBER + 1
      this%MPI_JOB(this%MPI_LAST_JOB_NUMBER)%var => var
      this%MPI_JOB(this%MPI_LAST_JOB_NUMBER)%var_list = linspace(var_range, N)

   end subroutine
! ###################################################

! ###################################################

   subroutine EP_set_variable_by_listMPI(this, var, var_list)
      class(MPI_), intent(inout) :: this
      real(real64), target, intent(in)   :: var
      real(real64), intent(in)   :: var_list(:)
      integer(int32) :: i
      type(Random_) :: random

      this%MPI_LAST_JOB_NUMBER = this%MPI_LAST_JOB_NUMBER + 1
      this%MPI_JOB(this%MPI_LAST_JOB_NUMBER)%var => var
      this%MPI_JOB(this%MPI_LAST_JOB_NUMBER)%var_list = var_list

   end subroutine
! ###################################################

! ###################################################
   function EP_get_variableMPI(this) result(ret)
      class(MPI_), intent(inout) :: this
      logical :: ret
      integer(int32) :: var_id, n, i, j
      integer(int32), allocatable :: order_list(:), part_rank(:)
      type(Random_) :: random

      ret = .false.

      if (.not. allocated(this%EP_ALL_VARIABLE_LIST)) then

         if (this%MPI_LAST_JOB_NUMBER == 1) then
            this%EP_ALL_VARIABLE_LIST = zeros(size(this%MPI_JOB(1)%var_list), 1)
            this%EP_ALL_VARIABLE_LIST(:, 1) = this%MPI_JOB(1)%var_list(:)
         elseif (this%MPI_LAST_JOB_NUMBER == 2) then
            this%EP_ALL_VARIABLE_LIST = cartesian_product( &
                                        this%MPI_JOB(1)%var_list, &
                                        this%MPI_JOB(2)%var_list)
         elseif (this%MPI_LAST_JOB_NUMBER >= 3) then
            this%EP_ALL_VARIABLE_LIST = cartesian_product( &
                                        this%MPI_JOB(1)%var_list, &
                                        this%MPI_JOB(2)%var_list)
            do var_id = 3, this%MPI_LAST_JOB_NUMBER
               this%EP_ALL_VARIABLE_LIST = cartesian_product( &
                                           this%EP_ALL_VARIABLE_LIST, &
                                           this%MPI_JOB(var_id)%var_list)
            end do
         else
            print *, "ERROR :: EP_get_variableMPI"
            stop
         end if

      end if

      if (.not. allocated(this%EP_MY_VARIABLE_LIST)) then
         ! this%EP_ALL_VARIABLE_LIST
         n = size(this%EP_ALL_VARIABLE_LIST, 1)
         if (this%myrank == 0) then
            order_list = [(i, i=1, n)]
            call random%shuffle(order_list)
         end if

         call this%Bcast(from=0, val=order_list)
         call this%createStack(n)
         this%EP_MY_VARIABLE_LIST = this%EP_ALL_VARIABLE_LIST(this%localstack(:), :)
      end if

      if (.not. allocated(this%EP_MY_TASK_SCHEDULE)) then
         allocate (this%EP_MY_TASK_SCHEDULE(size(this%EP_MY_VARIABLE_LIST, 1)))
         this%EP_MY_TASK_SCHEDULE(:) = .false.
      end if

      ret = .false.
      do i = 1, size(this%EP_MY_TASK_SCHEDULE)
         if (this%EP_MY_TASK_SCHEDULE(i)) then
            cycle
         else
            ! un-done
            do j = 1, size(this%EP_MY_VARIABLE_LIST, 2)
               ! substitute variables
               this%MPI_JOB(j)%var = this%EP_MY_VARIABLE_LIST(i, j)
            end do
            this%EP_MY_CURRENT_TASK_ID = i
            this%EP_MY_TASK_SCHEDULE(i) = .true.
            ret = .true.
            return
         end if
      end do

   end function
! ###################################################

   subroutine EP_set_resultMPI(this, result_value)
      class(MPI_), intent(inout) :: this
      real(real64), intent(in) :: result_value(:)
      type(IO_) :: f

      if (.not. allocated(this%EP_MY_RESULT_LIST)) then
         this%EP_MY_RESULT_LIST = zeros(size(this%EP_MY_VARIABLE_LIST, 1), size(result_value))
      end if

      this%EP_MY_RESULT_LIST(this%EP_MY_CURRENT_TASK_ID, :) = result_value(:)

   end subroutine
! ###################################################

! ###################################################

   subroutine EP_write_resultMPI(this)
      class(MPI_), intent(inout) :: this
      type(IO_) :: f

      if (.not. this%EP_result_summary%active) then
         call this%EP_result_summary%open("result"+zfill(this%myrank, 7) + ".tsv", "w")
      end if

      call this%EP_result_summary%write(this%EP_MY_VARIABLE_LIST(this%EP_MY_CURRENT_TASK_ID:this%EP_MY_CURRENT_TASK_ID, :) &
                                        .h.this%EP_MY_RESULT_LIST(this%EP_MY_CURRENT_TASK_ID:this%EP_MY_CURRENT_TASK_ID, :))

   end subroutine
! ###################################################
   function EP_num_variavleMPI(this) result(ret)
      class(MPI_), intent(in) :: this
      integer(int32) :: ret

      ret = this%MPI_LAST_JOB_NUMBER

   end function
! ###################################################

! ###################################################
   function EP_min_varMPI(this) result(ret)
      class(MPI_), intent(in) :: this
      real(real64), allocatable :: ret(:, :)
      integer(int32) :: i, idx

      ret = zeros(this%EP_num_variavle() + 1, size(this%EP_MY_RESULT_LIST, 2))
      do i = 1, size(this%EP_MY_RESULT_LIST, 2)
         idx = minvalid(this%EP_MY_RESULT_LIST(:, i))
         ret(1:this%EP_num_variavle(), i) = this%EP_MY_VARIABLE_LIST(idx, :)
         ret(this%EP_num_variavle() + 1, i) = this%EP_MY_RESULT_LIST(idx, i)
      end do

   end function
! ###################################################

! ###################################################
   function EP_max_varMPI(this) result(ret)
      class(MPI_), intent(in) :: this
      real(real64), allocatable :: ret(:, :)
      integer(int32) :: i, idx

      ret = zeros(this%EP_num_variavle() + 1, size(this%EP_MY_RESULT_LIST, 2))
      do i = 1, size(this%EP_MY_RESULT_LIST, 2)
         idx = maxvalid(this%EP_MY_RESULT_LIST(:, i))
         ret(1:this%EP_num_variavle(), i) = this%EP_MY_VARIABLE_LIST(idx, :)
         ret(this%EP_num_variavle() + 1, i) = this%EP_MY_RESULT_LIST(idx, i)
      end do

   end function
! ###################################################

   subroutine fopen_MPI(this, filename, io_option)
      class(MPI_), intent(inout)  :: this
      character(*), intent(in) :: filename, io_option

      this%filename = filename
      call this%file%open(filename + "_rank_"+zfill(this%myrank, 8), io_option)

   end subroutine
! ###################################################

! ###################################################
   subroutine fclose_MPI(this)
      class(MPI_), intent(inout)  :: this
      integer(int32) :: system_ret
      call this%file%close()
      if (this%myrank == 0) then
         system_ret = system("cat "+this%filename + "_rank_* > "+this%filename)
      end if

   end subroutine
! ###################################################

! ###################################################
   subroutine isend_Int32MPI(this, to, val, req, tag)
      class(MPI_), intent(inout) :: this
      integer(int32), intent(in) :: to
      integer(int32), intent(in) :: val
      integer(int32), intent(inout) :: req
      integer(int32), intent(in) :: tag

      call MPI_ISEND(val, 1, MPI_INTEGER4, to, tag, this%comm(1), req, this%ierr)

   end subroutine
! ###################################################

! ###################################################
   subroutine isend_Int32VectorMPI(this, to, val, req, tag)
      class(MPI_), intent(inout) :: this
      integer(int32), intent(in) :: to
      integer(int32), intent(in) :: val(:)
      integer(int32), intent(inout) :: req
      integer(int32), intent(in) :: tag

      call MPI_ISEND(val, size(val), MPI_INTEGER4, to, tag, this%comm(1), req, this%ierr)

   end subroutine
! ###################################################

! ###################################################
   subroutine isend_Real64MPI(this, to, val, req, tag)
      class(MPI_), intent(inout) :: this
      integer(int32), intent(in) :: to
      real(real64), intent(in) :: val
      integer(int32), intent(inout) :: req
      integer(int32), intent(in) :: tag

      call MPI_ISEND(val, 1, MPI_REAL8, to, tag, this%comm(1), req, this%ierr)

   end subroutine
! ###################################################

! ###################################################
   subroutine isend_Real64VectorMPI(this, to, val, req, tag)
      class(MPI_), intent(inout) :: this
      integer(int32), intent(in) :: to
      real(real64), intent(in) :: val(:)
      integer(int32), intent(inout) :: req
      integer(int32), intent(in) :: tag

      call MPI_ISEND(val, size(val), MPI_REAL8, to, tag, this%comm(1), req, this%ierr)

   end subroutine
! ###################################################

! ###################################################
   subroutine irecv_Int32MPI(this, from, val, req, tag)
      class(MPI_), intent(inout) :: this
      integer(int32), intent(in) :: from
      integer(int32), intent(inout) :: val
      integer(int32), intent(inout) :: req
      integer(int32), intent(in) :: tag

      call MPI_IRECV(val, 1, MPI_INTEGER4, from, tag, this%comm(1), req, this%ierr)

   end subroutine
! ###################################################

! ###################################################
   subroutine irecv_Int32VectorMPI(this, from, val, req, tag)
      class(MPI_), intent(inout) :: this
      integer(int32), intent(in) :: from
      integer(int32), intent(inout) :: val(:)
      integer(int32), intent(inout) :: req
      integer(int32), intent(in) :: tag

      call MPI_IRECV(val, size(val), MPI_INTEGER4, from, tag, this%comm(1), req, this%ierr)

   end subroutine
! ###################################################

! ###################################################
   subroutine irecv_Real64MPI(this, from, val, req, tag)
      class(MPI_), intent(inout) :: this
      integer(int32), intent(in) :: from
      real(real64), intent(inout) :: val
      integer(int32), intent(inout) :: req
      integer(int32), intent(in) :: tag

      call MPI_IRECV(val, 1, MPI_REAL8, from, tag, this%comm(1), req, this%ierr)

   end subroutine
! ###################################################

! ###################################################
   subroutine irecv_Real64VectorMPI(this, from, val, req, tag)
      class(MPI_), intent(inout) :: this
      integer(int32), intent(in) :: from
      real(real64), intent(inout) :: val(:)
      integer(int32), intent(inout) :: req
      integer(int32), intent(in) :: tag

      call MPI_IRECV(val, size(val), MPI_REAL8, from, tag, this%comm(1), req, this%ierr)

   end subroutine
! ###################################################

! ###################################################
   subroutine WaitAll_Int32MPI(this, send_req, recv_req)
      class(MPI_), intent(inout) :: this
      integer(int32), intent(in) :: send_req, recv_req
      integer(int32) :: reqs(2)
      integer(int32) :: mpistat(MPI_STATUS_SIZE, 2)

      reqs(1) = send_req
      reqs(2) = recv_req
      call MPI_Waitall(2, reqs, mpistat, this%ierr)

   end subroutine
! ###################################################

! ###################################################
   subroutine WaitAll_Int32VectorMPI(this, send_req, recv_req)
      class(MPI_), intent(inout) :: this
      integer(int32), intent(in) :: send_req(:), recv_req(:)
      integer(int32), allocatable :: reqs(:)
      integer(int32) :: mpistat(MPI_STATUS_SIZE, 2)

      reqs = send_req//recv_req
      call MPI_Waitall(size(reqs), reqs, mpistat, this%ierr)

   end subroutine
! ###################################################

   subroutine restart_point_intvec32_MPI(this, name, intvec)
      class(MPI_), intent(inout) :: this
      character(*), intent(in) :: name
      character(:), allocatable :: fname
      integer(int32), allocatable, intent(inout) :: intvec(:)
      type(IO_) :: f
      integer(int32) :: n, i

      ! restart-file name
      fname = "restart_r"+zfill(this%myrank, 6) + "_"+name + ".txt"

      ! restart-file format
      ! size,
      ! data

      if (this%mpi_restart_idf == 0) then
         if (f%exists(fname)) then
            ! first call after mpi%init()
            ! and has "Zense-No-Kioku(ZNK)"
            call f%open(fname, "r")
            read (f%fh, *) n
            if (allocated(intvec)) then
               deallocate (intvec)
            end if
            allocate (intvec(n))
            do i = 1, n
               read (f%fh, *) intvec(i)
            end do
            call f%close()
            this%mpi_restart_idf = 1
         else
            if (allocated(intvec)) then
               call f%open(fname, "w")
               n = size(intvec)
               write (f%fh, *) n
               do i = 1, n
                  write (f%fh, *) intvec(i)
               end do
               call f%close()
            end if
            this%mpi_restart_idf = 1
         end if
         return
      else
         ! second call after mpi%init()
         if (allocated(intvec)) then
            call f%open(fname, "w")
            n = size(intvec)
            write (f%fh, *) n
            do i = 1, n
               write (f%fh, *) intvec(i)
            end do
            call f%close()
         end if
         this%mpi_restart_idf = 1
      end if
   end subroutine

   subroutine restart_point_realvec64_MPI(this, name, dat_content)
      class(MPI_), intent(inout) :: this
      character(*), intent(in) :: name
      character(:), allocatable :: fname
      real(real64), allocatable, intent(inout) :: dat_content(:)
      type(IO_) :: f
      integer(int32) :: n, i

      ! restart-file name
      fname = "restart_r"+zfill(this%myrank, 6) + "_"+name + ".txt"

      ! restart-file format
      ! size,
      ! data

      if (this%mpi_restart_idf == 0) then
         if (f%exists(fname)) then
            ! first call after mpi%init()
            ! and has "Zense-No-Kioku(ZNK)"
            call f%open(fname, "r")
            read (f%fh, *) n
            if (allocated(dat_content)) then
               deallocate (dat_content)
            end if
            allocate (dat_content(n))
            do i = 1, n
               read (f%fh, *) dat_content(i)
            end do
            call f%close()
            this%mpi_restart_idf = 1
         else
            if (allocated(dat_content)) then
               call f%open(fname, "w")
               n = size(dat_content)
               write (f%fh, *) n
               do i = 1, n
                  write (f%fh, *) dat_content(i)
               end do
               call f%close()
            end if
            this%mpi_restart_idf = 1
         end if
         return
      else
         ! second call after mpi%init()
         if (allocated(dat_content)) then
            call f%open(fname, "w")
            n = size(dat_content)
            write (f%fh, *) n
            do i = 1, n
               write (f%fh, *) dat_content(i)
            end do
            call f%close()
         end if
         this%mpi_restart_idf = 1
      end if
   end subroutine

   subroutine restart_point_intmat64_MPI(this, name, dat_content)
      class(MPI_), intent(inout) :: this
      character(*), intent(in) :: name
      character(:), allocatable :: fname
      integer(int32), allocatable, intent(inout) :: dat_content(:, :)
      type(IO_) :: f
      integer(int32) :: n, i, m

      ! restart-file name
      fname = "restart_r"+zfill(this%myrank, 6) + "_"+name + ".txt"

      ! restart-file format
      ! size,
      ! data

      if (this%mpi_restart_idf == 0) then
         if (f%exists(fname)) then
            ! first call after mpi%init()
            ! and has "Zense-No-Kioku(ZNK)"
            call f%open(fname, "r")
            read (f%fh, *) n, m
            if (allocated(dat_content)) then
               deallocate (dat_content)
            end if
            allocate (dat_content(n, m))
            do i = 1, n
               read (f%fh, *) dat_content(i, :)
            end do
            call f%close()
            this%mpi_restart_idf = 1
         else
            if (allocated(dat_content)) then
               call f%open(fname, "w")
               n = size(dat_content, 1)
               m = size(dat_content, 2)
               write (f%fh, *) n, m
               do i = 1, n
                  write (f%fh, *) dat_content(i, :)
               end do
               call f%close()
            end if
            this%mpi_restart_idf = 1
         end if
         return
      else
         ! second call after mpi%init()
         if (allocated(dat_content)) then
            call f%open(fname, "w")
            n = size(dat_content, 1)
            m = size(dat_content, 2)
            write (f%fh, *) n, m
            do i = 1, n
               write (f%fh, *) dat_content(i, :)
            end do
            call f%close()
         end if
         this%mpi_restart_idf = 1
      end if
   end subroutine

   subroutine restart_point_realmat64_MPI(this, name, dat_content)
      class(MPI_), intent(inout) :: this
      character(*), intent(in) :: name
      character(:), allocatable :: fname
      real(real64), allocatable, intent(inout) :: dat_content(:, :)
      type(IO_) :: f
      integer(int32) :: n, i, m

      ! restart-file name
      fname = "restart_r"+zfill(this%myrank, 6) + "_"+name + ".txt"

      ! restart-file format
      ! size,
      ! data

      if (this%mpi_restart_idf == 0) then
         if (f%exists(fname)) then
            ! first call after mpi%init()
            ! and has "Zense-No-Kioku(ZNK)"
            call f%open(fname, "r")
            read (f%fh, *) n, m
            if (allocated(dat_content)) then
               deallocate (dat_content)
            end if
            allocate (dat_content(n, m))
            do i = 1, n
               read (f%fh, *) dat_content(i, :)
            end do
            call f%close()
            this%mpi_restart_idf = 1
         else
            if (allocated(dat_content)) then
               call f%open(fname, "w")
               n = size(dat_content, 1)
               m = size(dat_content, 2)
               write (f%fh, *) n, m
               do i = 1, n
                  write (f%fh, *) dat_content(i, :)
               end do
               call f%close()
            end if
            this%mpi_restart_idf = 1
         end if
         return
      else
         ! second call after mpi%init()
         if (allocated(dat_content)) then
            call f%open(fname, "w")
            n = size(dat_content, 1)
            m = size(dat_content, 2)
            write (f%fh, *) n, m
            do i = 1, n
               write (f%fh, *) dat_content(i, :)
            end do
            call f%close()
         end if
         this%mpi_restart_idf = 1
      end if
   end subroutine

end module
