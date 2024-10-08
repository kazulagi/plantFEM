module CSVClass
   use IOClass
   use ArrayClass

   implicit none

   type::CSVChar_
      character(200) :: char
   end type

   type::CSVinfo_
      real(real64) :: rval
      integer(int32) :: ival
      character(200) :: cval = " "
   contains
      procedure, public :: copy => copyCSVinfo
   end type

   type::CSV_
      type(CSVinfo_), allocatable :: info(:, :)
      real(real64), allocatable :: rval(:, :)
      integer(int32), allocatable :: ival(:, :)
      type(CSVChar_), allocatable :: charVal(:, :)
      character(200), allocatable :: cval(:, :)
      integer(int32), allocatable :: position(:, :)
   contains
      procedure, public :: open => importCSV
      procedure, public :: read => importCSV
      procedure, public :: export => exportCSV
      procedure, public :: import => importCSV
      procedure, public :: copy => copyCSV
   end type
contains

! ########################################################
   subroutine copyCSVinfo(obj, copy)
      class(CSVinfo_), intent(inout) :: obj
      class(CSVinfo_), intent(inout) :: copy

      copy%rval = obj%rval
      copy%ival = obj%ival
      copy%cval = obj%cval

   end subroutine
! ########################################################

! ########################################################
   function copyCSV(obj) result(copy)
      class(CSV_), intent(inout) :: obj
      class(CSV_), allocatable:: copy
      integer(int32) :: num_data, i

      if (allocated(copy%info)) then
         deallocate (copy%info)
      end if
      if (allocated(copy%position)) then
         deallocate (copy%position)
      end if
      num_data = size(obj%info, 1)
      allocate (copy%info(num_data, 1))
      allocate (copy%position(num_data, 2))

      do i = 1, num_data
         call obj%info(i, 1)%copy(copy%info(i, 1))
      end do
      copy%position(:, :) = obj%position(:, :)

   end function
! ########################################################

! ########################################################
   subroutine importCSV(obj, path, name, extention)
      class(CSV_), intent(inout) :: obj
      character(*), intent(in) :: path, name, extention
      integer, parameter :: max_line_len = 4000
      type(IO_) :: f
      integer(int32) :: n, i, j, k, itr, m, in, ii, jj
      real(real64) :: re
      character(max_line_len)::line
      character(:), allocatable::word
      character(:), allocatable::miniword
      logical :: tf
      n = 0

      call f%open(path, name, extention)
      do
         line = " "
         read (f%fh, '(A)', end=100) line(1:4000)
         n = n + 1

         ! get info from line
         i = 1
         k = 1
         itr = 1
         do
            j = index(line(i:), ",")
            !jj= index( line(j:),"," )
            !print *, j, jj
!
            if (j == 0) then
               word = line(k:)

            else
               if (j > i) then
                  word = line(k:j)
               else
                  word = line(k:i)
               end if
            end if

            ! got word

            if (word /= "," .and. word /= "") then
               !print *, "id",n,",",itr," : ",word
               !if(.not. allocated(obj%info) )then
               !    allocate(obj%info(0,1) )
               !endif
               !print *, word,i,j,k

               if (.not. allocated(obj%rval)) then
                  allocate (obj%rval(0, 1))
               end if
               if (.not. allocated(obj%ival)) then
                  allocate (obj%ival(0, 1))
               end if
               if (.not. allocated(obj%cval)) then
                  allocate (obj%cval(0, 200))
               end if
               if (.not. allocated(obj%position)) then
                  allocate (obj%position(0, 2))
               end if

               in = 0
               re = 0.0d0
               !print *, word
               if (IsItNumber(Word) .eqv. .true.) then
                  ! need bugdix
                  !print *, "bugfix",word
                  read (word, *) re
                  in = int(re)
               end if
               call extendArray(mat=obj%rval, extend1stColumn=.true., DefaultValue=re)
               call extendArray(mat=obj%ival, extend1stColumn=.true., DefaultValue=in)
               call extendArray(mat=obj%cval, extend1stColumn=.true., DefaultValue=word)
               call extendArray(mat=obj%position, extend1stColumn=.true.)
               m = size(obj%position, 1)
               obj%position(m, 1) = n
               obj%position(m, 2) = itr
               write (obj%cval(m, 1) (1:200), *) word
               !obj%cval(m,1)(1:200)=word
               !print *, len(obj%cval(m,1)(1:200)),obj%cval(m,1)(1:200),word

            end if
            i = i + j
            k = i
            itr = itr + 1
            if (j == 0) then
               exit
            end if
         end do
      end do
100   continue

      print *, "Imported!"
      m = size(obj%position, 1)
      !print *, size(obj%rval,1),size(obj%ival,1),size(obj%cval,1),size(obj%position,1)
      do i = 1, m
         print *, obj%rval(i, 1), obj%ival(i, 1), obj%cval(i, 1), obj%position(i, 1:2)
      end do

   end subroutine
! ########################################################

! ########################################################
   subroutine exportCSV(obj, path, name, extention)
      class(CSV_), intent(in) :: obj
      character(*), intent(in) :: path, name, extention
      type(IO_) :: f
      integer(int32) :: i, j, n, itr, xsize, ysize, k, l
      call f%open(path, name, extention)
      ysize = maxval(obj%position(:, 1))
      xsize = maxval(obj%position(:, 2))
      do i = 1, ysize
         do j = 1, xsize
            do k = 1, size(obj%position, 1)
               if (obj%position(k, 1) == i .and. obj%position(k, 2) == j) then
                  write (f%fh, '(A)', advance="no") obj%cval(k, 1)
               end if
            end do
            write (f%fh, '(A)', advance="no") ","
         end do
         write (f%fh, '(A)', advance="yes") ","
      end do
      call f%close()

   end subroutine
! ########################################################

end module
