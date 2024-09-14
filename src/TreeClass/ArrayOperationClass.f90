module ArrayOperationClass
   use MathClass
   implicit none

   interface MergeArray
      module procedure MergeArrayInt, MergeArrayReal
   end interface MergeArray

   interface CopyArray
      module procedure CopyArrayInt, CopyArrayReal, CopyArrayIntVec, CopyArrayRealVec
   end interface CopyArray

   interface TrimArray
      module procedure TrimArrayInt, TrimArrayReal
   end interface TrimArray

   interface ImportArray
      module procedure ImportArrayInt, ImportArrayReal
   end interface ImportArray

   interface ExportArray
      module procedure ExportArrayInt, ExportArrayReal
   end interface ExportArray

   interface ExportArraySize
      module procedure ExportArraySizeInt, ExportArraySizeReal
   end interface ExportArraySize

   interface InOrOut
      module procedure InOrOutReal, InOrOutInt
   end interface

   interface ShowArray
      module procedure ShowArrayInt, ShowArrayReal
   end interface ShowArray

   interface ShowArraySize
      module procedure ShowArraySizeInt, ShowArraySizeReal
      module procedure ShowArraySizeIntvec, ShowArraySizeRealvec
      module procedure ShowArraySizeIntThree, ShowArraySizeRealThree
   end interface ShowArraySize

   interface ExtendArray
      module procedure  :: ExtendArrayReal, ExtendArrayInt
   end interface ExtendArray

   interface insertArray
      module procedure :: insertArrayInt, insertArrayReal
   end interface insertArray

   interface removeArray
      module procedure :: removeArrayReal, removeArrayInt
   end interface removeArray
contains

! ############## Elementary Entities ##############

!=====================================
   subroutine MergeArrayInt(a, b, c)
      integer, intent(in)::a(:, :)
      integer, intent(in)::b(:, :)
      integer, allocatable, intent(out)::c(:, :)
      integer i, j, an, am, bn, bm

      if (allocated(c)) deallocate (c)
      an = size(a, 1)
      am = size(a, 2)

      bn = size(b, 1)
      bm = size(b, 2)

      if (am /= bm) then
         print *, "ERROR :: MergeArray, size(a,2)/= size(b,2)"
         return
      end if
      allocate (c(an + bn, am))
      do i = 1, an
         c(i, :) = a(i, :)
      end do
      do i = 1, bn
         c(i + an, :) = b(i, :)
      end do

   end subroutine
!=====================================

!=====================================
   subroutine MergeArrayReal(a, b, c)
      real(8), intent(in)::a(:, :)
      real(8), intent(in)::b(:, :)
      real(8), allocatable, intent(out)::c(:, :)
      integer i, j, an, am, bn, bm
      if (allocated(c)) deallocate (c)
      an = size(a, 1)
      am = size(a, 2)

      bn = size(b, 1)
      bm = size(b, 2)

      if (am /= bm) then
         print *, "ERROR :: MergeArray, size(a,2)/= size(b,2)"
         return
      end if
      allocate (c(an + bn, am))
      do i = 1, an
         c(i, :) = a(i, :)
      end do
      do i = 1, bn
         c(i + an, :) = b(i, :)
      end do

   end subroutine
!=====================================

!=====================================
   subroutine CopyArrayInt(a, ac)
      integer, allocatable, intent(inout)::a(:, :)
      integer, allocatable, intent(inout)::ac(:, :)
      integer i, j, n, m

      if (.not. allocated(a)) then
         if (allocated(ac)) then
            deallocate (ac)
            return
         end if
      end if
      n = size(a, 1)
      m = size(a, 2)
      if (allocated(ac)) deallocate (ac)

      allocate (ac(n, m))
      ac(:, :) = a(:, :)

   end subroutine
!=====================================

!=====================================
   subroutine CopyArrayReal(a, ac)
      real(8), allocatable, intent(inout)::a(:, :)
      real(8), allocatable, intent(inout)::ac(:, :)
      integer i, j, n, m

      if (.not. allocated(a)) then
         if (allocated(ac)) then
            deallocate (ac)
            return
         end if
      end if
      n = size(a, 1)
      m = size(a, 2)

      if (allocated(ac)) deallocate (ac)
      allocate (ac(n, m))
      ac(:, :) = a(:, :)

   end subroutine
!=====================================

!=====================================
   subroutine CopyArrayIntVec(a, ac)
      integer, allocatable, intent(inout)::a(:)
      integer, allocatable, intent(inout)::ac(:)
      integer i, j, n, m

      if (.not. allocated(a)) then
         if (allocated(ac)) then
            deallocate (ac)

         end if
         return
      end if
      n = size(a, 1)
      if (allocated(ac)) deallocate (ac)

      allocate (ac(n))
      ac(:) = a(:)

   end subroutine
!=====================================

!=====================================
   subroutine CopyArrayRealVec(a, ac)
      real(8), allocatable, intent(inout)::a(:)
      real(8), allocatable, intent(inout)::ac(:)
      integer i, j, n, m

      if (.not. allocated(a)) then
         if (allocated(ac)) then
            deallocate (ac)

         end if
         return
      end if
      n = size(a, 1)
      if (allocated(ac)) deallocate (ac)

      allocate (ac(n))
      ac(:) = a(:)

   end subroutine
!=====================================

!=====================================
   subroutine TrimArrayInt(a, k)
      integer, allocatable, intent(inout)::a(:, :)
      integer, intent(in)::k
      integer, allocatable::ac(:, :)
      integer :: i, j, n, m

      n = size(a, 1)
      m = size(a, 2)
      allocate (ac(k, m))

      do i = 1, k
         ac(i, :) = a(i, :)
      end do
      deallocate (a)
      allocate (a(k, m))
      a(:, :) = ac(:, :)
      return

   end subroutine
!=====================================

!=====================================
   subroutine TrimArrayReal(a, k)
      real(8), allocatable, intent(inout)::a(:, :)
      integer, intent(in)::k
      real(8), allocatable::ac(:, :)
      integer :: i, j, n, m

      n = size(a, 1)
      m = size(a, 2)
      allocate (ac(k, m))

      do i = 1, k
         ac(i, :) = a(i, :)
      end do

      deallocate (a)
      allocate (a(k, m))
      a(:, :) = ac(:, :)
      return

   end subroutine
!=====================================

!##################################################
   subroutine ImportArrayInt(Mat, OptionalFileHandle, OptionalSizeX, OptionalSizeY)
      integer, allocatable, intent(inout)::Mat(:, :)
      integer, optional, intent(in)::OptionalFileHandle, OptionalSizeX, OptionalSizeY

      integer i, j, n, m, fh

      if (present(OptionalSizeX)) then
         n = OptionalSizeX
      elseif (allocated(Mat)) then
         n = size(Mat, 1)
      else
         n = 1
         print *, "Caution :: ArrayOperationClass/ImportArray >> No size_X is set"
      end if

      if (present(OptionalSizeY)) then
         m = OptionalSizeY
      elseif (allocated(Mat)) then
         m = size(Mat, 2)
      else
         m = 1
         print *, "Caution :: ArrayOperationClass/ImportArray >> No size_Y is set"
      end if

      if (present(OptionalFileHandle)) then
         fh = OptionalFileHandle
      else
         fh = 10
      end if

      if (.not. allocated(Mat)) then
         allocate (Mat(n, m))
      end if

      if (size(Mat, 1) /= n .or. size(Mat, 2) /= m) then
         deallocate (Mat)
         allocate (Mat(n, m))
      end if

      do i = 1, size(Mat, 1)
         read (fh, *) Mat(i, :)
      end do

      !include "./ImportArray.f90"

   end subroutine ImportArrayInt
!##################################################

!##################################################
   subroutine ImportArrayReal(Mat, OptionalFileHandle, OptionalSizeX, OptionalSizeY)
      real(8), allocatable, intent(inout)::Mat(:, :)
      integer, optional, intent(in)::OptionalFileHandle, OptionalSizeX, OptionalSizeY

      !include "./ImportArray.f90"

      integer i, j, n, m, fh

      if (present(OptionalSizeX)) then
         n = OptionalSizeX
      elseif (allocated(Mat)) then
         n = size(Mat, 1)
      else
         n = 1
         print *, "Caution :: ArrayOperationClass/ImportArray >> No size_X is set"
      end if

      if (present(OptionalSizeY)) then
         m = OptionalSizeY
      elseif (allocated(Mat)) then
         m = size(Mat, 2)
      else
         m = 1
         print *, "Caution :: ArrayOperationClass/ImportArray >> No size_Y is set"
      end if

      if (present(OptionalFileHandle)) then
         fh = OptionalFileHandle
      else
         fh = 10
      end if

      if (.not. allocated(Mat)) then
         allocate (Mat(n, m))
      end if

      if (size(Mat, 1) /= n .or. size(Mat, 2) /= m) then
         deallocate (Mat)
         allocate (Mat(n, m))
      end if

      do i = 1, size(Mat, 1)
         read (fh, *) Mat(i, :)
      end do

   end subroutine ImportArrayReal
!##################################################

!##################################################
   subroutine ExportArraySizeInt(Mat, RankNum, OptionalFileHandle)
      integer, intent(in)::Mat(:, :)
      integer, optional, intent(in)::OptionalFileHandle
      integer, intent(in)::RankNum

      !#include "./ExportArraySize.f90"
      integer :: fh
      if (present(OptionalFileHandle)) then
         fh = OptionalFileHandle
      end if

      write (fh, *) size(Mat, RankNum)

   end subroutine ExportArraySizeInt
!##################################################

!##################################################
   subroutine ExportArraySizeReal(Mat, RankNum, OptionalFileHandle)
      real(8), intent(in)::Mat(:, :)
      integer, optional, intent(in)::OptionalFileHandle
      integer, intent(in)::RankNum

      !#include "./ExportArraySize.f90"
      integer :: fh
      if (present(OptionalFileHandle)) then
         fh = OptionalFileHandle
      end if

      write (fh, *) size(Mat, RankNum)

   end subroutine ExportArraySizeReal
!##################################################

!##################################################
   subroutine ExportArrayInt(Mat, OptionalFileHandle)
      integer, intent(in)::Mat(:, :)
      integer, optional, intent(in)::OptionalFileHandle

      !#include "./ExportArray.f90"
      integer :: fh, i

      if (present(OptionalFileHandle)) then
         fh = OptionalFileHandle
      else
         fh = 10
      end if

      do i = 1, size(Mat, 1)
         write (fh, *) Mat(i, :)
      end do

   end subroutine ExportArrayInt
!##################################################

!##################################################
   subroutine ExportArrayReal(Mat, OptionalFileHandle)
      real(8), intent(in)::Mat(:, :)
      integer, optional, intent(in)::OptionalFileHandle

      !#include "./ExportArray.f90"
      integer :: fh, i

      if (present(OptionalFileHandle)) then
         fh = OptionalFileHandle
      else
         fh = 10
      end if

      do i = 1, size(Mat, 1)
         write (fh, *) Mat(i, :)
      end do

   end subroutine ExportArrayReal
!##################################################

!##################################################
   subroutine ShowArrayInt(Mat, IndexArray, FileHandle, Name)
      integer, intent(in)::Mat(:, :)
      integer, optional, intent(in) :: IndexArray(:, :)
      integer, optional, intent(in)::FileHandle
      character(*), optional, intent(in)::Name

      !#include "./ExportArray.f90"
      integer :: fh, i, j, k

      if (present(FileHandle)) then
         fh = FileHandle
      else
         fh = 10
      end if

      if (present(Name)) then
         open (fh, file=Name)
      end if

      if (present(IndexArray)) then

         do i = 1, size(IndexArray, 1)
            do j = 1, size(IndexArray, 2)
               k = IndexArray(i, j)
               if (k <= 0) then
                  cycle
               end if

               if (present(FileHandle) .or. present(Name)) then
                  write (fh, *) Mat(k, :)
               else
                  print *, Mat(k, :)
               end if
            end do
         end do
      else

         do j = 1, size(Mat, 1)

            if (present(FileHandle) .or. present(Name)) then
               write (fh, *) Mat(j, :)
            else
               print *, Mat(j, :)
            end if

         end do

      end if

      if (present(FileHandle) .or. present(Name)) then
         flush (fh)
      end if

      if (present(Name)) then
         close (fh)
      end if

   end subroutine
!##################################################

!##################################################
   subroutine ShowArrayReal(Mat, IndexArray, FileHandle, Name)
      real(8), intent(in)::Mat(:, :)
      integer, optional, intent(in) :: IndexArray(:, :)
      integer, optional, intent(in)::FileHandle
      character(*), optional, intent(in)::Name

      !#include "./ExportArray.f90"
      integer :: fh, i, j, k

      if (present(FileHandle)) then
         fh = FileHandle
      else
         fh = 10
      end if

      if (present(Name)) then
         open (fh, file=Name)
      end if

      if (present(IndexArray)) then

         do i = 1, size(IndexArray, 1)
            do j = 1, size(IndexArray, 2)
               k = IndexArray(i, j)
               if (k <= 0) then
                  cycle
               end if

               if (present(FileHandle) .or. present(Name)) then
                  write (fh, *) Mat(k, :)
               else
                  print *, Mat(k, :)
               end if
            end do
         end do
      else

         do j = 1, size(Mat, 1)

            if (present(FileHandle) .or. present(Name)) then
               write (fh, *) Mat(j, :)
            else
               print *, Mat(j, :)
            end if

         end do

      end if

      if (present(FileHandle) .or. present(Name)) then
         flush (fh)
      end if

      if (present(Name)) then
         close (fh)
      end if

   end subroutine
!##################################################

!##################################################
   subroutine ShowArraySizeInt(Mat, OptionalFileHandle, Name)
      integer, allocatable, intent(in)::Mat(:, :)
      integer, optional, intent(in)::OptionalFileHandle
      character(*), optional, intent(in)::Name

      !#include "./ExportArray.f90"
      integer :: fh, i

      if (present(OptionalFileHandle)) then
         fh = OptionalFileHandle
      else
         fh = 10
      end if

      if (present(Name)) then
         open (fh, file=Name)
      end if

      if (.not. allocated(Mat)) then
         print *, "Not allocated!"
         if (present(OptionalFileHandle)) then
            write (fh, *) "Not allocated!"
         end if
      end if
      print *, size(Mat, 1), size(Mat, 2)
      if (present(OptionalFileHandle) .or. present(Name)) then
         write (fh, *) size(Mat, 1), size(Mat, 2)
      end if

      if (present(Name)) then
         close (fh)
      end if

   end subroutine
!##################################################

!##################################################
   subroutine ShowArraySizeReal(Mat, OptionalFileHandle, Name)
      real(8), allocatable, intent(in)::Mat(:, :)
      integer, optional, intent(in)::OptionalFileHandle
      character(*), optional, intent(in)::Name

      !#include "./ExportArray.f90"
      integer :: fh, i

      if (present(OptionalFileHandle)) then
         fh = OptionalFileHandle
      else
         fh = 10
      end if

      if (present(Name)) then
         open (fh, file=Name)
      end if

      if (.not. allocated(Mat)) then
         print *, "Not allocated!"
         if (present(OptionalFileHandle)) then
            write (fh, *) "Not allocated!"
         end if
      end if
      print *, size(Mat, 1), size(Mat, 2)
      if (present(OptionalFileHandle) .or. present(Name)) then
         write (fh, *) size(Mat, 1), size(Mat, 2)
      end if

      if (present(Name)) then
         close (fh)
      end if

   end subroutine
!##################################################

!##################################################
   subroutine ShowArraySizeIntvec(Mat, OptionalFileHandle, Name)
      integer, allocatable, intent(in)::Mat(:)
      integer, optional, intent(in)::OptionalFileHandle
      character(*), optional, intent(in)::Name

      !#include "./ExportArray.f90"
      integer :: fh, i

      if (present(OptionalFileHandle)) then
         fh = OptionalFileHandle
      else
         fh = 10
      end if

      if (present(Name)) then
         open (fh, file=Name)
      end if

      if (.not. allocated(Mat)) then
         print *, "Not allocated!"
         if (present(OptionalFileHandle) .or. present(Name)) then
            write (fh, *) "Not allocated!"
         end if
      end if
      print *, size(Mat, 1)
      if (present(OptionalFileHandle) .or. present(Name)) then
         write (fh, *) size(Mat, 1)
      end if

      if (present(Name)) then
         close (fh)
      end if

   end subroutine
!##################################################

!##################################################
   subroutine ShowArraySizeRealvec(Mat, OptionalFileHandle, Name)
      real(8), allocatable, intent(in)::Mat(:)
      integer, optional, intent(in)::OptionalFileHandle
      character(*), optional, intent(in)::Name

      !#include "./ExportArray.f90"
      integer :: fh, i

      if (present(OptionalFileHandle)) then
         fh = OptionalFileHandle
      else
         fh = 10
      end if

      if (present(Name)) then
         open (fh, file=Name)
      end if

      if (.not. allocated(Mat)) then
         print *, "Not allocated!"
         if (present(OptionalFileHandle) .or. present(Name)) then
            write (fh, *) "Not allocated!"
         end if
      end if
      print *, size(Mat, 1)
      if (present(OptionalFileHandle) .or. present(Name)) then
         write (fh, *) size(Mat, 1)
      end if

      if (present(Name)) then
         close (fh)
      end if

   end subroutine
!##################################################

!##################################################
   subroutine ShowArraySizeIntThree(Mat, OptionalFileHandle, Name)
      integer, allocatable, intent(in)::Mat(:, :, :)
      integer, optional, intent(in)::OptionalFileHandle
      character(*), optional, intent(in)::Name

      !#include "./ExportArray.f90"
      integer :: fh, i

      if (present(OptionalFileHandle)) then
         fh = OptionalFileHandle
      else
         fh = 10
      end if

      if (present(Name)) then
         open (fh, file=Name)
      end if

      if (.not. allocated(Mat)) then
         print *, "Not allocated!"
         if (present(OptionalFileHandle) .or. present(Name)) then
            write (fh, *) "Not allocated!"
         end if
      end if
      print *, size(Mat, 1), size(Mat, 2), size(Mat, 3)
      if (present(OptionalFileHandle) .or. present(Name)) then
         write (fh, *) size(Mat, 1), size(Mat, 2), size(Mat, 3)
      end if

      if (present(Name)) then
         close (fh)
      end if
   end subroutine
!##################################################

!##################################################
   subroutine ShowArraySizeRealThree(Mat, OptionalFileHandle, Name)
      real(8), allocatable, intent(in)::Mat(:, :, :, :)
      integer, optional, intent(in)::OptionalFileHandle
      character(*), optional, intent(in)::Name

      !#include "./ExportArray.f90"
      integer :: fh, i

      if (present(OptionalFileHandle)) then
         fh = OptionalFileHandle
      else
         fh = 10
      end if

      if (present(Name)) then
         open (fh, file=Name)
      end if

      if (.not. allocated(Mat)) then
         print *, "Not allocated!"
         if (present(OptionalFileHandle) .or. present(Name)) then
            write (fh, *) "Not allocated!"
         end if
      end if
      print *, size(Mat, 1), size(Mat, 2), size(Mat, 3)
      if (present(OptionalFileHandle) .or. present(Name)) then
         write (fh, *) size(Mat, 1), size(Mat, 2), size(Mat, 3)
      end if

      if (present(Name)) then
         close (fh)
      end if

   end subroutine
!##################################################

!##################################################
   function InOrOutReal(x, xmax, xmin, DimNum) result(Inside)
      real(8), intent(in)::x(:)
      real(8), intent(in)::xmax(:), xmin(:)
      integer, optional, intent(in)::DimNum
      integer :: dim_num
      logical ::Inside
      integer :: i, j, n, cout

      cout = 0
      if (present(DimNum)) then
         dim_num = DimNum
      else
         dim_num = size(x)
      end if

      do i = 1, dim_num
         if (xmin(i) <= x(i) .and. x(i) <= xmax(i)) then
            cout = cout + 1
         else
            cycle
         end if
      end do

      if (cout == dim_num) then
         Inside = .true.
      else
         Inside = .false.
      end if

   end function

!##################################################

!##################################################
   function InOrOutInt(x, xmax, xmin, DimNum) result(Inside)
      integer, intent(in)::x(:)
      integer, intent(in)::xmax(:), xmin(:)
      integer, optional, intent(in)::DimNum
      integer :: dim_num
      logical ::Inside
      integer :: i, j, n, cout

      cout = 0
      if (present(DimNum)) then
         dim_num = DimNum
      else
         dim_num = size(x)
      end if

      do i = 1, dim_num
         if (xmin(i) <= x(i) .and. x(i) <= xmax(i)) then
            cout = cout + 1
         else
            cycle
         end if
      end do

      if (cout == dim_num) then
         Inside = .true.
      else
         Inside = .false.
      end if

   end function

!##################################################

!##################################################
   subroutine ExtendArrayReal(mat, extend1stColumn, extend2ndColumn, DefaultValue)
      real(8), allocatable, intent(inout)::mat(:, :)
      real(8), allocatable :: buffer(:, :)
      logical, optional, intent(in) :: extend1stColumn, extend2ndColumn
      real(8), optional, intent(in) :: DefaultValue
      real(8) :: val
      integer :: n, m

      if (present(DefaultValue)) then
         val = DefaultValue
      else
         val = 0.0d0
      end if

      n = size(mat, 1)
      m = size(mat, 2)
      if (present(extend1stColumn)) then
         if (extend1stColumn .eqv. .true.) then
            allocate (buffer(n + 1, m))
            buffer(:, :) = val
            buffer(1:n, 1:m) = mat(1:n, 1:m)
            deallocate (mat)
            call copyArray(buffer, mat)
            deallocate (buffer)
         end if
      end if

      n = size(mat, 1)
      m = size(mat, 2)
      if (present(extend2ndColumn)) then
         if (extend2ndColumn .eqv. .true.) then
            allocate (buffer(n, m + 1))
            buffer(:, :) = val
            buffer(1:n, 1:m) = mat(1:n, 1:m)
            deallocate (mat)
            call copyArray(buffer, mat)
            deallocate (buffer)
         end if
      end if

   end subroutine
!##################################################

!##################################################
   subroutine ExtendArrayInt(mat, extend1stColumn, extend2ndColumn, DefaultValue)
      integer, allocatable, intent(inout)::mat(:, :)
      integer, allocatable :: buffer(:, :)
      logical, optional, intent(in) :: extend1stColumn, extend2ndColumn
      integer, optional, intent(in) :: DefaultValue
      integer :: val
      integer :: i, j, k, n, m

      if (present(DefaultValue)) then
         val = DefaultValue
      else
         val = 0
      end if

      n = size(mat, 1)
      m = size(mat, 2)
      if (present(extend1stColumn)) then
         if (extend1stColumn .eqv. .true.) then
            allocate (buffer(n + 1, m))
            buffer(:, :) = val
            buffer(1:n, 1:m) = mat(1:n, 1:m)
            deallocate (mat)
            call copyArray(buffer, mat)
            deallocate (buffer)
         end if
      end if

      n = size(mat, 1)
      m = size(mat, 2)
      if (present(extend2ndColumn)) then
         if (extend2ndColumn .eqv. .true.) then
            allocate (buffer(n, m + 1))
            buffer(:, :) = val
            buffer(1:n, 1:m) = mat(1:n, 1:m)
            deallocate (mat)
            call copyArray(buffer, mat)
            deallocate (buffer)
         end if
      end if

   end subroutine
!##################################################

!##################################################
   subroutine insertArrayInt(mat, insert1stColumn, insert2ndColumn, DefaultValue, NextOf)
      integer, allocatable, intent(inout)::mat(:, :)
      integer, allocatable :: buffer(:, :)
      logical, optional, intent(in) :: insert1stColumn, insert2ndColumn
      integer, optional, intent(in) :: DefaultValue, NextOf
      integer :: val
      integer :: i, nof

      call extendArray(mat, insert1stColumn, insert2ndColumn, DefaultValue)

      if (present(DefaultValue)) then
         val = DefaultValue
      else
         val = 0
      end if

      if (present(NextOf)) then
         nof = NextOf
      else
         if (present(insert1stColumn)) then
            if (insert1stColumn .eqv. .true.) then
               nof = size(mat, 1) - 1
            end if
         end if

         if (present(insert2ndColumn)) then
            if (insert2ndColumn .eqv. .true.) then
               nof = size(mat, 2) - 1
            end if
         end if

      end if

      if (present(insert1stColumn)) then
         if (insert1stColumn .eqv. .true.) then
            do i = size(mat, 1) - 1, nof, -1
               mat(i + 1, :) = mat(i, :)
            end do
            mat(nof + 1, :) = val
         end if
      end if
      if (present(insert2ndColumn)) then
         if (insert2ndColumn .eqv. .true.) then
            do i = size(mat, 1) - 1, nof, -1
               mat(:, i + 1) = mat(:, i)
            end do
            mat(:, nof + 1) = val
         end if
      end if
   end subroutine
!##################################################

!##################################################
   subroutine insertArrayReal(mat, insert1stColumn, insert2ndColumn, DefaultValue, NextOf)
      real(8), allocatable, intent(inout)::mat(:, :)
      real(8), allocatable :: buffer(:, :)
      logical, optional, intent(in) :: insert1stColumn, insert2ndColumn

      integer, optional, intent(in) :: NextOf
      real(8), optional, intent(in) :: DefaultValue
      real(8) :: val
      integer :: i, nof

      call extendArray(mat, insert1stColumn, insert2ndColumn, DefaultValue)

      if (present(DefaultValue)) then
         val = DefaultValue
      else
         val = 0
      end if

      if (present(NextOf)) then
         nof = NextOf
      else
         if (present(insert1stColumn)) then
            if (insert1stColumn .eqv. .true.) then
               nof = size(mat, 1) - 1
            end if
         end if

         if (present(insert2ndColumn)) then
            if (insert2ndColumn .eqv. .true.) then
               nof = size(mat, 2) - 1
            end if
         end if

      end if

      if (present(insert1stColumn)) then
         if (insert1stColumn .eqv. .true.) then
            do i = size(mat, 1) - 1, nof, -1
               mat(i + 1, :) = mat(i, :)
            end do
            mat(nof + 1, :) = val
         end if
      end if
      if (present(insert2ndColumn)) then
         if (insert2ndColumn .eqv. .true.) then
            do i = size(mat, 1) - 1, nof, -1
               mat(:, i + 1) = mat(:, i)
            end do
            mat(:, nof + 1) = val
         end if
      end if
   end subroutine
!##################################################

!##################################################
   subroutine removeArrayInt(mat, remove1stColumn, remove2ndColumn, NextOf)
      integer, allocatable, intent(inout)::mat(:, :)
      integer, allocatable :: buffer(:, :)
      logical, optional, intent(in) :: remove1stColumn, remove2ndColumn

      integer, optional, intent(in) :: NextOf

      integer :: i, nof

      if (present(remove1stColumn)) then
         if (remove1stColumn .eqv. .true.) then
            nof = size(mat, 1)
         end if
      end if

      if (present(remove2ndColumn)) then
         if (remove2ndColumn .eqv. .true.) then
            nof = size(mat, 2)
         end if
      end if

      if (present(NextOf)) then
         if (NextOf >= nof) then
            return
         end if
      end if
      call copyArray(mat, buffer)

      if (present(NextOf)) then
         nof = NextOf
      else
         if (present(remove1stColumn)) then
            if (remove1stColumn .eqv. .true.) then
               nof = size(mat, 1) - 1
            end if
         end if

         if (present(remove2ndColumn)) then
            if (remove2ndColumn .eqv. .true.) then
               nof = size(mat, 2) - 1
            end if
         end if
      end if

      deallocate (mat)

      if (present(remove1stColumn)) then
         if (remove1stColumn .eqv. .true.) then
            if (size(buffer, 1) - 1 == 0) then
               print *, "Array is deleted"
               return
            end if
            allocate (mat(size(buffer, 1) - 1, size(buffer, 2)))
            mat(:, :) = 0.0d0
            do i = 1, nof
               mat(i, :) = buffer(i, :)
            end do

            do i = nof + 1, size(buffer, 1) - 1
               mat(i, :) = buffer(i + 1, :)
            end do

         end if
      end if
      if (present(remove2ndColumn)) then
         if (remove2ndColumn .eqv. .true.) then

            if (size(buffer, 2) - 1 == 0) then
               print *, "Array is deleted"
               return
            end if
            allocate (mat(size(buffer, 1), size(buffer, 2) - 1))
            mat(:, :) = 0.0d0
            do i = 1, nof
               mat(:, i) = buffer(:, i)
            end do

            do i = nof + 1, size(buffer, 2) - 1
               mat(:, i) = buffer(:, i + 1)
            end do
         end if
      end if

   end subroutine
!##################################################

!##################################################
   subroutine removeArrayReal(mat, remove1stColumn, remove2ndColumn, NextOf)
      real(8), allocatable, intent(inout)::mat(:, :)
      real(8), allocatable :: buffer(:, :)
      logical, optional, intent(in) :: remove1stColumn, remove2ndColumn

      integer, optional, intent(in) :: NextOf

      integer :: i, nof

      if (present(remove1stColumn)) then
         if (remove1stColumn .eqv. .true.) then
            nof = size(mat, 1)
         end if
      end if

      if (present(remove2ndColumn)) then
         if (remove2ndColumn .eqv. .true.) then
            nof = size(mat, 2)
         end if
      end if

      if (present(NextOf)) then
         if (NextOf >= nof) then
            return
         end if
      end if

      call copyArray(mat, buffer)

      if (present(NextOf)) then
         nof = NextOf
      else
         if (present(remove1stColumn)) then
            if (remove1stColumn .eqv. .true.) then
               nof = size(mat, 1) - 1
            end if
         end if

         if (present(remove2ndColumn)) then
            if (remove2ndColumn .eqv. .true.) then
               nof = size(mat, 2) - 1
            end if
         end if
      end if

      deallocate (mat)

      if (present(remove1stColumn)) then
         if (remove1stColumn .eqv. .true.) then
            if (size(buffer, 1) - 1 == 0) then
               print *, "Array is deleted"
               return
            end if
            allocate (mat(size(buffer, 1) - 1, size(buffer, 2)))
            mat(:, :) = 0.0d0
            do i = 1, nof
               mat(i, :) = buffer(i, :)
            end do

            do i = nof + 1, size(buffer, 1) - 1
               mat(i, :) = buffer(i + 1, :)
            end do

         end if
      end if
      if (present(remove2ndColumn)) then
         if (remove2ndColumn .eqv. .true.) then

            if (size(buffer, 2) - 1 == 0) then
               print *, "Array is deleted"
               return
            end if
            allocate (mat(size(buffer, 1), size(buffer, 2) - 1))
            mat(:, :) = 0.0d0
            do i = 1, nof
               mat(:, i) = buffer(:, i)
            end do

            do i = nof + 1, size(buffer, 2) - 1
               mat(:, i) = buffer(:, i + 1)
            end do
         end if
      end if

   end subroutine
!##################################################

end module ArrayOperationClass
