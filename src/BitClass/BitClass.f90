module BitClass
   !! This module defines bit-wise operations.
   
   use iso_fortran_env
   implicit none

   !> This is a derived type for a bit array.
   type :: Bit_
      logical, allocatable :: bitArray(:)
   contains
      
      procedure, public :: init => initBit
      !! It is a constructor of a bit array

      procedure, public :: int => intBit
      !! It converts bit array into a int value.

      procedure, public :: not => notBit
      !! It converts NOT operation for all bits in a bit array.
   end type

   !> logical NOT operation 
   interface not
      procedure notBitfunc
   end interface

   !> Bit reverse 
   interface reverse
      procedure reverseBitfunc
   end interface

   !> assignment 
   interface assignment(=)
      module procedure assignIntBit
   end interface

contains

! ##########################################################
   subroutine initBit(obj, n)
      class(Bit_), intent(inout) :: obj
      integer(int32), intent(in) :: n

      if (allocated(obj%bitArray)) then
         deallocate (obj%bitArray)
      end if
      allocate (obj%bitArray(n))
      obj%bitArray(:) = .false.

   end subroutine
! ##########################################################

! ##########################################################
   subroutine notBit(obj)
      class(Bit_), intent(inout) :: obj

      obj%bitArray(:) = .not. (obj%bitArray(:))

   end subroutine
! ##########################################################

! ##########################################################
   function intBit(obj) result(ret)
      class(Bit_), intent(inout) :: obj
      integer(int32) :: ret, i

      if (.not. allocated(obj%bitArray)) then
         ret = 0
         return
      end if

      ret = 0
      do i = 1, size(obj%bitArray)
         if (obj%bitArray(i)) then
            ret = ret + 2**(i - 1)
         end if
      end do

   end function
! ##########################################################

! ##########################################################
   function notBitFunc(obj) result(ret)
      type(Bit_), intent(in) :: obj
      type(Bit_) :: ret

      ret = obj
      call ret%not()

   end function
! ##########################################################

! ##########################################################
   function reverseBitFunc(obj) result(ret)
      type(Bit_), intent(in) :: obj
      type(Bit_) :: ret
      integer(int32) :: i, j

      allocate (ret%bitArray(sizE(obj%bitArray)))
      j = 0
      do i = sizE(obj%bitArray), 1, -1
         j = j + 1
         ret%bitArray(j) = obj%bitArray(i)
      end do

   end function
! ##########################################################

! ##########################################################
   subroutine assignIntBit(x, y)
      type(Bit_), intent(inout) :: x
      integer(int32), intent(in)  :: y
      integer(int32) :: i, n, m, order

      m = y
      order = 1
      do
         if (m < 2) then

            if (mod(m, 2) == 1) then
               x%bitArray(order) = .true.
            else
               x%bitArray(order) = .false.
            end if
            exit
         end if

         if (size(x%bitArray) < order) then
            print *, "BitClass >> = ERROR :: exceed bit limit:", size(x%bitArray), " You request : ", order
            stop
         end if

         if (mod(m, 2) == 1) then
            x%bitArray(order) = .true.
         else
            x%bitArray(order) = .false.
         end if

         m = m - mod(m, 2)
         m = m/2
         order = order + 1

      end do

   end subroutine
! ##########################################################

end module
