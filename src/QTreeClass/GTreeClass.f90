! CAUTION :: UNDER IMPLEMENTATION!
! DO NOT USE!

module QTreeClass
   use iso_fortran_env
   implicit none

   type :: QTreeAddress_
      integer :: address
   end type QTreeAddress_

   type :: QTree_
      type(QTreeAddress_), allocatable :: address(:)
   contains
      procedure, public :: init => initQTree
   end type QTree_
contains

   subroutine initQTree(this, NumDivisions)
      class(QTree_) :: this
      integer(int32), optional :: NumDivisions

      if (allocated(this)) then
         deallocate (this%address)
      end if

      allocate (this%address(NumDivisions))

   end subroutine initQTree

end module
