module VertexClass
   use ArrayClass
   use RandomClass
   implicit none

   type :: Vertex_
      real(real64) :: reval = 0.0d0
      real(real64) :: x = 0.0d0
      real(real64) :: y = 0.0d0
      real(real64) :: z = 0.0d0
      integer(int32) :: intval = 0
      integer(int32) :: ID = 0
      integer(int32) :: MyRank = 0
      character(200) :: name = "NoName"
   contains
      procedure, public :: create => createVertex
      procedure, public :: copy => copyVertex
   end type

contains

! ##########################################
   subroutine createVertex(obj, intval, reval, name, Myrank, ID)
      class(Vertex_), intent(inout) :: obj
      integer(int32), optional, intent(in) :: intval, Myrank, ID
      real(real64), optional, intent(in) :: reval
      character(*), optional, intent(in) :: name
      type(Random_) :: random
      integer(int32) :: i
      call random%init()

      obj%x = random%random()
      obj%y = random%random()
      obj%z = random%random()

      if (present(intval)) then
         obj%intval = intval
      end if

      if (present(MyRank)) then
         obj%MyRank = MyRank
      end if
      if (present(ID)) then
         obj%ID = ID
      end if
      if (present(reval)) then
         obj%reval = reval
      end if
      if (present(name)) then
         obj%name = name
      end if

   end subroutine
! ##########################################

! ##########################################
   function copyVertex(obj) result(copy)
      class(Vertex_), intent(inout) :: obj
      type(Vertex_) :: copy

      copy%x = obj%x
      copy%y = obj%y
      copy%z = obj%z

      copy%intval = obj%intval
      copy%MyRank = obj%MyRank
      copy%ID = obj%ID
      copy%reval = obj%reval
      copy%name = obj%name

   end function
! ##########################################

end module
