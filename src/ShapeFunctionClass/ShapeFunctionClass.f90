module ShapeFunctionClass
   use, intrinsic :: iso_fortran_env
   use MathClass
   use ArrayClass
   use IOClass
   implicit none

   type::ShapeFunction_

      real(real64), allocatable::Nmat(:)
      real(real64), allocatable::dNdgzi(:, :)
      real(real64), allocatable::dNdgzidgzi(:, :)
      real(real64), allocatable::gzi(:)
      real(real64), allocatable::GaussPoint(:, :)
      real(real64), allocatable::GaussIntegWei(:)
      real(real64), allocatable::Jmat(:, :) ! d x/d xi
      real(real64), allocatable::JmatInv(:, :)! d xi/d x
      real(real64), allocatable::ElemCoord(:, :)
      real(real64), allocatable::ElemCoord_n(:, :)
      real(real64), allocatable::du(:, :)
      real(real64) :: detJ
      integer(int32) :: NumOfNode
      integer(int32) :: NumOfOrder
      integer(int32) :: NumOfDim = 0
      integer(int32) :: NumOfGp = 0
      integer(int32) :: GpID
      integer(int32) :: ierr
      integer(int32) :: currentGpID
      integer(int32) :: ElementID
      logical :: ReducedIntegration = .false.
      logical :: Empty = .true.
      character*70::ElemType
      character(len=60):: ErrorMsg
   contains
      procedure :: init => initShapeFunction
      procedure :: update => updateShapeFunction
      procedure :: SetType => SetShapeFuncType
      procedure :: GetAll => GetAllShapeFunc
      procedure :: get => GetAllShapeFunc
      procedure :: getOnlyNvec => GetShapeFunction
      procedure :: Deallocate => DeallocateShapeFunction
      procedure :: getType => getShapeFuncType
      procedure :: GetGaussPoint => GetGaussPoint
      procedure :: export => exportShapeFunction
      procedure :: remove => removeShapeFunction
      procedure :: save => saveShapeFunction
      procedure :: open => openShapeFunction

   end type ShapeFunction_

contains

!!  #####################################################
   subroutine openShapeFunction(obj, path, name)
      class(ShapeFunction_), intent(inout)::obj
      character(*), intent(in) :: path
      character(*), optional, intent(in) :: name
      type(IO_) :: f

      if (present(name)) then
         call execute_command_line("mkdir -p "//path//"/"//name)

         call f%open(path//"/"//name//"/", "ShapeFunction", "prop")

         call openArray(f%fh, obj%Nmat)
         call openArray(f%fh, obj%dNdgzi)
         call openArray(f%fh, obj%dNdgzidgzi)
         call openArray(f%fh, obj%gzi)
         call openArray(f%fh, obj%GaussPoint)
         call openArray(f%fh, obj%GaussIntegWei)
         call openArray(f%fh, obj%Jmat)
         call openArray(f%fh, obj%JmatInv)
         call openArray(f%fh, obj%ElemCoord)
         call openArray(f%fh, obj%ElemCoord_n)
         call openArray(f%fh, obj%du)
         read (f%fh, *) obj%detJ
         read (f%fh, *) obj%NumOfNode
         read (f%fh, *) obj%NumOfOrder
         read (f%fh, *) obj%NumOfDim
         read (f%fh, *) obj%NumOfGp
         read (f%fh, *) obj%GpID
         read (f%fh, *) obj%ierr
         read (f%fh, *) obj%currentGpID
         read (f%fh, *) obj%ReducedIntegration
         read (f%fh, *) obj%ElemType
         read (f%fh, *) obj%ErrorMsg

         call f%close()
      else

         call execute_command_line("mkdir -p "//path//"/ShapeFunction")
         call f%open(path//"/", "ShapeFunction/ShapeFunction", "prop")

         call openArray(f%fh, obj%Nmat)
         call openArray(f%fh, obj%dNdgzi)
         call openArray(f%fh, obj%dNdgzidgzi)
         call openArray(f%fh, obj%gzi)
         call openArray(f%fh, obj%GaussPoint)
         call openArray(f%fh, obj%GaussIntegWei)
         call openArray(f%fh, obj%Jmat)
         call openArray(f%fh, obj%JmatInv)
         call openArray(f%fh, obj%ElemCoord)
         call openArray(f%fh, obj%ElemCoord_n)
         call openArray(f%fh, obj%du)
         read (f%fh, *) obj%detJ
         read (f%fh, *) obj%NumOfNode
         read (f%fh, *) obj%NumOfOrder
         read (f%fh, *) obj%NumOfDim
         read (f%fh, *) obj%NumOfGp
         read (f%fh, *) obj%GpID
         read (f%fh, *) obj%ierr
         read (f%fh, *) obj%currentGpID
         read (f%fh, *) obj%ReducedIntegration
         read (f%fh, *) obj%ElemType
         read (f%fh, *) obj%ErrorMsg

         call f%close()
      end if

   end subroutine
!!  #####################################################

!!  #####################################################
   subroutine saveShapeFunction(obj, path, name)
      class(ShapeFunction_), intent(inout)::obj
      character(*), intent(in) :: path
      character(*), optional, intent(in) :: name
      type(IO_) :: f

      if (present(name)) then
         call execute_command_line("mkdir -p "//path//"/"//name)

         call f%open(path//"/"//name//"/", "ShapeFunction", "prop")

         call writeArray(f%fh, obj%Nmat)
         call writeArray(f%fh, obj%dNdgzi)
         call writeArray(f%fh, obj%dNdgzidgzi)
         call writeArray(f%fh, obj%gzi)
         call writeArray(f%fh, obj%GaussPoint)
         call writeArray(f%fh, obj%GaussIntegWei)
         call writeArray(f%fh, obj%Jmat)
         call writeArray(f%fh, obj%JmatInv)
         call writeArray(f%fh, obj%ElemCoord)
         call writeArray(f%fh, obj%ElemCoord_n)
         call writeArray(f%fh, obj%du)
         write (f%fh, *) obj%detJ
         write (f%fh, *) obj%NumOfNode
         write (f%fh, *) obj%NumOfOrder
         write (f%fh, *) obj%NumOfDim
         write (f%fh, *) obj%NumOfGp
         write (f%fh, *) obj%GpID
         write (f%fh, *) obj%ierr
         write (f%fh, *) obj%currentGpID
         write (f%fh, *) obj%ReducedIntegration
         write (f%fh, *) obj%ElemType
         write (f%fh, *) obj%ErrorMsg

         call f%close()
      else

         call execute_command_line("mkdir -p "//path//"/ShapeFunction")
         call f%open(path//"/", "ShapeFunction/ShapeFunction", "prop")

         call writeArray(f%fh, obj%Nmat)
         call writeArray(f%fh, obj%dNdgzi)
         call writeArray(f%fh, obj%dNdgzidgzi)
         call writeArray(f%fh, obj%gzi)
         call writeArray(f%fh, obj%GaussPoint)
         call writeArray(f%fh, obj%GaussIntegWei)
         call writeArray(f%fh, obj%Jmat)
         call writeArray(f%fh, obj%JmatInv)
         call writeArray(f%fh, obj%ElemCoord)
         call writeArray(f%fh, obj%ElemCoord_n)
         call writeArray(f%fh, obj%du)
         write (f%fh, *) obj%detJ
         write (f%fh, *) obj%NumOfNode
         write (f%fh, *) obj%NumOfOrder
         write (f%fh, *) obj%NumOfDim
         write (f%fh, *) obj%NumOfGp
         write (f%fh, *) obj%GpID
         write (f%fh, *) obj%ierr
         write (f%fh, *) obj%currentGpID
         write (f%fh, *) obj%ReducedIntegration
         write (f%fh, *) obj%ElemType
         write (f%fh, *) obj%ErrorMsg

         call f%close()
      end if

   end subroutine
!!  #####################################################

   subroutine removeShapeFunction(obj)
      class(ShapeFunction_), intent(inout)::obj

      if (allocated(obj%Nmat)) then
         deallocate (obj%Nmat)
      end if
      if (allocated(obj%dNdgzi)) then
         deallocate (obj%dNdgzi)
      end if
      if (allocated(obj%dNdgzidgzi)) then
         deallocate (obj%dNdgzidgzi)
      end if
      if (allocated(obj%gzi)) then
         deallocate (obj%gzi)
      end if
      if (allocated(obj%GaussPoint)) then
         deallocate (obj%GaussPoint)
      end if
      if (allocated(obj%GaussIntegWei)) then
         deallocate (obj%GaussIntegWei)
      end if
      if (allocated(obj%Jmat)) then
         deallocate (obj%Jmat)
      end if
      if (allocated(obj%ElemCoord)) then
         deallocate (obj%ElemCoord)
      end if
      if (allocated(obj%ElemCoord_n)) then
         deallocate (obj%ElemCoord_n)
      end if
      if (allocated(obj%du)) then
         deallocate (obj%du)
      end if
      obj%detJ = 0.0d0
      obj%NumOfNode = 0
      obj%NumOfOrder = 0
      obj%NumOfDim = 0
      obj%NumOfGp = 0
      obj%GpID = 0
      obj%ierr = 0
      obj%currentGpID = 0
      obj%ReducedIntegration = .false.
      obj%ElemType = " "
      obj%ErrorMsg = " "

   end subroutine

!!  ##################################################
   subroutine initShapeFunction(obj, ElemType)
      class(ShapeFunction_), intent(inout) :: obj
      character(*), intent(in), optional :: ElemType

      obj%ElemType = ElemType

      call obj%SetType()

   end subroutine
!!  ##################################################

!!  ##################################################
   subroutine updateShapeFunction(obj, ElemType, NodCoord, ElemNod, ElemID, GpID)
      class(ShapeFunction_), intent(inout) :: obj
      character(*), optional, intent(in) :: ElemType
      integer(int32), intent(in) ::ElemNod(:, :), ElemID, GpID
      real(real64), intent(in) ::NodCoord(:, :)
      integer(int32) :: i, j

      if (present(ElemType)) then
         call obj%init(ElemType)
      end if
      call obj%GetAll(elem_id=i, nod_coord=NodCoord, elem_nod=ElemNod, OptionalGpID=j)

   end subroutine
!!  ##################################################

!!  ##################################################
   subroutine SetShapeFuncType(obj, NumOfDim, NumOfNodePerElem, ReducedIntegration, NumOfGp)
      class(ShapeFunction_), intent(inout)::obj
      logical, optional, intent(in) :: ReducedIntegration
      character(:), allocatable ::  TrimedElemType
      integer(int32), optional, intent(in) :: NumOfDim, NumOfNodePerElem
      integer(int32), optional, intent(in) :: NumOfGp

      if (present(NumOfDim)) then
         if (present(NumOfNodePerElem)) then
            call obj%getType(NumOfDim, NumOfNodePerElem, NumOfGp)
         end if
      end if

      if (present(ReducedIntegration)) then
         obj%ReducedIntegration = ReducedIntegration
      end if

      TrimedElemType = obj%ElemType
      if (TrimedElemType == "LinearRectangularGp4") then

         obj%NumOfNode = 4
         obj%NumOfOrder = 1
         obj%NumOfDim = 2
         obj%NumOfGp = 4

      elseif (TrimedElemType == "LinearHexahedralGp8") then
         obj%NumOfNode = 8
         obj%NumOfOrder = 1
         obj%NumOfDim = 3
         obj%NumOfGp = 8
      elseif (TrimedElemType == "Triangular") then
         obj%NumOfNode = 3
         obj%NumOfOrder = 1
         obj%NumOfDim = 2
         obj%NumOfGp = 1
      elseif (TrimedElemType == "LinearTetrahedral") then
         obj%NumOfNode = 4
         obj%NumOfOrder = 1
         obj%NumOfDim = 3
         obj%NumOfGp = 1
      elseif (TrimedElemType == "QuadHexahedralGp8") then
         obj%NumOfNode = 20
         obj%NumOfOrder = 2
         obj%NumOfDim = 3
         obj%NumOfGp = 8
      elseif (TrimedElemType == "QuadHexahedralGp27") then
         obj%NumOfNode = 20
         obj%NumOfOrder = 2
         obj%NumOfDim = 3
         obj%NumOfGp = 27
      else
         obj%ErrorMsg = "ShapeFunctionClass.f90 :: SetShapeFuncType >> Element : "//TrimedElemType//"is not defined."
         print *, "ShapeFunctionClass.f90 :: SetShapeFuncType >> Element : ", TrimedElemType, "is not defined."
         return
      end if

   end subroutine SetShapeFuncType
!!  ##################################################

!!  ##################################################
   subroutine getShapeFuncType(obj, NumOfDim, NumOfNodePerElem, NumOfGp)
      class(ShapeFunction_), intent(inout)::obj
      character*70 ::  TrimedElemType
      integer(int32), intent(in) :: NumOfDim, NumOfNodePerElem
      integer(int32), optional, intent(in) :: NumOfGp

      if (NumOfDim == 2) then
         if (NumOfNodePerElem == 4) then
            obj%ElemType = "LinearRectangularGp4"
         else
            print *, "For 2D, NumOfNodePerElem = ", NumOfNodePerElem, " is not set."
         end if
      elseif (NumOfDim == 3) then
         if (NumOfNodePerElem == 8) then
            obj%ElemType = "LinearHexahedralGp8"
         elseif (NumOfNodePerElem == 20) then
            if (present(NumOfGP)) then
               if (NumOfGp == 8) then
                  obj%ElemType = "QuadHexahedralGp8"
               elseif (NumOfGp == 27) then
                  obj%ElemType = "QuadHexahedralGp27"
               else
                  print *, "For 3D, 20-noded element,Number of Gauss point ", NumOfGP, " is not ready."
               end if
            else
               obj%ElemType = "QuadHexahedralGp8"
            end if
         else
            print *, "For 3D, NumOfNodePerElem = ", NumOfNodePerElem, " is not set."
         end if
      else
         print *, "NumOfDim = ", NumOfDim, " is not set."
      end if

   end subroutine getShapeFuncType
!!  ##################################################

!!  ##################################################
   subroutine GetAllShapeFunc(obj, elem_id, nod_coord, nod_coord_n, elem_nod, OptionalNumOfNode, OptionalNumOfOrder, &
                              OptionalNumOfDim, OptionalNumOfGp, OptionalGpID, ReducedIntegration, NumOfDim, NumOfNodePerElem)
      class(ShapeFunction_), intent(inout)::obj
      integer(int32), optional, intent(in)::elem_nod(:, :), elem_id
      integer(int32), optional, intent(in) :: NumOfDim, NumOfNodePerElem
      real(real64), optional, intent(in)::nod_coord(:, :), nod_coord_n(:, :)
      integer(int32), optional, intent(in)::OptionalNumOfNode, OptionalNumOfOrder, &
                                             OptionalNumOfDim, OptionalNumOfGp, OptionalGpID
      logical, optional, intent(in) :: ReducedIntegration
      integer(int32) :: nd, ne

      nd = input(default=0, option=NumOfDim)
      ne = input(default=0, option=NumOfNodePerElem)
      if (present(nod_coord)) then
         nd = size(nod_coord, 2)
      end if
      if (present(nod_coord)) then
         ne = size(elem_nod, 2)
      end if

      if (obj%NumOfGp == 0 .and. obj%NumOfDim == 0) then

         if (nd == 0 .or. ne == 0) then
            print *, "ERROR :: GetAllShapeFunc >> please import NumOfDim and NumOfNodePerElem"
            stop
         end if
         call obj%SetType(NumOfDim=nd, NumOfNodePerElem=ne, ReducedIntegration=ReducedIntegration)
      end if
      if (present(ReducedIntegration)) then
         obj%ReducedIntegration = ReducedIntegration
      end if

      if (present(OptionalNumOfNode)) then
         obj%NumOfNode = OptionalNumOfNode
      end if

      if (present(OptionalNumOfOrder)) then
         obj%NumOfOrder = OptionalNumOfOrder
      end if

      if (present(OptionalNumOfDim)) then
         obj%NumOfDim = OptionalNumOfDim
      end if

      if (present(OptionalNumOfGp)) then
         obj%NumOfGp = OptionalNumOfGp
      end if

      if (present(OptionalGpID)) then
         obj%GpID = OptionalGpID
      end if

      call GetGaussPoint(obj)
      call SetGaussPoint(obj)

      call GetShapeFunction(obj)
      call GetShapeFuncDer1(obj)

      if (.not. present(nod_coord) .or. .not. present(elem_nod)) then

         obj%ErrorMsg = "Mesh%NodCoord and Mesh%ElemNod is necessary to get Jmat"
         print *, obj%ErrorMsg

         return
      end if
      call GetElemCoord(obj, nod_coord, elem_nod, elem_id)
      if (present(nod_coord_n)) then
         call GetElemCoord_n(obj, nod_coord_n, elem_nod, elem_id)
         call Getdu(obj)
      end if

      call GetJmat(obj)
      obj%detJ = det_mat(obj%Jmat, size(obj%Jmat, 1))

   end subroutine GetAllShapeFunc
!!  ##################################################

!!  ##################################################
   subroutine DeallocateShapeFunction(obj)
      class(ShapeFunction_), intent(inout)::obj

      if (allocated(obj%Nmat)) deallocate (obj%Nmat)
      if (allocated(obj%dNdgzi)) deallocate (obj%dNdgzi)
      if (allocated(obj%dNdgzidgzi)) deallocate (obj%dNdgzidgzi)
      if (allocated(obj%gzi)) deallocate (obj%gzi)
      if (allocated(obj%GaussPoint)) deallocate (obj%GaussPoint)
      if (allocated(obj%GaussIntegWei)) deallocate (obj%GaussIntegWei)
      if (allocated(obj%Jmat)) deallocate (obj%Jmat)

      obj%ErrorMsg = "All allocatable entities are deallocated"
   end subroutine DeallocateShapeFunction
!!  ##################################################

!!  ######################################
   subroutine GetGaussPoint(obj)
      !! This should not be called $OMP private, since it causes bug, where the GaussPoint is filled with 0.
      !! Instead, use firstprivate()
      

      class(ShapeFunction_), intent(inout)::obj
      real(real64) :: pval(1:3), wval(1:3)

      if (.not. allocated(obj%GaussPoint)) then
         allocate (obj%GaussPoint(obj%NumOfDim, obj%NumOfGp))
      else
         if (size(obj%GaussPoint, 1) /= obj%NumOfDim .or. &
             size(obj%GaussPoint, 2) /= obj%NumOfGp) then
            deallocate (obj%GaussPoint)
            allocate (obj%GaussPoint(obj%NumOfDim, obj%NumOfGp))
         end if
      end if

      if (.not. allocated(obj%GaussIntegWei)) then
         allocate (obj%GaussIntegWei(obj%NumOfGp))
      else
         if (size(obj%GaussIntegWei) /= obj%NumOfGp) then
            deallocate (obj%GaussIntegWei)
            allocate (obj%GaussIntegWei(obj%NumOfGp))
         end if
      end if

      if (size(obj%GaussPoint, 1) == 2 .and. size(obj%GaussPoint, 2) == 4) then
         obj%GaussPoint(1, 1) = -0.57735026918962576d0
         obj%GaussPoint(1, 2) = 0.57735026918962576d0
         obj%GaussPoint(1, 3) = 0.57735026918962576d0
         obj%GaussPoint(1, 4) = -0.57735026918962576d0

         obj%GaussPoint(2, 1) = -0.57735026918962576d0
         obj%GaussPoint(2, 2) = -0.57735026918962576d0
         obj%GaussPoint(2, 3) = 0.57735026918962576d0
         obj%GaussPoint(2, 4) = 0.57735026918962576d0
         obj%GaussIntegWei(:) = 1.0d0
         if (obj%ReducedIntegration .eqv. .true.) then
            obj%GaussPoint(:, :) = 0.0d0
            obj%GaussIntegWei(:) = 0.250d0
         end if
      elseif (size(obj%GaussPoint, 1) == 3 .and. size(obj%GaussPoint, 2) == 8) then
         obj%GaussPoint(1, 1) = -0.57735026918962576d0
         obj%GaussPoint(1, 2) = 0.57735026918962576d0
         obj%GaussPoint(1, 3) = 0.57735026918962576d0
         obj%GaussPoint(1, 4) = -0.57735026918962576d0
         obj%GaussPoint(1, 5) = -0.57735026918962576d0
         obj%GaussPoint(1, 6) = 0.57735026918962576d0
         obj%GaussPoint(1, 7) = 0.57735026918962576d0
         obj%GaussPoint(1, 8) = -0.57735026918962576d0

         obj%GaussPoint(2, 1) = -0.57735026918962576d0
         obj%GaussPoint(2, 2) = -0.57735026918962576d0
         obj%GaussPoint(2, 3) = 0.57735026918962576d0
         obj%GaussPoint(2, 4) = 0.57735026918962576d0
         obj%GaussPoint(2, 5) = -0.57735026918962576d0
         obj%GaussPoint(2, 6) = -0.57735026918962576d0
         obj%GaussPoint(2, 7) = 0.57735026918962576d0
         obj%GaussPoint(2, 8) = 0.57735026918962576d0

         obj%GaussPoint(3, 1) = -0.57735026918962576d0
         obj%GaussPoint(3, 2) = -0.57735026918962576d0
         obj%GaussPoint(3, 3) = -0.57735026918962576d0
         obj%GaussPoint(3, 4) = -0.57735026918962576d0
         obj%GaussPoint(3, 5) = 0.57735026918962576d0
         obj%GaussPoint(3, 6) = 0.57735026918962576d0
         obj%GaussPoint(3, 7) = 0.57735026918962576d0
         obj%GaussPoint(3, 8) = 0.57735026918962576d0
         obj%GaussIntegWei(:) = 1.0d0

         if (obj%ReducedIntegration .eqv. .true.) then
            obj%GaussPoint(:, :) = 0.0d0
            obj%GaussIntegWei(:) = 0.1250d0
         end if
      elseif (size(obj%GaussPoint, 1) == 3 .and. size(obj%GaussPoint, 2) == 27) then
         pval = [-sqrt(3.0d0/5.0d0), 0.0d0, sqrt(3.0d0/5.0d0)]
         wval = [5.0d0/9.0d0, 8.0d0/9.0d0, 5.0d0/9.0d0]
         obj%GaussPoint(1:3, 1) = [pval(1), pval(1), pval(1)]; obj%GaussIntegWei(1) = (wval(1)*wval(1)*wval(1))
         obj%GaussPoint(1:3, 2) = [pval(2), pval(1), pval(1)]; obj%GaussIntegWei(2) = (wval(2)*wval(1)*wval(1))
         obj%GaussPoint(1:3, 3) = [pval(3), pval(1), pval(1)]; obj%GaussIntegWei(3) = (wval(3)*wval(1)*wval(1))
         obj%GaussPoint(1:3, 4) = [pval(1), pval(2), pval(1)]; obj%GaussIntegWei(4) = (wval(1)*wval(2)*wval(1))
         obj%GaussPoint(1:3, 5) = [pval(2), pval(2), pval(1)]; obj%GaussIntegWei(5) = (wval(2)*wval(2)*wval(1))
         obj%GaussPoint(1:3, 6) = [pval(3), pval(2), pval(1)]; obj%GaussIntegWei(6) = (wval(3)*wval(2)*wval(1))
         obj%GaussPoint(1:3, 7) = [pval(1), pval(3), pval(1)]; obj%GaussIntegWei(7) = (wval(1)*wval(3)*wval(1))
         obj%GaussPoint(1:3, 8) = [pval(2), pval(3), pval(1)]; obj%GaussIntegWei(8) = (wval(2)*wval(3)*wval(1))
         obj%GaussPoint(1:3, 9) = [pval(3), pval(3), pval(1)]; obj%GaussIntegWei(9) = (wval(3)*wval(3)*wval(1))
         obj%GaussPoint(1:3, 10) = [pval(1), pval(1), pval(2)]; obj%GaussIntegWei(10) = (wval(1)*wval(1)*wval(2))
         obj%GaussPoint(1:3, 11) = [pval(2), pval(1), pval(2)]; obj%GaussIntegWei(11) = (wval(2)*wval(1)*wval(2))
         obj%GaussPoint(1:3, 12) = [pval(3), pval(1), pval(2)]; obj%GaussIntegWei(12) = (wval(3)*wval(1)*wval(2))
         obj%GaussPoint(1:3, 13) = [pval(1), pval(2), pval(2)]; obj%GaussIntegWei(13) = (wval(1)*wval(2)*wval(2))
         obj%GaussPoint(1:3, 14) = [pval(2), pval(2), pval(2)]; obj%GaussIntegWei(14) = (wval(2)*wval(2)*wval(2))
         obj%GaussPoint(1:3, 15) = [pval(3), pval(2), pval(2)]; obj%GaussIntegWei(15) = (wval(3)*wval(2)*wval(2))
         obj%GaussPoint(1:3, 16) = [pval(1), pval(3), pval(2)]; obj%GaussIntegWei(16) = (wval(1)*wval(3)*wval(2))
         obj%GaussPoint(1:3, 17) = [pval(2), pval(3), pval(2)]; obj%GaussIntegWei(17) = (wval(2)*wval(3)*wval(2))
         obj%GaussPoint(1:3, 18) = [pval(3), pval(3), pval(2)]; obj%GaussIntegWei(18) = (wval(3)*wval(3)*wval(2))
         obj%GaussPoint(1:3, 19) = [pval(1), pval(1), pval(3)]; obj%GaussIntegWei(19) = (wval(1)*wval(1)*wval(3))
         obj%GaussPoint(1:3, 20) = [pval(2), pval(1), pval(3)]; obj%GaussIntegWei(20) = (wval(2)*wval(1)*wval(3))
         obj%GaussPoint(1:3, 21) = [pval(3), pval(1), pval(3)]; obj%GaussIntegWei(21) = (wval(3)*wval(1)*wval(3))
         obj%GaussPoint(1:3, 22) = [pval(1), pval(2), pval(3)]; obj%GaussIntegWei(22) = (wval(1)*wval(2)*wval(3))
         obj%GaussPoint(1:3, 23) = [pval(2), pval(2), pval(3)]; obj%GaussIntegWei(23) = (wval(2)*wval(2)*wval(3))
         obj%GaussPoint(1:3, 24) = [pval(3), pval(2), pval(3)]; obj%GaussIntegWei(24) = (wval(3)*wval(2)*wval(3))
         obj%GaussPoint(1:3, 25) = [pval(1), pval(3), pval(3)]; obj%GaussIntegWei(25) = (wval(1)*wval(3)*wval(3))
         obj%GaussPoint(1:3, 26) = [pval(2), pval(3), pval(3)]; obj%GaussIntegWei(26) = (wval(2)*wval(3)*wval(3))
         obj%GaussPoint(1:3, 27) = [pval(3), pval(3), pval(3)]; obj%GaussIntegWei(27) = (wval(3)*wval(3)*wval(3))

      elseif (size(obj%GaussPoint, 2) == 1) then
            !!  Triangular or Tetrahedral
         if (allocated(obj%GaussPoint)) then
            deallocate (obj%GaussPoint)
         end if
         if (allocated(obj%GaussIntegWei)) then
            deallocate (obj%GaussIntegWei)
         end if

         obj%GaussPoint(1, 1) = 1.0d0
         obj%GaussPoint(2, 1) = 1.0d0
         obj%GaussPoint(3, 1) = 1.0d0
         obj%GaussIntegWei(:) = 1.0d0
      else
         print *, "ERROR :: ShapeFunctionClass/GetGaussPoint is not defined"
      end if

   end subroutine GetGaussPoint
!!  ######################################

!!  ######################################
   subroutine SetGaussPoint(obj)

      class(ShapeFunction_), intent(inout)::obj
      if (allocated(obj%gzi)) then
         if (size(obj%gzi, 1) /= obj%NumOfDim) then
            deallocate (obj%gzi)
            allocate (obj%gzi(obj%NumOfDim))
         end if
      else
         allocate (obj%gzi(obj%NumOfDim))
      end if

      if (obj%NumOfDim /= size(obj%GaussPoint, 1)) then
         print *, "ERROR::SetGaussPoint", obj%NumOfDim, size(obj%GaussPoint, 1)
         obj%ErrorMsg = "ERROR::SetGaussPoint"
         obj%ierr = 1
      else
         obj%gzi(:) = obj%GaussPoint(:, obj%GpID)

         obj%ErrorMsg = "Succeed::SetGaussPoint"
         obj%ierr = 0
      end if

   end subroutine SetGaussPoint
!!  ######################################

!!  ######################################
   subroutine GetShapeFunction(obj)
      class(ShapeFunction_), intent(inout)::obj

      if (allocated(obj%Nmat)) then
         if (size(obj%Nmat, 1) /= obj%NumOfNode) then
            deallocate (obj%Nmat)
            allocate (obj%Nmat(obj%NumOfNode))
         end if
      else
         allocate (obj%Nmat(obj%NumOfNode))
      end if

      if (obj%NumOfNode == 1) then
    !!  #########################################################################
    !!  #######                                                           #######
    !!  #######                             +     (1)                     #######
    !!  #######                                                           #######
    !!  #######                                                           #######
    !!  #########################################################################
         print *, "ERROR::GetShapeFunction"
         obj%ErrorMsg = "ERROR::GetShapeFunction NumOfNode==1 "
         obj%ierr = 1

      elseif (obj%NumOfNode == 2) then

         if (obj%NumOfDim == 1) then

        !!  #########################################################################
        !!  #######                                                           #######
        !!  #######                +-----------------------+                #######
        !!  #######            (1)                               (2)          #######
        !!  #######                                                           #######
        !!  #########################################################################

            obj%Nmat(1) = 0.50d0*(1.0d0 - obj%gzi(1))
            obj%Nmat(2) = 0.50d0*(-1.0d0 + obj%gzi(1))

            obj%ErrorMsg = "Succeed::GetShapeFunction "
            obj%ierr = 0
         else
            print *, "ERROR::GetShapeFunction"
            obj%ErrorMsg = "ERROR::GetShapeFunction NumOfNode==2 NumOfOrder/=1 "
            obj%ierr = 1
         end if
      elseif (obj%NumOfNode == 3) then
         if (obj%NumOfDim == 1) then
        !!  #########################################################################
        !!  #######                                                           #######
        !!  #######                +-----------+-------------+                #######
        !!  #######            (1)             (2)            (3)             #######
        !!  #######                                                           #######
        !!  #########################################################################
        !! allocate(obj%Nmat(obj%NumOfNode,obj%NumOfDim) )
        !!
        !! obj%Nmat(1,1)=0.50d0*( 1.0d0-gzi(1))
        !! obj%Nmat(1,2)=0.50d0*(-1.0d0+gzi(1))
        !! obj%Nmat(1,3)=0.50d0*( 1.0d0-gzi(1))
        !!
        !! obj%ErrorMsg="Succeed::GetShapeFunction "
        !! obj%ierr=0
            print *, "ERROR::GetShapeFunction"
            obj%ErrorMsg = "ERROR::GetShapeFunction "
            obj%ierr = 1
         elseif (obj%NumOfDim == 2) then
        !!  #########################################################################
        !!  #######                                                           #######
        !!  #######          (1)   +-------------------------+                #######
        !!  #######                 \                       /  (3)            #######
        !!  #######                   \                   /                   #######
        !!  #######                     \               /                     #######
        !!  #######                       \           /                       #######
        !!  #######                         \       /                         #######
        !!  #######                           \   /                           #######
        !!  #######                       (2)   +                             #######
        !!  #########################################################################

            print *, "ERROR::GetShapeFunction"
            obj%ErrorMsg = "ERROR::GetShapeFunction "
            obj%ierr = 1
         else

            print *, "ERROR::GetShapeFunction"
            obj%ErrorMsg = "ERROR::GetShapeFunction "
            obj%ierr = 1
         end if
      elseif (obj%NumOfNode == 4) then
         if (obj%NumOfDim == 1) then
        !!  #########################################################################
        !!  #######                                                           #######
        !!  #######                +-------+---------+-------+                #######
        !!  #######          (1)          (2)        (3)       (4)            #######
        !!  #######                                                           #######
        !!  #########################################################################
        !! obj%ErrorMsg="Succeed::GetShapeFunction "
        !! obj%ierr=0
            print *, "ERROR::GetShapeFunction"
            obj%ErrorMsg = "ERROR::GetShapeFunction "
            obj%ierr = 1
         elseif (obj%NumOfDim == 2) then
        !!  #########################################################################
        !!  #######                                                           #######
        !!  #######           (1)  +-------------------------+  (4)           #######
        !!  #######                !                         !                #######
        !!  #######                !                         !                #######
        !!  #######                !                         !                #######
        !!  #######                !                         !                #######
        !!  #######                !                         !                #######
        !!  #######                !                         !                #######
        !!  #######           (2)  +-------------------------+  (3)           #######
        !!  #########################################################################

            obj%Nmat(1) = 0.250d0*(1.0d0 - obj%gzi(1))*(1.0d0 - obj%gzi(2))
            obj%Nmat(2) = 0.250d0*(1.0d0 + obj%gzi(1))*(1.0d0 - obj%gzi(2))
            obj%Nmat(3) = 0.250d0*(1.0d0 + obj%gzi(1))*(1.0d0 + obj%gzi(2))
            obj%Nmat(4) = 0.250d0*(1.0d0 - obj%gzi(1))*(1.0d0 + obj%gzi(2))

            obj%ErrorMsg = "Succeed::GetShapeFunction "
            obj%ierr = 0
         elseif (obj%NumOfDim == 3) then
        !!  #########################################################################
        !!  #######                    (4)   +                                #######
        !!  #######                        /   \                              #######
        !!  #######                      /       \                            #######
        !!  #######                    /           \                          #######
        !!  #######                  /           ___+    (3)                  #######
        !!  #######                /  ___----         \                        #######
        !!  #######          (1)  +  -                 \                      #######
        !!  #######                    -----____       \                      #######
        !!  #######                              ----- +   (2)               #######
        !!  #########################################################################

            print *, "ERROR::GetShapeFunction"
            obj%ErrorMsg = "ERROR::GetShapeFunction "
            obj%ierr = 1
         else
            print *, "ERROR::GetShapeFunction"
            obj%ErrorMsg = "ERROR::GetShapeFunction "
            obj%ierr = 1
         end if

      elseif (obj%NumOfNode == 5) then
         print *, "ERROR::GetShapeFunction"
         obj%ErrorMsg = "ERROR::GetShapeFunction "
         obj%ierr = 1
      elseif (obj%NumOfNode == 6) then
         print *, "ERROR::GetShapeFunction"
         obj%ErrorMsg = "ERROR::GetShapeFunction "
         obj%ierr = 1
      elseif (obj%NumOfNode == 7) then
         print *, "ERROR::GetShapeFunction"
         obj%ErrorMsg = "ERROR::GetShapeFunction "
         obj%ierr = 1
      elseif (obj%NumOfNode == 8) then
         if (obj%NumOfDim == 3) then
        !!  #########################################################################
        !!  #######           (8)   +-------------------------+ (7)           #######
        !!  #######                /!                        /!               #######
        !!  #######               / !                  (6)  / !               #######
        !!  #######          (5) +--!----------------------+  !               #######
        !!  #######              !  !                      !  !               #######
        !!  #######              !  +----------------------!--+ (3)           #######
        !!  #######              ! / (4)                   ! /                #######
        !!  #######              !/                        !/                 #######
        !!  #######          (1) +-------------------------+ (2)              #######
        !!  #########################################################################

            obj%Nmat(1) = 0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0 - obj%gzi(2))*(1.0d0 - obj%gzi(3))
            obj%Nmat(2) = 0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0 - obj%gzi(2))*(1.0d0 - obj%gzi(3))
            obj%Nmat(3) = 0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0 + obj%gzi(2))*(1.0d0 - obj%gzi(3))
            obj%Nmat(4) = 0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0 + obj%gzi(2))*(1.0d0 - obj%gzi(3))
            obj%Nmat(5) = 0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0 - obj%gzi(2))*(1.0d0 + obj%gzi(3))
            obj%Nmat(6) = 0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0 - obj%gzi(2))*(1.0d0 + obj%gzi(3))
            obj%Nmat(7) = 0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0 + obj%gzi(2))*(1.0d0 + obj%gzi(3))
            obj%Nmat(8) = 0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0 + obj%gzi(2))*(1.0d0 + obj%gzi(3))

            obj%ErrorMsg = "Succeed::GetShapeFunction "
            obj%ierr = 0
         else
            print *, "ERROR::GetShapeFunction"
            obj%ErrorMsg = "ERROR::GetShapeFunction "
            obj%ierr = 1
         end if

      elseif (obj%NumOfNode == 9) then
         print *, "ERROR::GetShapeFunction"
         obj%ErrorMsg = "ERROR::GetShapeFunction "
         obj%ierr = 1
      elseif (obj%NumOfNode == 10) then
         print *, "ERROR::GetShapeFunction"
         obj%ErrorMsg = "ERROR::GetShapeFunction "
         obj%ierr = 1
      elseif (obj%NumOfNode == 11) then
         print *, "ERROR::GetShapeFunction"
         obj%ErrorMsg = "ERROR::GetShapeFunction "
         obj%ierr = 1
      elseif (obj%NumOfNode == 12) then
         print *, "ERROR::GetShapeFunction"
         obj%ErrorMsg = "ERROR::GetShapeFunction "
         obj%ierr = 1
      elseif (obj%NumOfNode == 13) then
         print *, "ERROR::GetShapeFunction"
         obj%ErrorMsg = "ERROR::GetShapeFunction "
         obj%ierr = 1
      elseif (obj%NumOfNode == 14) then
         print *, "ERROR::GetShapeFunction"
         obj%ErrorMsg = "ERROR::GetShapeFunction "
         obj%ierr = 1
      elseif (obj%NumOfNode == 15) then
         print *, "ERROR::GetShapeFunction"
         obj%ErrorMsg = "ERROR::GetShapeFunction "
         obj%ierr = 1
      elseif (obj%NumOfNode == 16) then
         print *, "ERROR::GetShapeFunction"
         obj%ErrorMsg = "ERROR::GetShapeFunction "
         obj%ierr = 1
      elseif (obj%NumOfNode == 17) then
         print *, "ERROR::GetShapeFunction"
         obj%ErrorMsg = "ERROR::GetShapeFunction "
         obj%ierr = 1
      elseif (obj%NumOfNode == 18) then
         print *, "ERROR::GetShapeFunction"
         obj%ErrorMsg = "ERROR::GetShapeFunction "
         obj%ierr = 1
      elseif (obj%NumOfNode == 19) then
         print *, "ERROR::GetShapeFunction"
         obj%ErrorMsg = "ERROR::GetShapeFunction "
         obj%ierr = 1
      elseif (obj%NumOfNode == 20) then
         if (obj%NumOfDim == 3) then
        !!  #########################################################################
        !!  #######           (8)   +----------(15)------------+ (7)          #######
        !!  #######        (16)    /!                 (14)    /!              #######
        !!  #######               / ! (20)              (6)  /  ! (19)         #######
        !!  #######          (5) +--!-------(13)------------+  !              #######
        !!  #######              !  !                      !  !               #######
        !!  #######          (17)!  +----------(11)--------!--+ (3)           #######
        !!  #######          (12)! / (4)              (18) ! / (10)           #######
        !!  #######              !/                        !/                 #######
        !!  #######          (1) +----------(9)------------+ (2)              #######
        !!  #########################################################################

            ! Koers, R.W.J., 1989. Use of modified standard 20-node isoparametric brick elements for representing stress/strain fields at a crack tip for elastic and perfectly plastic material. Int. J. Fract. 40, 79–110. https://doi.org/10.1007/BF00963969
            obj%Nmat(1) = -0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0 - obj%gzi(2))*(1.0d0 - obj%gzi(3)) &
                          *(2.0d0 + obj%gzi(1) + obj%gzi(2) + obj%gzi(3))
            obj%Nmat(2) = -0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0 - obj%gzi(2))*(1.0d0 - obj%gzi(3)) &
                          *(2.0d0 - obj%gzi(1) + obj%gzi(2) + obj%gzi(3))
            obj%Nmat(3) = -0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0 + obj%gzi(2))*(1.0d0 - obj%gzi(3)) &
                          *(2.0d0 - obj%gzi(1) - obj%gzi(2) + obj%gzi(3))
            obj%Nmat(4) = -0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0 + obj%gzi(2))*(1.0d0 - obj%gzi(3)) &
                          *(2.0d0 + obj%gzi(1) - obj%gzi(2) + obj%gzi(3))
            obj%Nmat(5) = -0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0 - obj%gzi(2))*(1.0d0 + obj%gzi(3)) &
                          *(2.0d0 + obj%gzi(1) + obj%gzi(2) - obj%gzi(3))
            obj%Nmat(6) = -0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0 - obj%gzi(2))*(1.0d0 + obj%gzi(3)) &
                          *(2.0d0 - obj%gzi(1) + obj%gzi(2) - obj%gzi(3))
            obj%Nmat(7) = -0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0 + obj%gzi(2))*(1.0d0 + obj%gzi(3)) &
                          *(2.0d0 - obj%gzi(1) - obj%gzi(2) - obj%gzi(3))
            obj%Nmat(8) = -0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0 + obj%gzi(2))*(1.0d0 + obj%gzi(3)) &
                          *(2.0d0 + obj%gzi(1) - obj%gzi(2) - obj%gzi(3))

            obj%Nmat(9) = 0.250d0*(1.0d0 - obj%gzi(1)**2)*(1.0d0 - obj%gzi(2))*(1.0d0 - obj%gzi(3))
            obj%Nmat(10) = 0.250d0*(1.0d0 + obj%gzi(1))*(1.0d0 - obj%gzi(2)**2)*(1.0d0 - obj%gzi(3))
            obj%Nmat(11) = 0.250d0*(1.0d0 - obj%gzi(1)**2)*(1.0d0 + obj%gzi(2))*(1.0d0 - obj%gzi(3))
            obj%Nmat(12) = 0.250d0*(1.0d0 - obj%gzi(1))*(1.0d0 - obj%gzi(2)**2)*(1.0d0 - obj%gzi(3))

            obj%Nmat(13) = 0.250d0*(1.0d0 - obj%gzi(1)**2)*(1.0d0 - obj%gzi(2))*(1.0d0 + obj%gzi(3))
            obj%Nmat(14) = 0.250d0*(1.0d0 + obj%gzi(1))*(1.0d0 - obj%gzi(2)**2)*(1.0d0 + obj%gzi(3))
            obj%Nmat(15) = 0.250d0*(1.0d0 - obj%gzi(1)**2)*(1.0d0 + obj%gzi(2))*(1.0d0 + obj%gzi(3))
            obj%Nmat(16) = 0.250d0*(1.0d0 - obj%gzi(1))*(1.0d0 - obj%gzi(2)**2)*(1.0d0 + obj%gzi(3))

            obj%Nmat(17) = 0.250d0*(1.0d0 - obj%gzi(1))*(1.0d0 - obj%gzi(2))*(1.0d0 - obj%gzi(3)**2)
            obj%Nmat(18) = 0.250d0*(1.0d0 + obj%gzi(1))*(1.0d0 - obj%gzi(2))*(1.0d0 - obj%gzi(3)**2)
            obj%Nmat(19) = 0.250d0*(1.0d0 + obj%gzi(1))*(1.0d0 + obj%gzi(2))*(1.0d0 - obj%gzi(3)**2)
            obj%Nmat(20) = 0.250d0*(1.0d0 - obj%gzi(1))*(1.0d0 + obj%gzi(2))*(1.0d0 - obj%gzi(3)**2)

            obj%ErrorMsg = "Succeed::GetShapeFunction "
            obj%ierr = 0
         else
            print *, "ERROR::GetShapeFunction"
            obj%ErrorMsg = "ERROR::GetShapeFunction "
            obj%ierr = 1
         end if

      elseif (obj%NumOfNode == 21) then
         print *, "ERROR::GetShapeFunction"
         obj%ErrorMsg = "ERROR::GetShapeFunction "
         obj%ierr = 1
      elseif (obj%NumOfNode == 22) then
         print *, "ERROR::GetShapeFunction"
         obj%ErrorMsg = "ERROR::GetShapeFunction "
         obj%ierr = 1
      elseif (obj%NumOfNode == 23) then
         print *, "ERROR::GetShapeFunction"
         obj%ErrorMsg = "ERROR::GetShapeFunction "
         obj%ierr = 1
      elseif (obj%NumOfNode == 24) then
         print *, "ERROR::GetShapeFunction"
         obj%ErrorMsg = "ERROR::GetShapeFunction "
         obj%ierr = 1
      else
         print *, "ERROR::GetShapeFunction"
         obj%ErrorMsg = "ERROR::GetShapeFunction "
         obj%ierr = 1
      end if

   end subroutine GetShapeFunction
!!  ######################################

!!  ######################################
   subroutine GetShapeFuncDer1(obj)
      class(ShapeFunction_), intent(inout)::obj

      if (allocated(obj%dNdgzi)) then
         if (size(obj%dNdgzi, 1) /= obj%NumOfDim .or. size(obj%dNdgzi, 2) /= obj%NumOfNode) then
            deallocate (obj%dNdgzi)
            allocate (obj%dNdgzi(obj%NumOfDim, obj%NumOfNode))
         end if
      else
         allocate (obj%dNdgzi(obj%NumOfDim, obj%NumOfNode))
      end if

      if (obj%NumOfNode == 1) then
            !!  #########################################################################
            !!  #######                                                           #######
            !!  #######                             +     (1)                     #######
            !!  #######                                                           #######
            !!  #######                                                           #######
            !!  #########################################################################
         print *, "ERROR::GetShapeFunction"
         obj%ErrorMsg = "ERROR::GetShapeFunction NumOfNode==1 "
         obj%ierr = 1

      elseif (obj%NumOfNode == 2) then

         if (obj%NumOfDim == 1) then

                !!  #########################################################################
                !!  #######                                                           #######
                !!  #######                +-----------------------+                #######
                !!  #######            (1)                               (2)          #######
                !!  #######                                                           #######
                !!  #########################################################################

            obj%dNdgzi(1, 1) = -0.50d0
            obj%dNdgzi(1, 2) = 0.50d0

            obj%ErrorMsg = "Succeed::GetShapeFunction "
            obj%ierr = 0
         else
            print *, "ERROR::GetShapeFunction"
            obj%ErrorMsg = "ERROR::GetShapeFunction NumOfNode==2 NumOfOrder/=1 "
            obj%ierr = 1
         end if
      elseif (obj%NumOfNode == 3) then
         if (obj%NumOfDim == 1) then
                !!  #########################################################################
                !!  #######                                                           #######
                !!  #######                +-----------+-------------+                #######
                !!  #######            (1)             (2)            (3)             #######
                !!  #######                                                           #######
                !!  #########################################################################
                !! allocate(obj%dNdgzi(obj%NumOfNode,obj%NumOfDim) )
                !!
                !! obj%dNdgzi(1,1)=0.50d0*( 1.0d0-gzi(1))
                !! obj%dNdgzi(1,2)=0.50d0*(-1.0d0+gzi(1))
                !! obj%dNdgzi(1,3)=0.50d0*( 1.0d0-gzi(1))
                !!
                !! obj%ErrorMsg="Succeed::GetShapeFunction "
                !! obj%ierr=0
            print *, "ERROR::GetShapeFunction"
            obj%ErrorMsg = "ERROR::GetShapeFunction "
            obj%ierr = 1
         elseif (obj%NumOfDim == 2) then
                !!  #########################################################################
                !!  #######                                                           #######
                !!  #######          (1)   +-------------------------+                #######
                !!  #######                 \                       /  (3)            #######
                !!  #######                   \                   /                   #######
                !!  #######                     \               /                     #######
                !!  #######                       \           /                       #######
                !!  #######                         \       /                         #######
                !!  #######                           \   /                           #######
                !!  #######                       (2)   +                             #######
                !!  #########################################################################

            print *, "ERROR::GetShapeFunction"
            obj%ErrorMsg = "ERROR::GetShapeFunction "
            obj%ierr = 1
         else

            print *, "ERROR::GetShapeFunction"
            obj%ErrorMsg = "ERROR::GetShapeFunction "
            obj%ierr = 1
         end if
      elseif (obj%NumOfNode == 4) then
         if (obj%NumOfDim == 1) then
                !!  #########################################################################
                !!  #######                                                           #######
                !!  #######                +-------+---------+-------+                #######
                !!  #######          (1)          (2)        (3)       (4)            #######
                !!  #######                                                           #######
                !!  #########################################################################
                !! obj%ErrorMsg="Succeed::GetShapeFunction "
                !! obj%ierr=0
            print *, "ERROR::GetShapeFunction"
            obj%ErrorMsg = "ERROR::GetShapeFunction "
            obj%ierr = 1
         elseif (obj%NumOfDim == 2) then
                !!  #########################################################################
                !!  #######                                                           #######
                !!  #######           (4)  +-------------------------+  (3)           #######
                !!  #######                !                         !                #######
                !!  #######                !                         !                #######
                !!  #######                !                         !                #######
                !!  #######                !                         !                #######
                !!  #######                !                         !                #######
                !!  #######                !                         !                #######
                !!  #######           (1)  +-------------------------+  (2)           #######
                !!  #########################################################################

            obj%dNdgzi(1, 1) = -0.250d0*(1.0d0 - obj%gzi(2))
            obj%dNdgzi(1, 2) = 0.250d0*(1.0d0 - obj%gzi(2))
            obj%dNdgzi(1, 3) = 0.250d0*(1.0d0 + obj%gzi(2))
            obj%dNdgzi(1, 4) = -0.250d0*(1.0d0 + obj%gzi(2))

            obj%dNdgzi(2, 1) = -0.250d0*(1.0d0 - obj%gzi(1))
            obj%dNdgzi(2, 2) = -0.250d0*(1.0d0 + obj%gzi(1))
            obj%dNdgzi(2, 3) = 0.250d0*(1.0d0 + obj%gzi(1))
            obj%dNdgzi(2, 4) = 0.250d0*(1.0d0 - obj%gzi(1))

            obj%ErrorMsg = "Succeed::GetShapeFunction "
            obj%ierr = 0
         elseif (obj%NumOfDim == 3) then
                !!  #########################################################################
                !!  #######                    (4)   +                                #######
                !!  #######                        /   \                              #######
                !!  #######                      /       \                            #######
                !!  #######                    /           \                          #######
                !!  #######                  /           ___+    (3)                  #######
                !!  #######                /  ___----         \                        #######
                !!  #######          (1)  +  -                 \                      #######
                !!  #######                    -----____       \                      #######
                !!  #######                              ----- +   (2)               #######
                !!  #########################################################################

            print *, "ERROR::GetShapeFunction"
            obj%ErrorMsg = "ERROR::GetShapeFunction "
            obj%ierr = 1
         else
            print *, "ERROR::GetShapeFunction"
            obj%ErrorMsg = "ERROR::GetShapeFunction "
            obj%ierr = 1
         end if

      elseif (obj%NumOfNode == 5) then
         print *, "ERROR::GetShapeFunction"
         obj%ErrorMsg = "ERROR::GetShapeFunction "
         obj%ierr = 1
      elseif (obj%NumOfNode == 6) then
         print *, "ERROR::GetShapeFunction"
         obj%ErrorMsg = "ERROR::GetShapeFunction "
         obj%ierr = 1
      elseif (obj%NumOfNode == 7) then
         print *, "ERROR::GetShapeFunction"
         obj%ErrorMsg = "ERROR::GetShapeFunction "
         obj%ierr = 1
      elseif (obj%NumOfNode == 8) then
         if (obj%NumOfDim == 3) then
                !!  #########################################################################
                !!  #######           (8)   +-------------------------+ (7)           #######
                !!  #######                /!                        /!               #######
                !!  #######               / !                  (6)  / !               #######
                !!  #######          (5) +--!----------------------+  !               #######
                !!  #######              !  !                      !  !               #######
                !!  #######              !  +----------------------!--+ (3)           #######
                !!  #######              ! / (4)                   ! /                #######
                !!  #######              !/                        !/                 #######
                !!  #######          (1) +-------------------------+ (2)              #######
                !!  #########################################################################

            obj%dNdgzi(1, 1) = -0.1250d0*(1.0d0 - obj%gzi(2))*(1.0d0 - obj%gzi(3))
            obj%dNdgzi(1, 2) = +0.1250d0*(1.0d0 - obj%gzi(2))*(1.0d0 - obj%gzi(3))
            obj%dNdgzi(1, 3) = +0.1250d0*(1.0d0 + obj%gzi(2))*(1.0d0 - obj%gzi(3))
            obj%dNdgzi(1, 4) = -0.1250d0*(1.0d0 + obj%gzi(2))*(1.0d0 - obj%gzi(3))
            obj%dNdgzi(1, 5) = -0.1250d0*(1.0d0 - obj%gzi(2))*(1.0d0 + obj%gzi(3))
            obj%dNdgzi(1, 6) = +0.1250d0*(1.0d0 - obj%gzi(2))*(1.0d0 + obj%gzi(3))
            obj%dNdgzi(1, 7) = +0.1250d0*(1.0d0 + obj%gzi(2))*(1.0d0 + obj%gzi(3))
            obj%dNdgzi(1, 8) = -0.1250d0*(1.0d0 + obj%gzi(2))*(1.0d0 + obj%gzi(3))

            obj%dNdgzi(2, 1) = -0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0 - obj%gzi(3))
            obj%dNdgzi(2, 2) = -0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0 - obj%gzi(3))
            obj%dNdgzi(2, 3) = +0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0 - obj%gzi(3))
            obj%dNdgzi(2, 4) = +0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0 - obj%gzi(3))
            obj%dNdgzi(2, 5) = -0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0 + obj%gzi(3))
            obj%dNdgzi(2, 6) = -0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0 + obj%gzi(3))
            obj%dNdgzi(2, 7) = +0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0 + obj%gzi(3))
            obj%dNdgzi(2, 8) = +0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0 + obj%gzi(3))

            obj%dNdgzi(3, 1) = -0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0 - obj%gzi(2))
            obj%dNdgzi(3, 2) = -0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0 - obj%gzi(2))
            obj%dNdgzi(3, 3) = -0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0 + obj%gzi(2))
            obj%dNdgzi(3, 4) = -0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0 + obj%gzi(2))
            obj%dNdgzi(3, 5) = +0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0 - obj%gzi(2))
            obj%dNdgzi(3, 6) = +0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0 - obj%gzi(2))
            obj%dNdgzi(3, 7) = +0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0 + obj%gzi(2))
            obj%dNdgzi(3, 8) = +0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0 + obj%gzi(2))

            obj%ErrorMsg = "Succeed::GetShapeFunction "
            obj%ierr = 0
         else
            print *, "ERROR::GetShapeFunction"
            obj%ErrorMsg = "ERROR::GetShapeFunction "
            obj%ierr = 1
         end if

      elseif (obj%NumOfNode == 9) then
         print *, "ERROR::GetShapeFunction"
         obj%ErrorMsg = "ERROR::GetShapeFunction "
         obj%ierr = 1
      elseif (obj%NumOfNode == 10) then
         print *, "ERROR::GetShapeFunction"
         obj%ErrorMsg = "ERROR::GetShapeFunction "
         obj%ierr = 1
      elseif (obj%NumOfNode == 11) then
         print *, "ERROR::GetShapeFunction"
         obj%ErrorMsg = "ERROR::GetShapeFunction "
         obj%ierr = 1
      elseif (obj%NumOfNode == 12) then
         print *, "ERROR::GetShapeFunction"
         obj%ErrorMsg = "ERROR::GetShapeFunction "
         obj%ierr = 1
      elseif (obj%NumOfNode == 13) then
         print *, "ERROR::GetShapeFunction"
         obj%ErrorMsg = "ERROR::GetShapeFunction "
         obj%ierr = 1
      elseif (obj%NumOfNode == 14) then
         print *, "ERROR::GetShapeFunction"
         obj%ErrorMsg = "ERROR::GetShapeFunction "
         obj%ierr = 1
      elseif (obj%NumOfNode == 15) then
         print *, "ERROR::GetShapeFunction"
         obj%ErrorMsg = "ERROR::GetShapeFunction "
         obj%ierr = 1
      elseif (obj%NumOfNode == 16) then
         print *, "ERROR::GetShapeFunction"
         obj%ErrorMsg = "ERROR::GetShapeFunction "
         obj%ierr = 1
      elseif (obj%NumOfNode == 17) then
         print *, "ERROR::GetShapeFunction"
         obj%ErrorMsg = "ERROR::GetShapeFunction "
         obj%ierr = 1
      elseif (obj%NumOfNode == 18) then
         print *, "ERROR::GetShapeFunction"
         obj%ErrorMsg = "ERROR::GetShapeFunction "
         obj%ierr = 1
      elseif (obj%NumOfNode == 19) then
         print *, "ERROR::GetShapeFunction"
         obj%ErrorMsg = "ERROR::GetShapeFunction "
         obj%ierr = 1
      elseif (obj%NumOfNode == 20) then
         if (obj%NumOfDim == 3) then
                !!  #########################################################################
                !!  #######           (8)   +----------(15)------------+ (7)          #######
                !!  #######        (16)    /!                 (14)    /!              #######
                !!  #######               / ! (20)              (6)  /  ! (19)         #######
                !!  #######          (5) +--!-------(13)------------+  !              #######
                !!  #######              !  !                      !  !               #######
                !!  #######          (17)!  +----------(11)--------!--+ (3)           #######
                !!  #######          (12)! / (4)              (18) ! / (10)           #######
                !!  #######              !/                        !/                 #######
                !!  #######          (1) +----------(9)------------+ (2)              #######
                !!  #########################################################################

            ! Koers, R.W.J., 1989. Use of modified standard 20-node isoparametric brick elements for representing stress/strain fields at a crack tip for elastic and perfectly plastic material. Int. J. Fract. 40, 79–110. https://doi.org/10.1007/BF00963969
            obj%dNdgzi(1, 1) = -0.1250d0*(-1.0d0)*(1.0d0 - obj%gzi(2))*(1.0d0 - obj%gzi(3)) &
                               *(2.0d0 + obj%gzi(1) + obj%gzi(2) + obj%gzi(3)) &
                               - 0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0 - obj%gzi(2))*(1.0d0 - obj%gzi(3)) &
                               *1.0d0
            obj%dNdgzi(1, 2) = -0.1250d0*(1.0d0)*(1.0d0 - obj%gzi(2))*(1.0d0 - obj%gzi(3)) &
                               *(2.0d0 - obj%gzi(1) + obj%gzi(2) + obj%gzi(3)) &
                               - 0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0 - obj%gzi(2))*(1.0d0 - obj%gzi(3)) &
                               *(-1.0d0)
            obj%dNdgzi(1, 3) = -0.1250d0*(1.0d0)*(1.0d0 + obj%gzi(2))*(1.0d0 - obj%gzi(3)) &
                               *(2.0d0 - obj%gzi(1) - obj%gzi(2) + obj%gzi(3)) &
                               - 0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0 + obj%gzi(2))*(1.0d0 - obj%gzi(3)) &
                               *(-1.0d0)
            obj%dNdgzi(1, 4) = -0.1250d0*(-1.0d0)*(1.0d0 + obj%gzi(2))*(1.0d0 - obj%gzi(3)) &
                               *(2.0d0 + obj%gzi(1) - obj%gzi(2) + obj%gzi(3)) &
                               - 0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0 + obj%gzi(2))*(1.0d0 - obj%gzi(3)) &
                               *(1.0d0)
            obj%dNdgzi(1, 5) = -0.1250d0*(-1.0d0)*(1.0d0 - obj%gzi(2))*(1.0d0 + obj%gzi(3)) &
                               *(2.0d0 + obj%gzi(1) + obj%gzi(2) - obj%gzi(3)) &
                               - 0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0 - obj%gzi(2))*(1.0d0 + obj%gzi(3)) &
                               *(1.0d0)
            obj%dNdgzi(1, 6) = -0.1250d0*(1.0d0)*(1.0d0 - obj%gzi(2))*(1.0d0 + obj%gzi(3)) &
                               *(2.0d0 - obj%gzi(1) + obj%gzi(2) - obj%gzi(3)) &
                               - 0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0 - obj%gzi(2))*(1.0d0 + obj%gzi(3)) &
                               *(-1.0d0)
            obj%dNdgzi(1, 7) = -0.1250d0*(1.0d0)*(1.0d0 + obj%gzi(2))*(1.0d0 + obj%gzi(3)) &
                               *(2.0d0 - obj%gzi(1) - obj%gzi(2) - obj%gzi(3)) &
                               - 0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0 + obj%gzi(2))*(1.0d0 + obj%gzi(3)) &
                               *(-1.0d0)
            obj%dNdgzi(1, 8) = -0.1250d0*(-1.0d0)*(1.0d0 + obj%gzi(2))*(1.0d0 + obj%gzi(3)) &
                               *(2.0d0 + obj%gzi(1) - obj%gzi(2) - obj%gzi(3)) &
                               - 0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0 + obj%gzi(2))*(1.0d0 + obj%gzi(3)) &
                               *(1.0d0)
            !<<<ok>>>

            obj%dNdgzi(1, 9) = 0.250d0*(-2.0d0*obj%gzi(1))*(1.0d0 - obj%gzi(2))*(1.0d0 - obj%gzi(3))
            obj%dNdgzi(1, 10) = 0.250d0*(1.0d0)*(1.0d0 - obj%gzi(2)**2)*(1.0d0 - obj%gzi(3))
            obj%dNdgzi(1, 11) = 0.250d0*(-2.0d0*obj%gzi(1))*(1.0d0 + obj%gzi(2))*(1.0d0 - obj%gzi(3))
            obj%dNdgzi(1, 12) = 0.250d0*(-1.0d0)*(1.0d0 - obj%gzi(2)**2)*(1.0d0 - obj%gzi(3))

            obj%dNdgzi(1, 13) = 0.250d0*(-2.0d0*obj%gzi(1))*(1.0d0 - obj%gzi(2))*(1.0d0 + obj%gzi(3))
            obj%dNdgzi(1, 14) = 0.250d0*(1.0d0)*(1.0d0 - obj%gzi(2)**2)*(1.0d0 + obj%gzi(3))
            obj%dNdgzi(1, 15) = 0.250d0*(-2.0d0*obj%gzi(1))*(1.0d0 + obj%gzi(2))*(1.0d0 + obj%gzi(3))
            obj%dNdgzi(1, 16) = 0.250d0*(-1.0d0)*(1.0d0 - obj%gzi(2)**2)*(1.0d0 + obj%gzi(3))

            obj%dNdgzi(1, 17) = 0.250d0*(-1.0d0)*(1.0d0 - obj%gzi(2))*(1.0d0 - obj%gzi(3)**2)
            obj%dNdgzi(1, 18) = 0.250d0*(1.0d0)*(1.0d0 - obj%gzi(2))*(1.0d0 - obj%gzi(3)**2)
            obj%dNdgzi(1, 19) = 0.250d0*(1.0d0)*(1.0d0 + obj%gzi(2))*(1.0d0 - obj%gzi(3)**2)
            obj%dNdgzi(1, 20) = 0.250d0*(-1.0d0)*(1.0d0 + obj%gzi(2))*(1.0d0 - obj%gzi(3)**2)
            !<<<ok>>>

            obj%dNdgzi(2, 1) = -0.1250d0*(1.0d0 - obj%gzi(1))*(-1.0d0)*(1.0d0 - obj%gzi(3)) &
                               *(2.0d0 + obj%gzi(1) + obj%gzi(2) + obj%gzi(3)) &
                               - 0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0 - obj%gzi(2))*(1.0d0 - obj%gzi(3)) &
                               *(1.0d0)
            obj%dNdgzi(2, 2) = -0.1250d0*(1.0d0 + obj%gzi(1))*(-1.0d0)*(1.0d0 - obj%gzi(3)) &
                               *(2.0d0 - obj%gzi(1) + obj%gzi(2) + obj%gzi(3)) &
                               - 0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0 - obj%gzi(2))*(1.0d0 - obj%gzi(3)) &
                               *(1.0d0)
            obj%dNdgzi(2, 3) = -0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0)*(1.0d0 - obj%gzi(3)) &
                               *(2.0d0 - obj%gzi(1) - obj%gzi(2) + obj%gzi(3)) &
                               - 0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0 + obj%gzi(2))*(1.0d0 - obj%gzi(3)) &
                               *(-1.0d0)
            obj%dNdgzi(2, 4) = -0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0)*(1.0d0 - obj%gzi(3)) &
                               *(2.0d0 + obj%gzi(1) - obj%gzi(2) + obj%gzi(3)) &
                               - 0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0 + obj%gzi(2))*(1.0d0 - obj%gzi(3)) &
                               *(-1.0d0)

            obj%dNdgzi(2, 5) = -0.1250d0*(1.0d0 - obj%gzi(1))*(-1.0d0)*(1.0d0 + obj%gzi(3)) &
                               *(2.0d0 + obj%gzi(1) + obj%gzi(2) - obj%gzi(3)) &
                               - 0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0 - obj%gzi(2))*(1.0d0 + obj%gzi(3)) &
                               *(1.0d0)
            obj%dNdgzi(2, 6) = -0.1250d0*(1.0d0 + obj%gzi(1))*(-1.0d0)*(1.0d0 + obj%gzi(3)) &
                               *(2.0d0 - obj%gzi(1) + obj%gzi(2) - obj%gzi(3)) &
                               - 0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0 - obj%gzi(2))*(1.0d0 + obj%gzi(3)) &
                               *(1.0d0)
            obj%dNdgzi(2, 7) = -0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0)*(1.0d0 + obj%gzi(3)) &
                               *(2.0d0 - obj%gzi(1) - obj%gzi(2) - obj%gzi(3)) &
                               - 0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0 + obj%gzi(2))*(1.0d0 + obj%gzi(3)) &
                               *(-1.0d0)
            obj%dNdgzi(2, 8) = -0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0)*(1.0d0 + obj%gzi(3)) &
                               *(2.0d0 + obj%gzi(1) - obj%gzi(2) - obj%gzi(3)) &
                               - 0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0 + obj%gzi(2))*(1.0d0 + obj%gzi(3)) &
                               *(-1.0d0)

            !<<<ok>>>
            obj%dNdgzi(2, 9) = 0.250d0*(1.0d0 - obj%gzi(1)**2)*(-1.0d0)*(1.0d0 - obj%gzi(3))
            obj%dNdgzi(2, 10) = 0.250d0*(1.0d0 + obj%gzi(1))*(-2.0d0*obj%gzi(2))*(1.0d0 - obj%gzi(3))
            obj%dNdgzi(2, 11) = 0.250d0*(1.0d0 - obj%gzi(1)**2)*(1.0d0)*(1.0d0 - obj%gzi(3))
            obj%dNdgzi(2, 12) = 0.250d0*(1.0d0 - obj%gzi(1))*(-2.0d0*obj%gzi(2))*(1.0d0 - obj%gzi(3))

            obj%dNdgzi(2, 13) = 0.250d0*(1.0d0 - obj%gzi(1)**2)*(-1.0d0)*(1.0d0 + obj%gzi(3))
            obj%dNdgzi(2, 14) = 0.250d0*(1.0d0 + obj%gzi(1))*(-2.0d0*obj%gzi(2))*(1.0d0 + obj%gzi(3))
            obj%dNdgzi(2, 15) = 0.250d0*(1.0d0 - obj%gzi(1)**2)*(1.0d0)*(1.0d0 + obj%gzi(3))
            obj%dNdgzi(2, 16) = 0.250d0*(1.0d0 - obj%gzi(1))*(-2.0d0*obj%gzi(2))*(1.0d0 + obj%gzi(3))

            obj%dNdgzi(2, 17) = 0.250d0*(1.0d0 - obj%gzi(1))*(-1.0d0)*(1.0d0 - obj%gzi(3)**2)
            obj%dNdgzi(2, 18) = 0.250d0*(1.0d0 + obj%gzi(1))*(-1.0d0)*(1.0d0 - obj%gzi(3)**2)
            obj%dNdgzi(2, 19) = 0.250d0*(1.0d0 + obj%gzi(1))*(1.0d0)*(1.0d0 - obj%gzi(3)**2)
            obj%dNdgzi(2, 20) = 0.250d0*(1.0d0 - obj%gzi(1))*(1.0d0)*(1.0d0 - obj%gzi(3)**2)
            !<<<ok>>>

            obj%dNdgzi(3, 1) = -0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0 - obj%gzi(2))*(-1.0d0) &
                               *(2.0d0 + obj%gzi(1) + obj%gzi(2) + obj%gzi(3)) &
                               - 0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0 - obj%gzi(2))*(1.0d0 - obj%gzi(3)) &
                               *(1.0d0)
            obj%dNdgzi(3, 2) = -0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0 - obj%gzi(2))*(-1.0d0) &
                               *(2.0d0 - obj%gzi(1) + obj%gzi(2) + obj%gzi(3)) &
                               - 0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0 - obj%gzi(2))*(1.0d0 - obj%gzi(3)) &
                               *(1.0d0)
            obj%dNdgzi(3, 3) = -0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0 + obj%gzi(2))*(-1.0d0) &
                               *(2.0d0 - obj%gzi(1) - obj%gzi(2) + obj%gzi(3)) &
                               - 0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0 + obj%gzi(2))*(1.0d0 - obj%gzi(3)) &
                               *(1.0d0)
            obj%dNdgzi(3, 4) = -0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0 + obj%gzi(2))*(-1.0d0) &
                               *(2.0d0 + obj%gzi(1) - obj%gzi(2) + obj%gzi(3)) &
                               - 0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0 + obj%gzi(2))*(1.0d0 - obj%gzi(3)) &
                               *(1.0d0)

            obj%dNdgzi(3, 5) = -0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0 - obj%gzi(2))*(1.0d0) &
                               *(2.0d0 + obj%gzi(1) + obj%gzi(2) - obj%gzi(3)) &
                               - 0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0 - obj%gzi(2))*(1.0d0 + obj%gzi(3)) &
                               *(-1.0d0)
            obj%dNdgzi(3, 6) = -0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0 - obj%gzi(2))*(1.0d0) &
                               *(2.0d0 - obj%gzi(1) + obj%gzi(2) - obj%gzi(3)) &
                               - 0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0 - obj%gzi(2))*(1.0d0 + obj%gzi(3)) &
                               *(-1.0d0)
            obj%dNdgzi(3, 7) = -0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0 + obj%gzi(2))*(1.0d0) &
                               *(2.0d0 - obj%gzi(1) - obj%gzi(2) - obj%gzi(3)) &
                               - 0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0 + obj%gzi(2))*(1.0d0 + obj%gzi(3)) &
                               *(-1.0d0)
            obj%dNdgzi(3, 8) = -0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0 + obj%gzi(2))*(1.0d0) &
                               *(2.0d0 + obj%gzi(1) - obj%gzi(2) - obj%gzi(3)) &
                               - 0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0 + obj%gzi(2))*(1.0d0 + obj%gzi(3)) &
                               *(-1.0d0)
            !<<<ok>>>
            obj%dNdgzi(3, 9) = 0.250d0*(1.0d0 - obj%gzi(1)**2)*(1.0d0 - obj%gzi(2))*(-1.0d0)
            obj%dNdgzi(3, 10) = 0.250d0*(1.0d0 + obj%gzi(1))*(1.0d0 - obj%gzi(2)**2)*(-1.0d0)
            obj%dNdgzi(3, 11) = 0.250d0*(1.0d0 - obj%gzi(1)**2)*(1.0d0 + obj%gzi(2))*(-1.0d0)
            obj%dNdgzi(3, 12) = 0.250d0*(1.0d0 - obj%gzi(1))*(1.0d0 - obj%gzi(2)**2)*(-1.0d0)

            obj%dNdgzi(3, 13) = 0.250d0*(1.0d0 - obj%gzi(1)**2)*(1.0d0 - obj%gzi(2))*(1.0d0)
            obj%dNdgzi(3, 14) = 0.250d0*(1.0d0 + obj%gzi(1))*(1.0d0 - obj%gzi(2)**2)*(1.0d0)
            obj%dNdgzi(3, 15) = 0.250d0*(1.0d0 - obj%gzi(1)**2)*(1.0d0 + obj%gzi(2))*(1.0d0)
            obj%dNdgzi(3, 16) = 0.250d0*(1.0d0 - obj%gzi(1))*(1.0d0 - obj%gzi(2)**2)*(1.0d0)

            obj%dNdgzi(3, 17) = 0.250d0*(1.0d0 - obj%gzi(1))*(1.0d0 - obj%gzi(2))*(-2.0d0*obj%gzi(3))
            obj%dNdgzi(3, 18) = 0.250d0*(1.0d0 + obj%gzi(1))*(1.0d0 - obj%gzi(2))*(-2.0d0*obj%gzi(3))
            obj%dNdgzi(3, 19) = 0.250d0*(1.0d0 + obj%gzi(1))*(1.0d0 + obj%gzi(2))*(-2.0d0*obj%gzi(3))
            obj%dNdgzi(3, 20) = 0.250d0*(1.0d0 - obj%gzi(1))*(1.0d0 + obj%gzi(2))*(-2.0d0*obj%gzi(3))

            obj%ErrorMsg = "Succeed::GetShapeFunction "
            obj%ierr = 0
         else
            print *, "ERROR::GetShapeFunction"
            obj%ErrorMsg = "ERROR::GetShapeFunction "
            obj%ierr = 1
         end if

      elseif (obj%NumOfNode == 21) then
         print *, "ERROR::GetShapeFunction"
         obj%ErrorMsg = "ERROR::GetShapeFunction "
         obj%ierr = 1
      elseif (obj%NumOfNode == 22) then
         print *, "ERROR::GetShapeFunction"
         obj%ErrorMsg = "ERROR::GetShapeFunction "
         obj%ierr = 1
      elseif (obj%NumOfNode == 23) then
         print *, "ERROR::GetShapeFunction"
         obj%ErrorMsg = "ERROR::GetShapeFunction "
         obj%ierr = 1
      elseif (obj%NumOfNode == 24) then
         print *, "ERROR::GetShapeFunction"
         obj%ErrorMsg = "ERROR::GetShapeFunction "
         obj%ierr = 1
      else
         print *, "ERROR::GetShapeFunction"
         obj%ErrorMsg = "ERROR::GetShapeFunction "
         obj%ierr = 1
      end if
   end subroutine GetShapeFuncDer1
!!  ######################################

!!  ######################################
   subroutine GetShapeFuncDer2(obj)
      class(ShapeFunction_), intent(inout)::obj

   end subroutine GetShapeFuncDer2
!!  ######################################

!!  ######################################
   subroutine GetElemCoord(obj, nod_coord, elem_nod, elem_id)
      class(ShapeFunction_), intent(inout)::obj
      integer(int32), intent(in)::elem_nod(:, :), elem_id
      real(real64), intent(in)::nod_coord(:, :)
      integer(int32)::i, j, k, n, m

      n = size(elem_nod, 2)
      m = size(nod_coord, 2)

      if (allocated(obj%ElemCoord)) then
         if (n /= size(obj%ElemCoord, 1) .or. m /= size(obj%ElemCoord, 2)) then
            deallocate (obj%ElemCoord)
            allocate (obj%ElemCoord(n, m))
         end if
      else
         allocate (obj%ElemCoord(n, m))
      end if
      do j = 1, n
         obj%ElemCoord(j, 1:m) = nod_coord(elem_nod(elem_id, j), 1:m)
        !! print *, obj%ElemCoord(j,1:m)
      end do

      return

   end subroutine GetElemCoord
!!  ######################################

!!  ######################################
   subroutine GetElemCoord_n(obj, nod_coord_n, elem_nod, elem_id)
      class(ShapeFunction_), intent(inout)::obj
      integer(int32), intent(in)::elem_nod(:, :), elem_id
      real(real64), intent(in)::nod_coord_n(:, :)
      integer(int32)::i, j, k, n, m

      n = size(elem_nod, 2)
      m = size(nod_coord_n, 2)

      if (allocated(obj%ElemCoord_n)) then
         if (n /= size(obj%ElemCoord_n, 1) .or. m /= size(obj%ElemCoord_n, 2)) then
            deallocate (obj%ElemCoord_n)
            allocate (obj%ElemCoord_n(n, m))
         end if
      else
         allocate (obj%ElemCoord_n(n, m))
      end if
      do j = 1, n
         obj%ElemCoord_n(j, 1:m) = nod_coord_n(elem_nod(elem_id, j), 1:m)
        !! print *, obj%ElemCoord_n(j,1:m)
      end do

      return

   end subroutine
!!  ######################################

!!  ######################################
   subroutine getdu(obj)
      class(ShapeFunction_), intent(inout)::obj
      integer(int32)::i, j, k, n, m

      n = size(obj%ElemCoord, 1)
      m = size(obj%ElemCoord, 2)

      if (allocated(obj%du)) then
         if (n /= size(obj%du, 1) .or. m /= size(obj%du, 2)) then
            deallocate (obj%du)
            allocate (obj%du(n, m))
         end if
      else
         allocate (obj%du(n, m))
      end if
      do j = 1, n
         obj%du(j, 1:m) = obj%ElemCoord(j, 1:m) - obj%ElemCoord_n(j, 1:m)
      end do

      return

   end subroutine
!!  ######################################

!!  ######################################
   subroutine GetJmat(obj)
      class(ShapeFunction_), intent(inout)::obj
      integer(int32) n

      n = size(obj%ElemCoord, 2)
      if (allocated(obj%Jmat)) then
         if (n /= size(obj%Jmat, 1) .or. n /= size(obj%Jmat, 2)) then
            deallocate (obj%Jmat)
            allocate (obj%Jmat(n, n))
            allocate (obj%JmatInv(n, n))

         end if
      else
         allocate (obj%Jmat(n, n))
         allocate (obj%JmatInv(n, n))
      end if

      obj%Jmat(:, :) = matmul(obj%dNdgzi, obj%ElemCoord)

      call inverse_rank_2(obj%Jmat, obj%JmatInv)

   end subroutine GetJmat
!!  ######################################

   subroutine exportShapeFunction(obj, restart, path)
      class(ShapeFunction_), intent(inout) :: obj
      logical, optional, intent(in) :: restart
      character(*), intent(in) :: path
      type(IO_) :: f

      if (present(restart)) then
         call execute_command_line("mkdir -p "//path//"/ShapeFunction")
         call f%open(path//"/ShapeFunction/", "ShapeFunction", ".res")
         write (f%fh, *) obj%Nmat(:)
         write (f%fh, *) obj%dNdgzi(:, :)
         write (f%fh, *) obj%dNdgzidgzi(:, :)
         write (f%fh, *) obj%gzi(:)
         write (f%fh, *) obj%gaussPoint(:, :)
         write (f%fh, *) obj%gaussIntegWei(:)
         write (f%fh, *) obj%Jmat(:, :)
         write (f%fh, *) obj%JmatInv(:, :)
         write (f%fh, *) obj%ElemCoord(:, :)
         write (f%fh, *) obj%ElemCoord_n(:, :)
         write (f%fh, *) obj%du(:, :)
         write (f%fh, *) obj%detJ
         write (f%fh, *) obj%NumOfNode
         write (f%fh, *) obj%NumOfOrder
         write (f%fh, *) obj%NumOfDim
         write (f%fh, *) obj%NumOfGp
         write (f%fh, *) obj%GpID
         write (f%fh, *) obj%ierr
         write (f%fh, *) obj%currentGpID
         write (f%fh, *) obj%ReducedIntegration
         write (f%fh, '(A)') obj%ElemType
         write (f%fh, '(A)') obj%ErrorMsg
         call f%close()
      end if

   end subroutine

end module ShapeFunctionClass
