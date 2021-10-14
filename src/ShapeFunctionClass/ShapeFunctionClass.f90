module ShapeFunctionClass
    use, intrinsic :: iso_fortran_env
    use MathClass
    use ArrayClass
    use IOClass
    implicit none
    
    type::ShapeFunction_
        
        real(real64),allocatable::Nmat(:)
        real(real64),allocatable::dNdgzi(:,:)
        real(real64),allocatable::dNdgzidgzi(:,:)
        real(real64),allocatable::gzi(:)
        real(real64),allocatable::GaussPoint(:,:)
        real(real64),allocatable::GaussIntegWei(:)
        real(real64),allocatable::Jmat(:,:),JmatInv(:,:)
        real(real64),allocatable::ElemCoord(:,:)
        real(real64),allocatable::ElemCoord_n(:,:)
        real(real64),allocatable::du(:,:)
        real(real64) :: detJ
        integer(int32) :: NumOfNode
        integer(int32) :: NumOfOrder
        integer(int32) :: NumOfDim=0
        integer(int32) :: NumOfGp=0
        integer(int32) :: GpID
        integer(int32) :: ierr
        integer(int32) :: currentGpID
        integer(int32) :: ElementID
        logical :: ReducedIntegration = .false.
        logical :: Empty=.true.
        character*70::ElemType
        character(len=60):: ErrorMsg
    contains
        procedure :: init => initShapeFunction
        procedure :: update => updateShapeFunction
        procedure :: SetType => SetShapeFuncType
        procedure :: GetAll  => GetAllShapeFunc
        procedure :: get  => GetAllShapeFunc
        procedure :: getOnlyNvec  => GetShapeFunction
        procedure :: Deallocate => DeallocateShapeFunction
        procedure :: getType => getShapeFuncType 
        procedure :: GetGaussPoint => GetGaussPoint
        procedure :: export => exportShapeFunction
        procedure :: remove => removeShapeFunction
        procedure :: save => saveShapeFunction
        procedure :: open => openShapeFunction
        procedure :: getNvec => getNvecShapeFunction
    end type ShapeFunction_

contains

!!  #####################################################
function getNvecShapeFunction(obj,x,y,z) result(nvec)
    class(Shapefunction_),intent(in) :: obj
    real(real64),optional,intent(in)::x,y,z
    real(real64),allocatable :: nvec(:)

    
end function
!!  #####################################################

!!  #####################################################
subroutine openShapeFunction(obj,path,name)
    class(ShapeFunction_ ),intent(inout)::obj
    character(*),intent(in) :: path
    character(*),optional,intent(in) :: name
    type(IO_) :: f

    if(present(name) )then
        call execute_command_line("mkdir -p "//trim(path)//"/"//trim(adjustl(name)))
        
        call f%open(trim(path)//"/"//trim(adjustl(name))//"/","ShapeFunction","prop" )
        
        call openArray(f%fh,obj%Nmat)
        call openArray(f%fh,obj%dNdgzi)
        call openArray(f%fh,obj%dNdgzidgzi)
        call openArray(f%fh,obj%gzi)
        call openArray(f%fh,obj%GaussPoint)
        call openArray(f%fh,obj%GaussIntegWei)
        call openArray(f%fh,obj%Jmat)
        call openArray(f%fh,obj%JmatInv)
        call openArray(f%fh,obj%ElemCoord)
        call openArray(f%fh,obj%ElemCoord_n)
        call openArray(f%fh,obj%du)
        read(f%fh,*)obj%detJ
        read(f%fh,*)obj%NumOfNode
        read(f%fh,*)obj%NumOfOrder
        read(f%fh,*)obj%NumOfDim
        read(f%fh,*)obj%NumOfGp
        read(f%fh,*)obj%GpID
        read(f%fh,*)obj%ierr
        read(f%fh,*)obj%currentGpID
        read(f%fh,*)obj%ReducedIntegration
        read(f%fh,*)obj%ElemType
        read(f%fh,*)obj%ErrorMsg
        
        call f%close()
    else

        call execute_command_line("mkdir -p "//trim(path)//"/ShapeFunction")
        call f%open(trim(path)//"/","ShapeFunction/ShapeFunction","prop" )
        
        call openArray(f%fh,obj%Nmat)
        call openArray(f%fh,obj%dNdgzi)
        call openArray(f%fh,obj%dNdgzidgzi)
        call openArray(f%fh,obj%gzi)
        call openArray(f%fh,obj%GaussPoint)
        call openArray(f%fh,obj%GaussIntegWei)
        call openArray(f%fh,obj%Jmat)
        call openArray(f%fh,obj%JmatInv)
        call openArray(f%fh,obj%ElemCoord)
        call openArray(f%fh,obj%ElemCoord_n)
        call openArray(f%fh,obj%du)
        read(f%fh,*)obj%detJ
        read(f%fh,*)obj%NumOfNode
        read(f%fh,*)obj%NumOfOrder
        read(f%fh,*)obj%NumOfDim
        read(f%fh,*)obj%NumOfGp
        read(f%fh,*)obj%GpID
        read(f%fh,*)obj%ierr
        read(f%fh,*)obj%currentGpID
        read(f%fh,*)obj%ReducedIntegration
        read(f%fh,*)obj%ElemType
        read(f%fh,*)obj%ErrorMsg
        
        call f%close()
    endif

end subroutine
!!  #####################################################


!!  #####################################################
subroutine saveShapeFunction(obj,path,name)
    class(ShapeFunction_ ),intent(inout)::obj
    character(*),intent(in) :: path
    character(*),optional,intent(in) :: name
    type(IO_) :: f

    if(present(name) )then
        call execute_command_line("mkdir -p "//trim(path)//"/"//trim(adjustl(name)))
        
        call f%open(trim(path)//"/"//trim(adjustl(name))//"/","ShapeFunction","prop" )
        
        call writeArray(f%fh,obj%Nmat)
        call writeArray(f%fh,obj%dNdgzi)
        call writeArray(f%fh,obj%dNdgzidgzi)
        call writeArray(f%fh,obj%gzi)
        call writeArray(f%fh,obj%GaussPoint)
        call writeArray(f%fh,obj%GaussIntegWei)
        call writeArray(f%fh,obj%Jmat)
        call writeArray(f%fh,obj%JmatInv)
        call writeArray(f%fh,obj%ElemCoord)
        call writeArray(f%fh,obj%ElemCoord_n)
        call writeArray(f%fh,obj%du)
        write(f%fh,*)obj%detJ
        write(f%fh,*)obj%NumOfNode
        write(f%fh,*)obj%NumOfOrder
        write(f%fh,*)obj%NumOfDim
        write(f%fh,*)obj%NumOfGp
        write(f%fh,*)obj%GpID
        write(f%fh,*)obj%ierr
        write(f%fh,*)obj%currentGpID
        write(f%fh,*)obj%ReducedIntegration
        write(f%fh,*)trim(obj%ElemType)
        write(f%fh,*)trim(obj%ErrorMsg)
        
        call f%close()
    else

        call execute_command_line("mkdir -p "//trim(path)//"/ShapeFunction")
        call f%open(trim(path)//"/","ShapeFunction/ShapeFunction","prop" )
        
        call writeArray(f%fh,obj%Nmat)
        call writeArray(f%fh,obj%dNdgzi)
        call writeArray(f%fh,obj%dNdgzidgzi)
        call writeArray(f%fh,obj%gzi)
        call writeArray(f%fh,obj%GaussPoint)
        call writeArray(f%fh,obj%GaussIntegWei)
        call writeArray(f%fh,obj%Jmat)
        call writeArray(f%fh,obj%JmatInv)
        call writeArray(f%fh,obj%ElemCoord)
        call writeArray(f%fh,obj%ElemCoord_n)
        call writeArray(f%fh,obj%du)
        write(f%fh,*) obj%detJ
        write(f%fh,*) obj%NumOfNode
        write(f%fh,*) obj%NumOfOrder
        write(f%fh,*) obj%NumOfDim
        write(f%fh,*) obj%NumOfGp
        write(f%fh,*) obj%GpID
        write(f%fh,*) obj%ierr
        write(f%fh,*) obj%currentGpID
        write(f%fh,*) obj%ReducedIntegration
        write(f%fh,*) trim(obj%ElemType)
        write(f%fh,*) trim(obj%ErrorMsg)
        
        call f%close()
    endif

end subroutine
!!  #####################################################

subroutine removeShapeFunction(obj)
    class(ShapeFunction_),intent(inout)::obj

    if(allocated(obj%Nmat))then
        deallocate(obj%Nmat)
    endif
    if(allocated(obj%dNdgzi))then
        deallocate(obj%dNdgzi)
    endif
    if(allocated(obj%dNdgzidgzi))then
        deallocate(obj%dNdgzidgzi)
    endif
    if(allocated(obj%gzi))then
        deallocate(obj%gzi)
    endif
    if(allocated(obj%GaussPoint))then
        deallocate(obj%GaussPoint)
    endif
    if(allocated(obj%GaussIntegWei))then
        deallocate(obj%GaussIntegWei)
    endif
    if(allocated(obj%Jmat))then
        deallocate(obj%Jmat)
    endif
    if(allocated(obj%ElemCoord))then
        deallocate(obj%ElemCoord)
    endif
    if(allocated(obj%ElemCoord_n))then
        deallocate(obj%ElemCoord_n)
    endif
    if(allocated(obj%du))then
        deallocate(obj%du)
    endif
    obj%detJ=0.0d0
    obj%NumOfNode=0
    obj%NumOfOrder=0
    obj%NumOfDim=0
    obj%NumOfGp=0
    obj%GpID=0
    obj%ierr=0
    obj%currentGpID=0
    obj%ReducedIntegration = .false.
    obj%ElemType=" "
    obj%ErrorMsg=" "

end subroutine

!!  ##################################################
subroutine initShapeFunction(obj,ElemType)
    class(ShapeFunction_),intent(inout) :: obj
    character(*),intent(in),optional :: ElemType

    obj%ElemType = ElemType

    call obj%SetType()

end subroutine
!!  ##################################################


!!  ##################################################
subroutine updateShapeFunction(obj,ElemType,NodCoord,ElemNod,ElemID,GpID)
    class(ShapeFunction_),intent(inout) :: obj
    character(*),optional,intent(in) :: ElemType
    integer(int32),intent(in) ::ElemNod(:,:),ElemID,GpID
    real(real64),intent(in) ::NodCoord(:,:)
    integer(int32) :: i,j

    if(present(ElemType) )then
        call obj%init(ElemType)
    endif
    call obj%GetAll(elem_id=i,nod_coord=NodCoord,elem_nod=ElemNod,OptionalGpID=j)

end subroutine
!!  ##################################################


!!  ##################################################
subroutine SetShapeFuncType(obj,NumOfDim,NumOfNodePerElem,ReducedIntegration)
    class(ShapeFunction_),intent(inout)::obj
    logical,optional,intent(in) :: ReducedIntegration
    character*70 ::  TrimedElemType
    integer(int32),optional,intent(in) :: NumOfDim,NumOfNodePerElem

    if(present(NumOfDim) )then
        if(present(NumOfNodePerElem) )then
            call obj%getType(NumOfDim,NumOfNodePerElem)
        endif
    endif
    
    if(present(ReducedIntegration) )then
        obj%ReducedIntegration=ReducedIntegration
    endif
    
    TrimedElemType=trim(obj%ElemType)
    if(trim(TrimedElemType)=="LinearRectangularGp4")then

        obj%NumOfNode   = 4
        obj%NumOfOrder  = 1
        obj%NumOfDim    = 2
        obj%NumOfGp     = 4
        

    elseif(trim(TrimedElemType)=="LinearHexahedralGp8")then
        obj%NumOfNode   = 8
        obj%NumOfOrder  = 1
        obj%NumOfDim    = 3
        obj%NumOfGp     = 8
        
    elseif(trim(TrimedElemType)=="Triangular")then
        obj%NumOfNode   = 3
        obj%NumOfOrder  = 1
        obj%NumOfDim    = 2
        obj%NumOfGp     = 1
    elseif(trim(TrimedElemType)=="LinearTetrahedral")then
        obj%NumOfNode   = 4
        obj%NumOfOrder  = 1
        obj%NumOfDim    = 3
        obj%NumOfGp     = 1
    else
        obj%ErrorMsg="ShapeFunctionClass.f90 :: SetShapeFuncType >> Element : "//TrimedElemType//"is not defined."
        print *, "ShapeFunctionClass.f90 :: SetShapeFuncType >> Element : ",TrimedElemType,"is not defined."
        return
    endif

    

end subroutine SetShapeFuncType
!!  ##################################################

!!  ##################################################
subroutine getShapeFuncType(obj,NumOfDim,NumOfNodePerElem)
    class(ShapeFunction_),intent(inout)::obj
    character*70 ::  TrimedElemType
    integer(int32),intent(in) :: NumOfDim,NumOfNodePerElem

    if(NumOfDim==2)then
        if(NumOfNodePerElem==4)then
            obj%ElemType="LinearRectangularGp4"
        else
            print *, "For 2D, NumOfNodePerElem = ",NumOfNodePerElem," is not set."
        endif
    elseif(NumOfDim==3)then
        if(NumOfNodePerElem==8)then
            obj%ElemType="LinearHexahedralGp8"
        else
            print *, "For 3D, NumOfNodePerElem = ",NumOfNodePerElem," is not set."
        endif
    else
        print *, "NumOfDim = ",NumOfDim," is not set."
    endif
    

end subroutine getShapeFuncType
!!  ##################################################

!!  ##################################################
subroutine GetAllShapeFunc(obj,elem_id,nod_coord,nod_coord_n,elem_nod,OptionalNumOfNode,OptionalNumOfOrder,&
    OptionalNumOfDim,OptionalNumOfGp,OptionalGpID,ReducedIntegration,NumOfDim,NumOfNodePerElem)
    class(ShapeFunction_),intent(inout)::obj
    integer(int32),optional,intent(in)::elem_nod(:,:),elem_id
    integer(int32),optional,intent(in) :: NumOfDim,NumOfNodePerElem
    real(real64),optional,intent(in)::nod_coord(:,:),nod_coord_n(:,:)
    integer(int32),optional,intent(in)::OptionalNumOfNode,OptionalNumOfOrder,&
    OptionalNumOfDim,OptionalNumOfGp,OptionalGpID
    logical,optional,intent(in) :: ReducedIntegration
    integer(int32) :: nd, ne

    nd = input(default=0, option=NumOfDim )
    ne = input(default=0, option=NumOfNodePerElem )
    if(present(nod_coord) )then
        nd = size(nod_coord,2)
    endif
    if(present(nod_coord) )then
        ne = size(elem_nod,2)
    endif

    if(obj%NumOfGp==0 .and. obj%NumOfDim==0)then
        
        if(nd==0 .or. ne==0)then
            print *, "ERROR :: GetAllShapeFunc >> please import NumOfDim and NumOfNodePerElem"
            stop
        endif
        call obj%SetType(NumOfDim=nd,NumOfNodePerElem=ne,ReducedIntegration=ReducedIntegration)
    endif
    if(present(ReducedIntegration) )then
        obj%ReducedIntegration=ReducedIntegration
    endif

    if(present(OptionalNumOfNode ) )then
        obj%NumOfNode=OptionalNumOfNode
    endif

    if(present(OptionalNumOfOrder ) )then
        obj%NumOfOrder=OptionalNumOfOrder
    endif

    if(present(OptionalNumOfDim ) )then
        obj%NumOfDim=OptionalNumOfDim
    endif

    if(present(OptionalNumOfGp ) )then
        obj%NumOfGp=OptionalNumOfGp
    endif
    
    if(present(OptionalGpID ) )then
        obj%GpID=OptionalGpID
    endif


    call GetGaussPoint(obj)
    call SetGaussPoint(obj)



    call GetShapeFunction(obj)
    call GetShapeFuncDer1(obj)
    !! call GetShapeFuncDer2(obj)



    if(.not.present(nod_coord) .or. .not.present(elem_nod) )then
        
        obj%ErrorMsg="Mesh%NodCoord and Mesh%ElemNod is necessary to get Jmat"
        print *, obj%ErrorMsg
        
        return
    endif
    call GetElemCoord(obj,nod_coord,elem_nod,elem_id)
    if(present(nod_coord_n) )then
        call GetElemCoord_n(obj,nod_coord_n,elem_nod,elem_id)
        call Getdu(obj)
    endif



    call GetJmat(obj)
    obj%detJ = det_mat(obj%Jmat,size(obj%Jmat,1 ) )

end subroutine GetAllShapeFunc
!!  ##################################################


!!  ##################################################
subroutine DeallocateShapeFunction(obj)
    class(ShapeFunction_),intent(inout)::obj

    if( allocated(obj%Nmat            ) ) deallocate(obj%Nmat             )
    if( allocated(obj%dNdgzi          ) ) deallocate(obj%dNdgzi           )
    if( allocated(obj%dNdgzidgzi      ) ) deallocate(obj%dNdgzidgzi       )
    if( allocated(obj%gzi             ) ) deallocate(obj%gzi              )
    if( allocated(obj%GaussPoint      ) ) deallocate(obj%GaussPoint       )
    if( allocated(obj%GaussIntegWei   ) ) deallocate(obj%GaussIntegWei    )
    if( allocated(obj%Jmat            ) ) deallocate(obj%Jmat           )
    
    obj%ErrorMsg="All allocatable entities are deallocated"
end subroutine DeallocateShapeFunction
!!  ##################################################

!!  ######################################
    subroutine GetGaussPoint(obj)

        class(ShapeFunction_),intent(inout)::obj

        !! include "./GetGaussPoint.f90"

        if(.not.allocated(obj%GaussPoint) )then
            allocate(obj%GaussPoint(obj%NumOfDim,obj%NumOfGp ) )
        else
            if(size(obj%GaussPoint,1)/=obj%NumOfDim .or.&
            size(obj%GaussPoint,2)/=obj%NumOfGp)then
                deallocate(obj%GaussPoint)
                allocate(obj%GaussPoint(obj%NumOfDim,obj%NumOfGp ) )
            endif
        endif

        if(.not.allocated(obj%GaussIntegWei) )then
            allocate(obj%GaussIntegWei(obj%NumOfGp ) )
        else
            if(size(obj%GaussIntegWei)/=obj%NumOfGp)then
                deallocate(obj%GaussIntegWei)
                allocate(obj%GaussIntegWei(obj%NumOfGp ) )
            endif
        endif

        if(size(obj%GaussPoint,1)==2 .and. size(obj%GaussPoint,2)==4)then
            obj%GaussPoint(1,1) = -0.57735026918962576d0
            obj%GaussPoint(1,2) =  0.57735026918962576d0
            obj%GaussPoint(1,3) =  0.57735026918962576d0
            obj%GaussPoint(1,4) = -0.57735026918962576d0

            obj%GaussPoint(2,1) = -0.57735026918962576d0
            obj%GaussPoint(2,2) = -0.57735026918962576d0
            obj%GaussPoint(2,3) =  0.57735026918962576d0
            obj%GaussPoint(2,4) =  0.57735026918962576d0
            obj%GaussIntegWei(:)=1.0d0
            if(obj%ReducedIntegration .eqv. .true.)then
                obj%GaussPoint(:,:) =0.0d0
                obj%GaussIntegWei(:)=0.250d0
            endif
        elseif(size(obj%GaussPoint,1)==3 .and. size(obj%GaussPoint,2)==8)then
            obj%GaussPoint(1,1) = -0.57735026918962576d0
            obj%GaussPoint(1,2) =  0.57735026918962576d0
            obj%GaussPoint(1,3) =  0.57735026918962576d0
            obj%GaussPoint(1,4) = -0.57735026918962576d0
            obj%GaussPoint(1,5) = -0.57735026918962576d0
            obj%GaussPoint(1,6) =  0.57735026918962576d0
            obj%GaussPoint(1,7) =  0.57735026918962576d0
            obj%GaussPoint(1,8) = -0.57735026918962576d0

            obj%GaussPoint(2,1) = -0.57735026918962576d0
            obj%GaussPoint(2,2) = -0.57735026918962576d0
            obj%GaussPoint(2,3) =  0.57735026918962576d0
            obj%GaussPoint(2,4) =  0.57735026918962576d0
            obj%GaussPoint(2,5) = -0.57735026918962576d0
            obj%GaussPoint(2,6) = -0.57735026918962576d0
            obj%GaussPoint(2,7) =  0.57735026918962576d0
            obj%GaussPoint(2,8) =  0.57735026918962576d0

            obj%GaussPoint(3,1) = -0.57735026918962576d0
            obj%GaussPoint(3,2) = -0.57735026918962576d0
            obj%GaussPoint(3,3) = -0.57735026918962576d0
            obj%GaussPoint(3,4) = -0.57735026918962576d0
            obj%GaussPoint(3,5) =  0.57735026918962576d0
            obj%GaussPoint(3,6) =  0.57735026918962576d0
            obj%GaussPoint(3,7) =  0.57735026918962576d0
            obj%GaussPoint(3,8) =  0.57735026918962576d0
            obj%GaussIntegWei(:)=1.0d0
            
            if(obj%ReducedIntegration .eqv. .true.)then
                obj%GaussPoint(:,:) =0.0d0
                obj%GaussIntegWei(:)=0.1250d0
            endif

        elseif(size(obj%GaussPoint,2)==1 )then
            !!  Triangular or Tetrahedral
            if(allocated(obj%GaussPoint))then
                deallocate(obj%GaussPoint)
            endif
            if(allocated(obj%GaussIntegWei))then
                deallocate(obj%GaussIntegWei)
            endif
            
            obj%GaussPoint(1,1) = 1.0d0
            obj%GaussPoint(2,1) = 1.0d0
            obj%GaussPoint(3,1) = 1.0d0
            obj%GaussIntegWei(:)= 1.0d0
        else
            print *, "ERROR :: ShapeFunctionClass/GetGaussPoint is not defined"
        endif


    end subroutine GetGaussPoint
!!  ######################################

!!  ######################################
    subroutine SetGaussPoint(obj)

        class(ShapeFunction_),intent(inout)::obj
        if(allocated(obj%gzi) ) then
            if(size(obj%gzi,1)/=obj%NumOfDim )then
            deallocate(obj%gzi)
                allocate(obj%gzi(obj%NumOfDim) )
            endif
        else
            allocate(obj%gzi(obj%NumOfDim) )
        endif
        
        
        if(obj%NumOfDim /= size(obj%GaussPoint,1) )then
            print *, "ERROR::SetGaussPoint",obj%NumOfDim, size(obj%GaussPoint,1)
            obj%ErrorMsg="ERROR::SetGaussPoint"
            obj%ierr=1
        else
            obj%gzi(:) = obj%GaussPoint(:,obj%GpID )
            
            obj%ErrorMsg="Succeed::SetGaussPoint"
            obj%ierr=0
        endif
        


    end subroutine SetGaussPoint
!!  ######################################


!!  ######################################
    subroutine GetShapeFunction(obj)
        class(ShapeFunction_),intent(inout)::obj

        
if(allocated(obj%Nmat) ) then
    if(size(obj%Nmat,1)/=obj%NumOfNode)then
        deallocate(obj%Nmat)
        allocate(obj%Nmat(obj%NumOfNode) )
    endif
else
    allocate(obj%Nmat(obj%NumOfNode) )
endif


if(obj%NumOfNode==1) then
    !!  #########################################################################
    !!  #######                                                           #######
    !!  #######                             +     (1)                     #######
    !!  #######                                                           #######
    !!  #######                                                           #######
    !!  #########################################################################
    print *, "ERROR::GetShapeFunction"
    obj%ErrorMsg="ERROR::GetShapeFunction NumOfNode==1 "
    obj%ierr=1

elseif(obj%NumOfNode==2) then
    
    if(obj%NumOfDim==1) then
        
        !!  #########################################################################
        !!  #######                                                           #######
        !!  #######                +-----------------------+                #######
        !!  #######            (1)                               (2)          #######
        !!  #######                                                           #######
        !!  #########################################################################
        
        obj%Nmat(1)=0.50d0*( 1.0d0-obj%gzi(1))
        obj%Nmat(2)=0.50d0*(-1.0d0+obj%gzi(1))
        
        obj%ErrorMsg="Succeed::GetShapeFunction "
        obj%ierr=0
    else
        print *, "ERROR::GetShapeFunction"
        obj%ErrorMsg="ERROR::GetShapeFunction NumOfNode==2 NumOfOrder/=1 "
        obj%ierr=1
    endif
elseif(obj%NumOfNode==3) then
    if(obj%NumOfDim==1) then
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
        obj%ErrorMsg="ERROR::GetShapeFunction "
        obj%ierr=1
    elseif(obj%NumOfDim==2) then
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
        obj%ErrorMsg="ERROR::GetShapeFunction "
        obj%ierr=1
    else

        print *, "ERROR::GetShapeFunction"
        obj%ErrorMsg="ERROR::GetShapeFunction "
        obj%ierr=1
    endif
elseif(obj%NumOfNode==4) then
    if(obj%NumOfDim==1) then
        !!  #########################################################################
        !!  #######                                                           #######
        !!  #######                +-------+---------+-------+                #######
        !!  #######          (1)          (2)        (3)       (4)            #######
        !!  #######                                                           #######
        !!  #########################################################################
        !! obj%ErrorMsg="Succeed::GetShapeFunction "
        !! obj%ierr=0
        print *, "ERROR::GetShapeFunction"
        obj%ErrorMsg="ERROR::GetShapeFunction "
        obj%ierr=1
    elseif(obj%NumOfDim==2) then
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
        
		obj%Nmat(1)=0.250d0*(1.0d0-obj%gzi(1))*(1.0d0-obj%gzi(2))
	    obj%Nmat(2)=0.250d0*(1.0d0+obj%gzi(1))*(1.0d0-obj%gzi(2))
		obj%Nmat(3)=0.250d0*(1.0d0+obj%gzi(1))*(1.0d0+obj%gzi(2))
		obj%Nmat(4)=0.250d0*(1.0d0-obj%gzi(1))*(1.0d0+obj%gzi(2))
	  
        obj%ErrorMsg="Succeed::GetShapeFunction "
        obj%ierr=0
    elseif(obj%NumOfDim==3) then
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
        obj%ErrorMsg="ERROR::GetShapeFunction "
        obj%ierr=1
    else
        print *, "ERROR::GetShapeFunction"
        obj%ErrorMsg="ERROR::GetShapeFunction "
        obj%ierr=1
    endif
    
    
elseif(obj%NumOfNode==5) then
    print *, "ERROR::GetShapeFunction"
    obj%ErrorMsg="ERROR::GetShapeFunction "
    obj%ierr=1
elseif(obj%NumOfNode==6) then
    print *, "ERROR::GetShapeFunction"
    obj%ErrorMsg="ERROR::GetShapeFunction "
    obj%ierr=1
elseif(obj%NumOfNode==7) then
    print *, "ERROR::GetShapeFunction"
    obj%ErrorMsg="ERROR::GetShapeFunction "
    obj%ierr=1
elseif(obj%NumOfNode==8) then
    if(obj%NumOfDim==3) then
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
        
        obj%Nmat(1)=0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0 - obj%gzi(2))*(1.0d0 - obj%gzi(3))
	    obj%Nmat(2)=0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0 - obj%gzi(2))*(1.0d0 - obj%gzi(3))
		obj%Nmat(3)=0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0 + obj%gzi(2))*(1.0d0 - obj%gzi(3))
		obj%Nmat(4)=0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0 + obj%gzi(2))*(1.0d0 - obj%gzi(3))
        obj%Nmat(5)=0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0 - obj%gzi(2))*(1.0d0 + obj%gzi(3))
	    obj%Nmat(6)=0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0 - obj%gzi(2))*(1.0d0 + obj%gzi(3))
		obj%Nmat(7)=0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0 + obj%gzi(2))*(1.0d0 + obj%gzi(3))
		obj%Nmat(8)=0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0 + obj%gzi(2))*(1.0d0 + obj%gzi(3))
	  

        obj%ErrorMsg="Succeed::GetShapeFunction "
        obj%ierr=0
    else
        print *, "ERROR::GetShapeFunction"
        obj%ErrorMsg="ERROR::GetShapeFunction "
        obj%ierr=1
    endif
    
elseif(obj%NumOfNode==9) then
    print *, "ERROR::GetShapeFunction"
    obj%ErrorMsg="ERROR::GetShapeFunction "
    obj%ierr=1
elseif(obj%NumOfNode==10) then
    print *, "ERROR::GetShapeFunction"
    obj%ErrorMsg="ERROR::GetShapeFunction "
    obj%ierr=1
elseif(obj%NumOfNode==11) then
    print *, "ERROR::GetShapeFunction"
    obj%ErrorMsg="ERROR::GetShapeFunction "
    obj%ierr=1
elseif(obj%NumOfNode==12) then
    print *, "ERROR::GetShapeFunction"
    obj%ErrorMsg="ERROR::GetShapeFunction "
    obj%ierr=1
elseif(obj%NumOfNode==13) then
    print *, "ERROR::GetShapeFunction"
    obj%ErrorMsg="ERROR::GetShapeFunction "
    obj%ierr=1
elseif(obj%NumOfNode==14) then
    print *, "ERROR::GetShapeFunction"
    obj%ErrorMsg="ERROR::GetShapeFunction "
    obj%ierr=1
elseif(obj%NumOfNode==15) then
    print *, "ERROR::GetShapeFunction"
    obj%ErrorMsg="ERROR::GetShapeFunction "
    obj%ierr=1
elseif(obj%NumOfNode==16) then
    print *, "ERROR::GetShapeFunction"
    obj%ErrorMsg="ERROR::GetShapeFunction "
    obj%ierr=1
elseif(obj%NumOfNode==17) then
    print *, "ERROR::GetShapeFunction"
    obj%ErrorMsg="ERROR::GetShapeFunction "
    obj%ierr=1
elseif(obj%NumOfNode==18) then
    print *, "ERROR::GetShapeFunction"
    obj%ErrorMsg="ERROR::GetShapeFunction "
    obj%ierr=1
elseif(obj%NumOfNode==19) then
    print *, "ERROR::GetShapeFunction"
    obj%ErrorMsg="ERROR::GetShapeFunction "
    obj%ierr=1
elseif(obj%NumOfNode==20) then
    print *, "ERROR::GetShapeFunction"
    obj%ErrorMsg="ERROR::GetShapeFunction "
    obj%ierr=1
elseif(obj%NumOfNode==21) then
    print *, "ERROR::GetShapeFunction"
    obj%ErrorMsg="ERROR::GetShapeFunction "
    obj%ierr=1
elseif(obj%NumOfNode==22) then
    print *, "ERROR::GetShapeFunction"
    obj%ErrorMsg="ERROR::GetShapeFunction "
    obj%ierr=1
elseif(obj%NumOfNode==23) then
    print *, "ERROR::GetShapeFunction"
    obj%ErrorMsg="ERROR::GetShapeFunction "
    obj%ierr=1
elseif(obj%NumOfNode==24) then
    print *, "ERROR::GetShapeFunction"
    obj%ErrorMsg="ERROR::GetShapeFunction "
    obj%ierr=1
else
    print *, "ERROR::GetShapeFunction"
    obj%ErrorMsg="ERROR::GetShapeFunction "
    obj%ierr=1
endif

    end subroutine GetShapeFunction
!!  ######################################

!!  ######################################
    subroutine GetShapeFuncDer1(obj)
        class(ShapeFunction_),intent(inout)::obj

        

        if(allocated(obj%dNdgzi) ) then
            if(size(obj%dNdgzi,1)/=obj%NumOfDim .or. size(obj%dNdgzi,2)/=obj%NumOfNode)then
                deallocate(obj%dNdgzi)
                allocate(obj%dNdgzi(obj%NumOfDim,obj%NumOfNode) )
            endif
        else
            allocate(obj%dNdgzi(obj%NumOfDim,obj%NumOfNode) )
        endif
        
        
        
        if(obj%NumOfNode==1) then
            !!  #########################################################################
            !!  #######                                                           #######
            !!  #######                             +     (1)                     #######
            !!  #######                                                           #######
            !!  #######                                                           #######
            !!  #########################################################################
            print *, "ERROR::GetShapeFunction"
            obj%ErrorMsg="ERROR::GetShapeFunction NumOfNode==1 "
            obj%ierr=1
        
        elseif(obj%NumOfNode==2) then
            
            if(obj%NumOfDim==1) then
                
                !!  #########################################################################
                !!  #######                                                           #######
                !!  #######                +-----------------------+                #######
                !!  #######            (1)                               (2)          #######
                !!  #######                                                           #######
                !!  #########################################################################
                
                obj%dNdgzi(1,1)= -0.50d0
                obj%dNdgzi(1,2)=  0.50d0
                
                obj%ErrorMsg="Succeed::GetShapeFunction "
                obj%ierr=0
            else
                print *, "ERROR::GetShapeFunction"
                obj%ErrorMsg="ERROR::GetShapeFunction NumOfNode==2 NumOfOrder/=1 "
                obj%ierr=1
            endif
        elseif(obj%NumOfNode==3) then
            if(obj%NumOfDim==1) then
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
                obj%ErrorMsg="ERROR::GetShapeFunction "
                obj%ierr=1
            elseif(obj%NumOfDim==2) then
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
                obj%ErrorMsg="ERROR::GetShapeFunction "
                obj%ierr=1
            else
        
                print *, "ERROR::GetShapeFunction"
                obj%ErrorMsg="ERROR::GetShapeFunction "
                obj%ierr=1
            endif
        elseif(obj%NumOfNode==4) then
            if(obj%NumOfDim==1) then
                !!  #########################################################################
                !!  #######                                                           #######
                !!  #######                +-------+---------+-------+                #######
                !!  #######          (1)          (2)        (3)       (4)            #######
                !!  #######                                                           #######
                !!  #########################################################################
                !! obj%ErrorMsg="Succeed::GetShapeFunction "
                !! obj%ierr=0
                print *, "ERROR::GetShapeFunction"
                obj%ErrorMsg="ERROR::GetShapeFunction "
                obj%ierr=1
            elseif(obj%NumOfDim==2) then
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
                
                obj%dNdgzi(1,1) = -0.250d0*(1.0d0-obj%gzi(2))
                obj%dNdgzi(1,2) =  0.250d0*(1.0d0-obj%gzi(2))
                obj%dNdgzi(1,3) =  0.250d0*(1.0d0+obj%gzi(2))
                obj%dNdgzi(1,4) = -0.250d0*(1.0d0+obj%gzi(2))
              
                obj%dNdgzi(2,1) = -0.250d0*(1.0d0-obj%gzi(1))
                obj%dNdgzi(2,2) = -0.250d0*(1.0d0+obj%gzi(1))
                obj%dNdgzi(2,3) =  0.250d0*(1.0d0+obj%gzi(1))
                obj%dNdgzi(2,4) =  0.250d0*(1.0d0-obj%gzi(1))
              
                obj%ErrorMsg="Succeed::GetShapeFunction "
                obj%ierr=0
            elseif(obj%NumOfDim==3) then
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
                obj%ErrorMsg="ERROR::GetShapeFunction "
                obj%ierr=1
            else
                print *, "ERROR::GetShapeFunction"
                obj%ErrorMsg="ERROR::GetShapeFunction "
                obj%ierr=1
            endif
            
            
        elseif(obj%NumOfNode==5) then
            print *, "ERROR::GetShapeFunction"
            obj%ErrorMsg="ERROR::GetShapeFunction "
            obj%ierr=1
        elseif(obj%NumOfNode==6) then
            print *, "ERROR::GetShapeFunction"
            obj%ErrorMsg="ERROR::GetShapeFunction "
            obj%ierr=1
        elseif(obj%NumOfNode==7) then
            print *, "ERROR::GetShapeFunction"
            obj%ErrorMsg="ERROR::GetShapeFunction "
            obj%ierr=1
        elseif(obj%NumOfNode==8) then
            if(obj%NumOfDim==3) then
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
                
                obj%dNdgzi(1,1)= - 0.1250d0*(1.0d0 - obj%gzi(2))*(1.0d0 - obj%gzi(3))
                obj%dNdgzi(1,2)= + 0.1250d0*(1.0d0 - obj%gzi(2))*(1.0d0 - obj%gzi(3))
                obj%dNdgzi(1,3)= + 0.1250d0*(1.0d0 + obj%gzi(2))*(1.0d0 - obj%gzi(3))
                obj%dNdgzi(1,4)= - 0.1250d0*(1.0d0 + obj%gzi(2))*(1.0d0 - obj%gzi(3))
                obj%dNdgzi(1,5)= - 0.1250d0*(1.0d0 - obj%gzi(2))*(1.0d0 + obj%gzi(3))
                obj%dNdgzi(1,6)= + 0.1250d0*(1.0d0 - obj%gzi(2))*(1.0d0 + obj%gzi(3))
                obj%dNdgzi(1,7)= + 0.1250d0*(1.0d0 + obj%gzi(2))*(1.0d0 + obj%gzi(3))
                obj%dNdgzi(1,8)= - 0.1250d0*(1.0d0 + obj%gzi(2))*(1.0d0 + obj%gzi(3))
              
                obj%dNdgzi(2,1)= - 0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0 - obj%gzi(3))
                obj%dNdgzi(2,2)= - 0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0 - obj%gzi(3))
                obj%dNdgzi(2,3)= + 0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0 - obj%gzi(3))
                obj%dNdgzi(2,4)= + 0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0 - obj%gzi(3))
                obj%dNdgzi(2,5)= - 0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0 + obj%gzi(3))
                obj%dNdgzi(2,6)= - 0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0 + obj%gzi(3))
                obj%dNdgzi(2,7)= + 0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0 + obj%gzi(3))
                obj%dNdgzi(2,8)= + 0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0 + obj%gzi(3))
              
                obj%dNdgzi(3,1)= - 0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0 - obj%gzi(2))
                obj%dNdgzi(3,2)= - 0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0 - obj%gzi(2))
                obj%dNdgzi(3,3)= - 0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0 + obj%gzi(2))
                obj%dNdgzi(3,4)= - 0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0 + obj%gzi(2))
                obj%dNdgzi(3,5)= + 0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0 - obj%gzi(2))
                obj%dNdgzi(3,6)= + 0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0 - obj%gzi(2))
                obj%dNdgzi(3,7)= + 0.1250d0*(1.0d0 + obj%gzi(1))*(1.0d0 + obj%gzi(2))
                obj%dNdgzi(3,8)= + 0.1250d0*(1.0d0 - obj%gzi(1))*(1.0d0 + obj%gzi(2))
                
        
                obj%ErrorMsg="Succeed::GetShapeFunction "
                obj%ierr=0
            else
                print *, "ERROR::GetShapeFunction"
                obj%ErrorMsg="ERROR::GetShapeFunction "
                obj%ierr=1
            endif
            
        elseif(obj%NumOfNode==9) then
            print *, "ERROR::GetShapeFunction"
            obj%ErrorMsg="ERROR::GetShapeFunction "
            obj%ierr=1
        elseif(obj%NumOfNode==10) then
            print *, "ERROR::GetShapeFunction"
            obj%ErrorMsg="ERROR::GetShapeFunction "
            obj%ierr=1
        elseif(obj%NumOfNode==11) then
            print *, "ERROR::GetShapeFunction"
            obj%ErrorMsg="ERROR::GetShapeFunction "
            obj%ierr=1
        elseif(obj%NumOfNode==12) then
            print *, "ERROR::GetShapeFunction"
            obj%ErrorMsg="ERROR::GetShapeFunction "
            obj%ierr=1
        elseif(obj%NumOfNode==13) then
            print *, "ERROR::GetShapeFunction"
            obj%ErrorMsg="ERROR::GetShapeFunction "
            obj%ierr=1
        elseif(obj%NumOfNode==14) then
            print *, "ERROR::GetShapeFunction"
            obj%ErrorMsg="ERROR::GetShapeFunction "
            obj%ierr=1
        elseif(obj%NumOfNode==15) then
            print *, "ERROR::GetShapeFunction"
            obj%ErrorMsg="ERROR::GetShapeFunction "
            obj%ierr=1
        elseif(obj%NumOfNode==16) then
            print *, "ERROR::GetShapeFunction"
            obj%ErrorMsg="ERROR::GetShapeFunction "
            obj%ierr=1
        elseif(obj%NumOfNode==17) then
            print *, "ERROR::GetShapeFunction"
            obj%ErrorMsg="ERROR::GetShapeFunction "
            obj%ierr=1
        elseif(obj%NumOfNode==18) then
            print *, "ERROR::GetShapeFunction"
            obj%ErrorMsg="ERROR::GetShapeFunction "
            obj%ierr=1
        elseif(obj%NumOfNode==19) then
            print *, "ERROR::GetShapeFunction"
            obj%ErrorMsg="ERROR::GetShapeFunction "
            obj%ierr=1
        elseif(obj%NumOfNode==20) then
            print *, "ERROR::GetShapeFunction"
            obj%ErrorMsg="ERROR::GetShapeFunction "
            obj%ierr=1
        elseif(obj%NumOfNode==21) then
            print *, "ERROR::GetShapeFunction"
            obj%ErrorMsg="ERROR::GetShapeFunction "
            obj%ierr=1
        elseif(obj%NumOfNode==22) then
            print *, "ERROR::GetShapeFunction"
            obj%ErrorMsg="ERROR::GetShapeFunction "
            obj%ierr=1
        elseif(obj%NumOfNode==23) then
            print *, "ERROR::GetShapeFunction"
            obj%ErrorMsg="ERROR::GetShapeFunction "
            obj%ierr=1
        elseif(obj%NumOfNode==24) then
            print *, "ERROR::GetShapeFunction"
            obj%ErrorMsg="ERROR::GetShapeFunction "
            obj%ierr=1
        else
            print *, "ERROR::GetShapeFunction"
            obj%ErrorMsg="ERROR::GetShapeFunction "
            obj%ierr=1
        endif
    end subroutine GetShapeFuncDer1
!!  ######################################

!!  ######################################
    subroutine GetShapeFuncDer2(obj)
        class(ShapeFunction_),intent(inout)::obj

        

    end subroutine GetShapeFuncDer2
!!  ######################################



!!  ######################################
subroutine GetElemCoord(obj,nod_coord,elem_nod,elem_id)
    class(ShapeFunction_),intent(inout)::obj
    integer(int32),intent(in)::elem_nod(:,:),elem_id
    real(real64),intent(in)::nod_coord(:,:)
    integer(int32)::i,j,k,n,m

    
    n=size(elem_nod,2)
    m=size(nod_coord,2)
    
    if(allocated(obj%ElemCoord) )then
        if(n/=size(obj%ElemCoord,1) .or. m/=size(obj%ElemCoord,2)  )then
            deallocate(obj%ElemCoord)
            allocate(obj%ElemCoord(n,m)  )
        endif
    else
        allocate(obj%ElemCoord(n,m)  )
    endif
    do j=1,n
        obj%ElemCoord(j,1:m)=nod_coord(elem_nod(elem_id,j),1:m )
        !! print *, obj%ElemCoord(j,1:m)
    enddo
    
    return


end subroutine GetElemCoord
!!  ######################################



!!  ######################################
subroutine GetElemCoord_n(obj,nod_coord_n,elem_nod,elem_id)
    class(ShapeFunction_),intent(inout)::obj
    integer(int32),intent(in)::elem_nod(:,:),elem_id
    real(real64),intent(in)::nod_coord_n(:,:)
    integer(int32)::i,j,k,n,m

    
    n=size(elem_nod,2)
    m=size(nod_coord_n,2)
    
    if(allocated(obj%ElemCoord_n) )then
        if(n/=size(obj%ElemCoord_n,1) .or. m/=size(obj%ElemCoord_n,2)  )then
            deallocate(obj%ElemCoord_n)
            allocate(obj%ElemCoord_n(n,m)  )
        endif
    else
        allocate(obj%ElemCoord_n(n,m)  )
    endif
    do j=1,n
        obj%ElemCoord_n(j,1:m)=nod_coord_n(elem_nod(elem_id,j),1:m )
        !! print *, obj%ElemCoord_n(j,1:m)
    enddo
    
    return


end subroutine 
!!  ######################################



!!  ######################################
subroutine getdu(obj)
    class(ShapeFunction_),intent(inout)::obj
    integer(int32)::i,j,k,n,m

    n=size(obj%ElemCoord,1)
    m=size(obj%ElemCoord,2)
    
    if(allocated(obj%du) )then
        if(n/=size(obj%du,1) .or. m/=size(obj%du,2)  )then
            deallocate(obj%du)
            allocate(obj%du(n,m)  )
        endif
    else
        allocate(obj%du(n,m)  )
    endif
    do j=1,n
        obj%du(j,1:m)=obj%ElemCoord(j,1:m)-obj%ElemCoord_n(j,1:m)
    enddo
    
    return


end subroutine 
!!  ######################################


!!  ######################################
subroutine GetJmat(obj)
    class(ShapeFunction_),intent(inout)::obj
    integer(int32) n


    n=size(obj%ElemCoord,2)
    if(allocated(obj%Jmat) )then
        if(n/=size(obj%Jmat,1) .or. n/=size(obj%Jmat,2)  )then
            deallocate(obj%Jmat)
            allocate(obj%Jmat(n,n)  )
            allocate(obj%JmatInv(n,n) )
    
        endif
    else
        allocate(obj%Jmat(n,n)  )
        allocate(obj%JmatInv(n,n) )
    endif
    
    obj%Jmat(:,:)=matmul(obj%dNdgzi,obj%ElemCoord)
    
    call inverse_rank_2(obj%Jmat,obj%JmatInv)
    
end subroutine GetJmat
!!  ######################################

subroutine exportShapeFunction(obj,restart,path)
    class(ShapeFunction_),intent(inout) :: obj
    logical,optional,intent(in) :: restart
    character(*),intent(in) :: path
    type(IO_) :: f

    if(present(restart) )then
        call execute_command_line("mkdir -p "//trim(path)//"/ShapeFunction")
        call f%open(trim(path)//"/ShapeFunction/","ShapeFunction",".res")
        write(f%fh,*) obj%Nmat(:)
        write(f%fh,*) obj%dNdgzi(:,:)
        write(f%fh,*) obj%dNdgzidgzi(:,:)
        write(f%fh,*) obj%gzi(:)
        write(f%fh,*) obj%gaussPoint(:,:)
        write(f%fh,*) obj%gaussIntegWei(:)
        write(f%fh,*) obj%Jmat(:,:)
        write(f%fh,*) obj%JmatInv(:,:)
        write(f%fh,*) obj%ElemCoord(:,:)
        write(f%fh,*) obj%ElemCoord_n(:,:)
        write(f%fh,*) obj%du(:,:)
        write(f%fh,*) obj%detJ
        write(f%fh,*) obj%NumOfNode
        write(f%fh,*) obj%NumOfOrder
        write(f%fh,*) obj%NumOfDim
        write(f%fh,*) obj%NumOfGp
        write(f%fh,*) obj%GpID
        write(f%fh,*) obj%ierr
        write(f%fh,*) obj%currentGpID
        write(f%fh,*) obj%ReducedIntegration
        write(f%fh,'(A)' ) trim(obj%elemType)
        write(f%fh,'(A)' ) trim(obj%ErrorMsg)
        call f%close()
    endif

end subroutine

end module ShapeFunctionClass