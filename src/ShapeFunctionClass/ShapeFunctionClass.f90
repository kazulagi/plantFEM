module ShapeFunctionClass
    use MathClass
    type::ShapeFunction_
        
        real(8),allocatable::Nmat(:)
        real(8),allocatable::dNdgzi(:,:)
        real(8),allocatable::dNdgzidgzi(:,:)
        real(8),allocatable::gzi(:)
        real(8),allocatable::GaussPoint(:,:)
        real(8),allocatable::GaussIntegWei(:)
        real(8),allocatable::Jmat(:,:),JmatInv(:,:)
        real(8),allocatable::ElemCoord(:,:)

        real(8) :: detJ

        integer :: NumOfNode
        integer :: NumOfOrder
        integer :: NumOfDim
        integer :: NumOfGp
        integer :: GpID
        integer :: ierr
        integer :: currentGpID
        
        character*70::ElemType
        character(len=60):: ErrorMsg
    contains
        procedure :: init => initShapeFunction
        procedure :: update => updateShapeFunction

        procedure :: SetType => SetShapeFuncType
        procedure :: GetAll  => GetAllShapeFunc
        procedure :: Deallocate => DeallocateShapeFunction
        procedure :: getType => getShapeFuncType 
        procedure :: GetGaussPoint => GetGaussPoint
    end type ShapeFunction_

contains

!##################################################
subroutine initShapeFunction(obj,ElemType)
    class(ShapeFunction_),intent(inout) :: obj
    character(*),intent(in),optional :: ElemType

    obj%ElemType = ElemType

    call obj%SetType()

end subroutine
!##################################################


!##################################################
subroutine updateShapeFunction(obj,ElemType,NodCoord,ElemNod,ElemID,GpID)
    class(ShapeFunction_),intent(inout) :: obj
    character(*),optional,intent(in) :: ElemType
    integer,intent(in) ::ElemNod(:,:),ElemID,GpID
    real(8),intent(in) ::NodCoord(:,:)

    if(present(ElemType) )then
        call obj%init(ElemType)
    endif
    call obj%GetAll(elem_id=i,nod_coord=NodCoord,elem_nod=ElemNod,OptionalGpID=j)

end subroutine
!##################################################


!##################################################
subroutine SetShapeFuncType(obj)
    class(ShapeFunction_),intent(inout)::obj
    character*70 ::  TrimedElemType

    

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
    else
        obj%ErrorMsg="ShapeFunctionClass.f90 :: SetShapeFuncType >> Element : "//TrimedElemType//"is not defined."
        print *, "ShapeFunctionClass.f90 :: SetShapeFuncType >> Element : ",TrimedElemType,"is not defined."
        return
    endif

    

end subroutine SetShapeFuncType
!##################################################

!##################################################
subroutine getShapeFuncType(obj,NumOfDim,NumOfNodePerElem)
    class(ShapeFunction_),intent(inout)::obj
    character*70 ::  TrimedElemType
    integer,intent(in) :: NumOfDim,NumOfNodePerElem

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
!##################################################

!##################################################
subroutine GetAllShapeFunc(obj,elem_id,nod_coord,elem_nod,OptionalNumOfNode,OptionalNumOfOrder,&
    OptionalNumOfDim,OptionalNumOfGp,OptionalGpID)
    class(ShapeFunction_),intent(inout)::obj
    integer,optional,intent(in)::elem_nod(:,:),elem_id
    real(8),optional,intent(in)::nod_coord(:,:)
    integer,optional,intent(in)::OptionalNumOfNode,OptionalNumOfOrder,&
    OptionalNumOfDim,OptionalNumOfGp,OptionalGpID

    

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
    !call GetShapeFuncDer2(obj)



    if(.not.present(nod_coord) .or. .not.present(elem_nod) )then
        
        obj%ErrorMsg="Mesh%NodCoord and Mesh%ElemNod is necessary to get Jmat"
        print *, obj%ErrorMsg
        
        return
    endif
    call GetElemCoord(obj,nod_coord,elem_nod,elem_id)



    call GetJmat(obj)
    obj%detJ = det_mat(obj%Jmat,size(obj%Jmat,1 ) )

end subroutine GetAllShapeFunc
!##################################################


!##################################################
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
!##################################################

!--------------------------------------------------------
    subroutine GetGaussPoint(obj)

        class(ShapeFunction_),intent(inout)::obj

        !include "./GetGaussPoint.f90"

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
            obj%GaussPoint(1,1) = -1.0d0/dsqrt(3.0d0)
            obj%GaussPoint(1,2) =  1.0d0/dsqrt(3.0d0)
            obj%GaussPoint(1,3) =  1.0d0/dsqrt(3.0d0)
            obj%GaussPoint(1,4) = -1.0d0/dsqrt(3.0d0)

            obj%GaussPoint(2,1) = -1.0d0/dsqrt(3.0d0)
            obj%GaussPoint(2,2) = -1.0d0/dsqrt(3.0d0)
            obj%GaussPoint(2,3) =  1.0d0/dsqrt(3.0d0)
            obj%GaussPoint(2,4) =  1.0d0/dsqrt(3.0d0)
            
            obj%GaussIntegWei(:)=1.0d0
        elseif(size(obj%GaussPoint,1)==3 .and. size(obj%GaussPoint,2)==8)then
            obj%GaussPoint(1,1) = -1.0d0/dsqrt(3.0d0)
            obj%GaussPoint(1,2) =  1.0d0/dsqrt(3.0d0)
            obj%GaussPoint(1,3) =  1.0d0/dsqrt(3.0d0)
            obj%GaussPoint(1,4) = -1.0d0/dsqrt(3.0d0)
            obj%GaussPoint(1,5) = -1.0d0/dsqrt(3.0d0)
            obj%GaussPoint(1,6) =  1.0d0/dsqrt(3.0d0)
            obj%GaussPoint(1,7) =  1.0d0/dsqrt(3.0d0)
            obj%GaussPoint(1,8) = -1.0d0/dsqrt(3.0d0)

            obj%GaussPoint(2,1) = -1.0d0/dsqrt(3.0d0)
            obj%GaussPoint(2,2) = -1.0d0/dsqrt(3.0d0)
            obj%GaussPoint(2,3) =  1.0d0/dsqrt(3.0d0)
            obj%GaussPoint(2,4) =  1.0d0/dsqrt(3.0d0)
            obj%GaussPoint(2,5) = -1.0d0/dsqrt(3.0d0)
            obj%GaussPoint(2,6) = -1.0d0/dsqrt(3.0d0)
            obj%GaussPoint(2,7) =  1.0d0/dsqrt(3.0d0)
            obj%GaussPoint(2,8) =  1.0d0/dsqrt(3.0d0)

            obj%GaussPoint(3,1) = -1.0d0/dsqrt(3.0d0)
            obj%GaussPoint(3,2) = -1.0d0/dsqrt(3.0d0)
            obj%GaussPoint(3,3) = -1.0d0/dsqrt(3.0d0)
            obj%GaussPoint(3,4) = -1.0d0/dsqrt(3.0d0)
            obj%GaussPoint(3,5) =  1.0d0/dsqrt(3.0d0)
            obj%GaussPoint(3,6) =  1.0d0/dsqrt(3.0d0)
            obj%GaussPoint(3,7) =  1.0d0/dsqrt(3.0d0)
            obj%GaussPoint(3,8) =  1.0d0/dsqrt(3.0d0)

            obj%GaussIntegWei(:)=1.0d0

        else
            print *, "ERROR :: ShapeFunctionClass/GetGaussPoint is not defined"
        endif


    end subroutine GetGaussPoint
!--------------------------------------------------------

!--------------------------------------------------------
    subroutine SetGaussPoint(obj)

        class(ShapeFunction_),intent(inout)::obj

        include "./SetGaussPoint.f90"


    end subroutine SetGaussPoint
!--------------------------------------------------------


!--------------------------------------------------------
    subroutine GetShapeFunction(obj)
        class(ShapeFunction_),intent(inout)::obj

        
        include "./GetShapeFunction.f90"

    end subroutine GetShapeFunction
!--------------------------------------------------------

!--------------------------------------------------------
    subroutine GetShapeFuncDer1(obj)
        class(ShapeFunction_),intent(inout)::obj

        
        include "./GetShapeFuncDer1.f90"

    end subroutine GetShapeFuncDer1
!--------------------------------------------------------

!--------------------------------------------------------
    subroutine GetShapeFuncDer2(obj)
        class(ShapeFunction_),intent(inout)::obj

        
        include "./GetShapeFuncDer2.f90"

    end subroutine GetShapeFuncDer2
!--------------------------------------------------------



!--------------------------------------------------------
subroutine GetElemCoord(obj,nod_coord,elem_nod,elem_id)
    class(ShapeFunction_),intent(inout)::obj
    integer,intent(in)::elem_nod(:,:),elem_id
    real(8),intent(in)::nod_coord(:,:)
    integer::i,j,k,n,m

    
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
        !print *, obj%ElemCoord(j,1:m)
    enddo
    
    return


end subroutine GetElemCoord
!--------------------------------------------------------


!--------------------------------------------------------
subroutine GetJmat(obj)
    class(ShapeFunction_),intent(inout)::obj
    integer n


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
!--------------------------------------------------------

end module ShapeFunctionClass