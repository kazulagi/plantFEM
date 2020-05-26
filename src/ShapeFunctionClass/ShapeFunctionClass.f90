module ShapeFunctionClass
    use, intrinsic :: iso_fortran_env
    use MathClass
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
        integer(int32) :: NumOfDim
        integer(int32) :: NumOfGp
        integer(int32) :: GpID
        integer(int32) :: ierr
        integer(int32) :: currentGpID
        logical :: ReducedIntegration = .false.
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
    integer(int32),intent(in) ::ElemNod(:,:),ElemID,GpID
    real(real64),intent(in) ::NodCoord(:,:)
    integer(int32) :: i,j

    if(present(ElemType) )then
        call obj%init(ElemType)
    endif
    call obj%GetAll(elem_id=i,nod_coord=NodCoord,elem_nod=ElemNod,OptionalGpID=j)

end subroutine
!##################################################


!##################################################
subroutine SetShapeFuncType(obj,ReducedIntegration)
    class(ShapeFunction_),intent(inout)::obj
    logical,optional,intent(in) :: ReducedIntegration
    character*70 ::  TrimedElemType


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
!##################################################

!##################################################
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
!##################################################

!##################################################
subroutine GetAllShapeFunc(obj,elem_id,nod_coord,nod_coord_n,elem_nod,OptionalNumOfNode,OptionalNumOfOrder,&
    OptionalNumOfDim,OptionalNumOfGp,OptionalGpID,ReducedIntegration)
    class(ShapeFunction_),intent(inout)::obj
    integer(int32),optional,intent(in)::elem_nod(:,:),elem_id
    real(real64),optional,intent(in)::nod_coord(:,:),nod_coord_n(:,:)
    integer(int32),optional,intent(in)::OptionalNumOfNode,OptionalNumOfOrder,&
    OptionalNumOfDim,OptionalNumOfGp,OptionalGpID
    logical,optional,intent(in) :: ReducedIntegration

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
    !call GetShapeFuncDer2(obj)



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
            if(obj%ReducedIntegration .eqv. .true.)then
                obj%GaussPoint(:,:) =0.0d0
                obj%GaussIntegWei(:)=1.0d0/4.0d0
            endif
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
            
            if(obj%ReducedIntegration .eqv. .true.)then
                obj%GaussPoint(:,:) =0.0d0
                obj%GaussIntegWei(:)=1.0d0/8.0d0
            endif

        elseif(size(obj%GaussPoint,2)==1 )then
            ! Triangular or Tetrahedral
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
!--------------------------------------------------------

!--------------------------------------------------------
    subroutine SetGaussPoint(obj)

        class(ShapeFunction_),intent(inout)::obj
        if(allocated(obj%gzi) ) then
            deallocate(obj%gzi)
        endif
        allocate(obj%gzi(obj%NumOfDim) )
        
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
!--------------------------------------------------------


!--------------------------------------------------------
    subroutine GetShapeFunction(obj)
        class(ShapeFunction_),intent(inout)::obj

        
if(allocated(obj%Nmat) ) then
    deallocate(obj%Nmat)
endif
allocate(obj%Nmat(obj%NumOfNode) )


if(obj%NumOfNode==1) then
    !#########################################################################
    !#######                                                           #######
    !#######                             +     (1)                     #######
    !#######                                                           #######
    !#######                                                           #######
    !#########################################################################
    print *, "ERROR::GetShapeFunction"
    obj%ErrorMsg="ERROR::GetShapeFunction NumOfNode==1 "
    obj%ierr=1

elseif(obj%NumOfNode==2) then
    
    if(obj%NumOfDim==1) then
        
        !#########################################################################
        !#######                                                           #######
        !#######                +>----------------------->+                #######
        !#######            (1)                               (2)          #######
        !#######                                                           #######
        !#########################################################################
        
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
        !#########################################################################
        !#######                                                           #######
        !#######                +-----------+-------------+                #######
        !#######            (1)             (2)            (3)             #######
        !#######                                                           #######
        !#########################################################################
        !allocate(obj%Nmat(obj%NumOfNode,obj%NumOfDim) )
        !
        !obj%Nmat(1,1)=0.50d0*( 1.0d0-gzi(1))
        !obj%Nmat(1,2)=0.50d0*(-1.0d0+gzi(1))
        !obj%Nmat(1,3)=0.50d0*( 1.0d0-gzi(1))
        !
        !obj%ErrorMsg="Succeed::GetShapeFunction "
        !obj%ierr=0
        print *, "ERROR::GetShapeFunction"
        obj%ErrorMsg="ERROR::GetShapeFunction "
        obj%ierr=1
    elseif(obj%NumOfDim==2) then
        !#########################################################################
        !#######                                                           #######
        !#######          (1)   +-------------------------+                #######
        !#######                 \                       /  (3)            #######
        !#######                   \                   /                   #######
        !#######                     \               /                     #######
        !#######                       \           /                       #######
        !#######                         \       /                         #######
        !#######                           \   /                           #######        
        !#######                       (2)   +                             #######        
        !#########################################################################
        
        
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
        !#########################################################################
        !#######                                                           #######
        !#######                +-------+---------+-------+                #######
        !#######          (1)          (2)        (3)       (4)            #######
        !#######                                                           #######
        !#########################################################################
        !obj%ErrorMsg="Succeed::GetShapeFunction "
        !obj%ierr=0
        print *, "ERROR::GetShapeFunction"
        obj%ErrorMsg="ERROR::GetShapeFunction "
        obj%ierr=1
    elseif(obj%NumOfDim==2) then
        !#########################################################################
        !#######                                                           #######
        !#######           (1)  +-------------------------+  (4)           #######
        !#######                |                         |                #######
        !#######                |                         |                #######
        !#######                |                         |                #######
        !#######                |                         |                #######
        !#######                |                         |                #######
        !#######                |                         |                #######        
        !#######           (2)  +-------------------------+  (3)           #######        
        !#########################################################################
        
		obj%Nmat(1)=1.0d0/4.0d0*(1.0d0-obj%gzi(1))*(1.0d0-obj%gzi(2))
	    obj%Nmat(2)=1.0d0/4.0d0*(1.0d0+obj%gzi(1))*(1.0d0-obj%gzi(2))
		obj%Nmat(3)=1.0d0/4.0d0*(1.0d0+obj%gzi(1))*(1.0d0+obj%gzi(2))
		obj%Nmat(4)=1.0d0/4.0d0*(1.0d0-obj%gzi(1))*(1.0d0+obj%gzi(2))
	  
        obj%ErrorMsg="Succeed::GetShapeFunction "
        obj%ierr=0
    elseif(obj%NumOfDim==3) then
        !#########################################################################
        !#######                    (4)   +                                #######
        !#######                        /   \                              #######
        !#######                      /       \                            #######
        !#######                    /           \                          #######
        !#######                  /           ___+    (3)                  #######
        !#######                /  ___----'''     \                        #######
        !#######          (1)  +  -                 \                      #######
        !#######                '''-->---____       \                      #######        
        !#######                              -->--- +   (2)               #######        
        !#########################################################################
        
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
        !#########################################################################
        !#######           (8)   +-------------------------+ (7)           #######
        !#######                /|                        /|               #######
        !#######               / |                  (6)  / |               #######
        !#######          (5) +--|----------------------+  |               #######
        !#######              |  |                      |  |               #######
        !#######              |  +----------------------|--+ (3)           #######
        !#######              | / (4)                   | /                #######
        !#######              |/                        |/                 #######        
        !#######          (1) +-------------------------+ (2)              #######        
        !#########################################################################
        
        obj%Nmat(1)=1.0d0/8.0d0*(1.0d0 - obj%gzi(1))*(1.0d0 - obj%gzi(2))*(1.0d0 - obj%gzi(3))
	    obj%Nmat(2)=1.0d0/8.0d0*(1.0d0 + obj%gzi(1))*(1.0d0 - obj%gzi(2))*(1.0d0 - obj%gzi(3))
		obj%Nmat(3)=1.0d0/8.0d0*(1.0d0 + obj%gzi(1))*(1.0d0 + obj%gzi(2))*(1.0d0 - obj%gzi(3))
		obj%Nmat(4)=1.0d0/8.0d0*(1.0d0 - obj%gzi(1))*(1.0d0 + obj%gzi(2))*(1.0d0 - obj%gzi(3))
        obj%Nmat(5)=1.0d0/8.0d0*(1.0d0 - obj%gzi(1))*(1.0d0 - obj%gzi(2))*(1.0d0 + obj%gzi(3))
	    obj%Nmat(6)=1.0d0/8.0d0*(1.0d0 + obj%gzi(1))*(1.0d0 - obj%gzi(2))*(1.0d0 + obj%gzi(3))
		obj%Nmat(7)=1.0d0/8.0d0*(1.0d0 + obj%gzi(1))*(1.0d0 + obj%gzi(2))*(1.0d0 + obj%gzi(3))
		obj%Nmat(8)=1.0d0/8.0d0*(1.0d0 - obj%gzi(1))*(1.0d0 + obj%gzi(2))*(1.0d0 + obj%gzi(3))
	  

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
        !print *, obj%ElemCoord(j,1:m)
    enddo
    
    return


end subroutine GetElemCoord
!--------------------------------------------------------



!--------------------------------------------------------
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
        !print *, obj%ElemCoord_n(j,1:m)
    enddo
    
    return


end subroutine 
!--------------------------------------------------------



!--------------------------------------------------------
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
!--------------------------------------------------------


!--------------------------------------------------------
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
!--------------------------------------------------------

end module ShapeFunctionClass