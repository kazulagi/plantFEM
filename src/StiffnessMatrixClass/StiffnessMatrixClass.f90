module StiffnessMatrixClass
    use ShapeFunctionClass
    use MeshClass
    use StressClass
    use StrainClass

    implicit none

    ! Provides a set of Element-by-element stiffness matrix & RHS vector of internal force
    ! for all types of elements, all Strain Theory, and all constitutive models.

    type :: StiffnessMatrix_
        real(8),allocatable     :: Amat(:,:),bvec(:)   
        type(ShapeFunction_)    :: ShapeFunc
        type(Stress_),pointer   :: Stress
        type(Strain_),pointer   :: Strain


        integer :: TheoryID,ElemID,GpID
        
        character*40 :: StrainTheory
        
        ! Please input one of following keyphrases

        ! Finite_Elasticity
        ! Finite_ElastoPlasticity
        ! Infinitesimal_Elasticity
        ! Infinitesimal_ElastoPlasticity
        ! Small_strain
    contains
        procedure,public :: init    => initStiffnessMatrix
        procedure,public :: update  => updateStiffnessMatrix
    end type

contains

! ######################################
subroutine initStiffnessMatrix(obj,FEMDomain,Stress,Strain,withInit)
    class(StiffnessMatrix_),intent(inout)::obj
    !character(*),intent(in) :: StrainTheory
    class(FEMDomain_),intent(in) :: FEMDomain
    class(Stress_),target,intent(in) :: Stress
    class(Strain_),target,intent(in) :: Strain
    logical,optional,intent(in) :: withInit
    integer :: NumOfDim,NumOfNodePerElem

    obj%Stress => Stress
    obj%Strain => Strain
    ! create Stress/Strain measure objects
    !obj%StrainTheory = StrainTheory
    !call obj%Stress%init(StrainTheory)
    !call obj%Stress%init(StrainTheory)

    ! create a ShapeFunction object 
    NumOfDim         = size(FEMDomain%Mesh%NodCoord,2)
    NumOfNodePerElem = size(FEMDomain%Mesh%ElemNod,2)
    call obj%ShapeFunc%getType(NumOfDim=NumOfDim,NumOfNodePerElem=NumOfNodePerElem)
    call obj%ShapeFunc%setType()

    obj%ElemID=1
    obj%GpID=1

end subroutine
! ######################################

! ######################################
subroutine updateStiffnessMatrix(obj,Mesh,ElemID,MeshID)
    class(StiffnessMatrixClass_),intent(inout) :: obj
    class(Mesh_),intent(in) :: Mesh 
    integer,optional,intent(in) :: ElemID,MeshID
    integer :: ElemID_out,MeshID_out

    ! get element ID and Gauss-point-id
    if(present(ElemID))then
        ElemID_out=ElemID
    else
        ElemID_out=obj%ElemID
    endif
    if(present(GpID))then
        GpID_out=GpID
    else
        GpID_out=obj%GpID
    endif

    ! get all objects related to a shape function
    call obj%ShapeFunc%update(ElemType=obj%ShapeFunc%ElemType,&
        NodCoord=Mesh%NodCoordInit,NodCoord=Mesh%NodCoord_n,&
        ElemNod=Mesh%ElemNod,ElemID=ElemID_out,GpID=GpID_out)

    ! create strain tensor


    ! create stress tensor


    ! create stiffness matrix
    


    ! update element ID and Gauss-point-id
    obj%GpID = obj%GpID +1
    if(obj%GpID > size(obj%ShapeFunc%NumOfGp) )then
        ! go to next element
        obj%GpID = 1
        obj%ElemID =obj%ElemID + 1 
    else
        ! go to next Gauss point in the same element
        obj%GpID = obj%GpID + 1
    endif
end subroutine
! ######################################

end module
