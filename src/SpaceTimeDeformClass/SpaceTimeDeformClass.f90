module SpaceTimeDeformClass
    use MathClass
	use LinearSolverClass
	use FEMDomainClass
	use PostProcessingClass
    use ConstitutiveModelClass
    
    type :: SpaceTimeFEM_
		type(STFEMDomain_),pointer ::STFEMDomain
	    real(8),allocatable ::DeformStress(:,:,:)
	    real(8),allocatable ::DeformStrain(:,:,:)
        real(8),allocatable ::DeformStressInit(:,:,:)
		real(8),allocatable ::DeformStressMat(:,:,:)
		real(8),allocatable ::DeformStressRHS(:,:)
		real(8),allocatable ::DeformVecEBETot(:,:)
        real(8),allocatable ::DeformVecEBEInc(:,:)

        real(8),allocatable ::DeformVecGloTot(:)
		real(8),allocatable ::DeformVecGloInc(:)
		
		real(8),allocatable ::TractionVecGlo(:)
		real(8),allocatable ::ResidualVecGlo(:)
		real(8),allocatable ::InternalVecGlo(:)

		real(8),allocatable ::VolInitCurrEBE(:,:)
        real(8)             ::dt,error


        logical :: MeshMove
    contains
        procedure :: SetMovingMesh    => SetMovingMesh
        procedure :: Init       => Init
        procedure :: SetMatrix  => SetMatrix
        procedure :: SetRHS     => SetRHS
    end type

contains


! ############################################################
subroutine SetMovingMesh(obj,on_off)
    class(SpaceTimeFEM_),intent(inout)  :: obj
    logical,intent(in)::on_off

    obj%MeshMove = on_off

end subroutine
! ############################################################


! ############################################################
subroutine Init(obj)
    class(SpaceTimeFEM_),intent(inout)  :: obj

    obj%MeshMove = .false.
    
end subroutine
! ############################################################



! ############################################################
subroutine SetMatrix(obj)
    class(SpaceTimeFEM_),intent(inout)  :: obj
    
end subroutine
! ############################################################



! ############################################################
subroutine GetSTShapeFunc(obj,ElemID,GpID)
    class(SpaceTimeFEM_),intent(inout)  :: obj
    integer,optional,intent(in)::ElemID
    integer,optional,intent(in)::GpID

    if( obj%MeshMove .eqv. .false. )then
        call obj%STFEMDomain%ShapeFunction%GetAll(elem_id=ElemID,nod_coord=obj%STFEMDomain%Mesh%NodCoord,&
            elem_nod=obj%STFEMDomain%Mesh%ElemNod,OptionalGpID=GpID)
        call obj%STFEMDomain%TimeShapeFunction%GetAll(elem_id=ElemID,nod_coord=obj%STFEMDomain%Mesh%NodCoord,&
            elem_nod=obj%STFEMDomain%Mesh%ElemNod,OptionalGpID=GpID)
    elseif(obj%MeshMove .eqv. .true.)then
        call obj%STFEMDomain%ShapeFunction%GetAll(elem_id=ElemID,nod_coord=obj%STFEMDomain%Mesh%NodCoord,&
            elem_nod=obj%STFEMDomain%Mesh%ElemNod,OptionalGpID=GpID)
        call obj%STFEMDomain%TimeShapeFunction%GetAll(elem_id=ElemID,nod_coord=obj%STFEMDomain%Mesh%NodCoord,&
            elem_nod=obj%STFEMDomain%Mesh%ElemNod,OptionalGpID=GpID)
    else
        stop "ERROR :: SetElementMatrix >> please run constructor obj%Init to create instance"
    endif

end subroutine
! ############################################################


! ############################################################
subroutine SetElementMatrix(obj)
    class(SpaceTimeFEM_),intent(inout)  :: obj


end subroutine
! ############################################################


! ############################################################
subroutine SetSpaceTimeShapeFunction(Nobj,Tobj)
    class(ShapeFunction_),intent(inout)  :: Nobj,Tobj

end subroutine


! ############################################################




! ############################################################
subroutine SetRHS(obj)
    class(SpaceTimeFEM_),intent(inout)  :: obj

    
end subroutine
! ############################################################

end module