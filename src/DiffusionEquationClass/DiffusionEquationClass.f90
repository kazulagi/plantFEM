module DiffusionEquationClass
    use FEMDomainClass
    use PostProcessingClass
    use LinearSolverClass
    implicit none

    type:: DiffusionEq_
        type(FEMDomain_),pointer ::FEMDomain
        real(8),allocatable ::UnknownValue(:,:)
        real(8),allocatable ::UnknownValueInit(:,:)
        real(8),allocatable ::UnknownValueRate(:,:)
        real(8),allocatable ::DiffusionMat(:,:,:)
        real(8),allocatable ::Divergence(:,:)
        real(8),allocatable ::Flowvector(:,:)
        real(8),allocatable ::FluxVector3D(:,:)
        real(8)             ::dt
        integer             :: step
    contains
        procedure :: Solve => SolveDiffusionEq
        procedure :: Update => UpdateDiffusionEq
        procedure :: Setup => SetupDiffusionEq
        procedure :: GetMat => GetDiffusionMat
        procedure :: GetRHS => GetFlowvector
        procedure :: GetInitVal => GetUnknownValue
        procedure :: Display => DisplayDiffusionEq 
    end type

contains

!######################## ImportData of DiffusionEq ########################
subroutine ImportFEMDomainDiff(obj,OptionalFileFormat,OptionalProjectName)
    class(FEMDomain_),intent(inout)::obj
    character*4,optional,intent(in)::OptionalFileFormat
    character*70,optional,intent(in)::OptionalProjectName

    character*4::FileFormat
    character*70::ProjectName
    character*74 ::FileName
    integer,allocatable::IntMat(:,:)
    real(8),allocatable::RealMat(:,:)
    integer :: fh,i,j,NumOfDomain,n,m,DimNum
    character*70 Msg

call DeallocateFEMDomain(obj)

fh =104
if(present(OptionalFileFormat) )then
    FileFormat=trim(OptionalFileFormat)
else
    FileFormat=".scf"
endif


if(present(OptionalProjectName) )then
    ProjectName=trim(OptionalProjectName)
else
    ProjectName="untitled"
endif

FileName = trim(ProjectName)//trim(FileFormat)
obj%FileName = trim(FileName)
obj%FilePath = "./"
!!print *, "Project : ",ProjectName
!!print *, "is Exported as : ",FileFormat," format"
!!print *, "File Name is : ",FileName

open(fh,file=FileName,status="old")

if(trim(FileFormat)==".scf" )then

    read(fh,*) NumOfDomain
    allocate(IntMat(NumOfDomain,2))
    allocate(obj%Mesh%SubMeshNodFromTo(NumOfDomain,3) )
    allocate(obj%Mesh%SubMeshElemFromTo(NumOfDomain,3) )
    
    do i=1,NumOfDomain
        obj%Mesh%SubMeshNodFromTo(i,1)=i
        read(fh,*) obj%Mesh%SubMeshNodFromTo(i,2),obj%Mesh%SubMeshNodFromTo(i,3)
    enddo

    do i=1,NumOfDomain
        obj%Mesh%SubMeshElemFromTo(i,1)=i
        read(fh,*) obj%Mesh%SubMeshElemFromTo(i,3)
        if(i==1)then
            obj%Mesh%SubMeshElemFromTo(i,2)=1    
        else
            obj%Mesh%SubMeshElemFromTo(i,2)=obj%Mesh%SubMeshElemFromTo(i-1,3)+1
        endif
    enddo
    
    read(fh,*) n,m
    DimNum=m
    allocate(obj%Mesh%NodCoord(n,m) )
    call ImportArray(obj%Mesh%NodCoord,OptionalFileHandle=fh)

    read(fh,*) n,m
    read(fh,*)obj%Mesh%ElemType
    allocate(obj%Mesh%ElemNod(n,m) )
    allocate(obj%Mesh%ElemMat(n  ) )
    call ImportArray(obj%Mesh%ElemNod,OptionalFileHandle=fh)
    do i=1,n
        read(fh,*) obj%Mesh%ElemMat(i)
    enddo
    read(fh,*) n,m
    allocate(obj%MaterialProp%MatPara(n,m) )
    call ImportArray(obj%MaterialProp%MatPara,OptionalFileHandle=fh)
    


    !######### Dirichlet boundary conditions #################
    DimNum=1
    allocate(obj%Boundary%DBoundNum(DimNum ))
    read(fh,*) obj%Boundary%DBoundNum(:)
    allocate(obj%Boundary%DBoundNodID( maxval(obj%Boundary%DBoundNum), size(obj%Boundary%DBoundNum)  )  )
    allocate(obj%Boundary%DBoundVal( maxval(obj%Boundary%DBoundNum), size(obj%Boundary%DBoundNum)  )  )

    obj%Boundary%DBoundNodID(:,:)=-1
    obj%Boundary%DBoundVal(:,:)  =0.0d0

    do i=1,size(obj%Boundary%DBoundNum,1)
        do j=1,obj%Boundary%DBoundNum(i)
            read(fh,*) obj%Boundary%DBoundNodID(j,i)
        enddo
        do j=1,obj%Boundary%DBoundNum(i)
            read(fh,*) obj%Boundary%DBoundVal(j,i)
        enddo
    enddo
    !######### Dirichlet boundary conditions #################
    
    
    
    !######### Neumann boundary conditions #################
    read(fh,*) n
    allocate( obj%Boundary%NBoundNum(DimNum))
    allocate(obj%Boundary%NBoundNodID(n, size(obj%Boundary%NBoundNum)  )  )
    allocate(obj%Boundary%NBoundVal( n, size(obj%Boundary%NBoundNum)  )  )
    obj%Boundary%NBoundNodID(:,:)=-1
    obj%Boundary%NBoundVal(:,:)  =0.0d0

    obj%Boundary%NBoundNum(:)=n
    do i=1,n
        read(fh,*) m
        obj%Boundary%NBoundNodID(i,:)=m
    enddo

    do i=1,n
        read(fh,*) obj%Boundary%NBoundVal(i,:)
    enddo
    !######### Neumann boundary conditions #################




    !######### Initial conditions #################
    read(fh,*) n
    allocate(obj%Boundary%TBoundNodID(n,1) )
    allocate(obj%Boundary%TBoundVal(  n,1) )
    allocate(obj%Boundary%TBoundNum(  1) )

    obj%Boundary%TBoundNum=n
    
    if(n/=0)then
        if(n<0)then
            print *, "ERROR :: number of initial conditions are to be zero"
        else
            do i=1,n
                read(fh,*) obj%Boundary%TBoundNodID(i,1)
            enddo
            do i=1,n
                read(fh,*) obj%Boundary%TBoundVal(i,1)
            enddo
        endif
    endif
    
    !######### Initial conditions #################
    read(fh,*) obj%ControlPara%ItrTol,obj%ControlPara%Timestep
    close(fh)
else
    !!print *, "ERROR :: ExportFEMDomain >> only .scf file can be exported."
endif

end subroutine ImportFEMDomainDiff
!######################## ImportData of DiffusionEq ########################







!######################## Solve DiffusionEq ########################
subroutine SolveDiffusionEq(obj,Solvertype)
    class(DiffusionEq_),intent(inout)::obj
    character(*),optional,intent(in)::Solvertype
    character*70 ::solver,defaultsolver

    real(8),allocatable::Amat(:,:),bvec(:),xvec(:)
    real(8)::val,er
    integer ::i,j, n,m,k,nodeid1,nodeid2,localid,itrmax

    defaultsolver="GaussJordan"

    if(present(SolverType) )then            
        solver=Solvertype
    else
        solver=defaultsolver
    endif

    
    n=size(obj%FEMDomain%Mesh%NodCoord,1)
    allocate(Amat(n,n) ,bvec(n),xvec(n) )
    Amat(:,:)=0.0d0
    bvec(:)     =0.0d0

    !===================================
    ! assemble matrix
    n=size(obj%FEMDomain%Mesh%ElemNod,1)    
    m=size(obj%FEMDomain%Mesh%ElemNod,2)
    do i=1,n
        do j=1,m
            nodeid1=obj%FEMDomain%Mesh%ElemNod(i,j)
            do k=1,m
                nodeid2=obj%FEMDomain%Mesh%ElemNod(i,k)
                Amat(nodeid1,nodeid2)=Amat(nodeid1,nodeid2)&
                +obj%DiffusionMat(i,j,k)
            enddo
            bvec(nodeid1)=bvec(nodeid1)+obj%Flowvector(i,j)
        enddo
    enddo
    !===================================
    
    

    !===================================
    xvec(:)=0.0d0
    ! set initial value for xvec
    ! 
    if(allocated(obj%FEMDomain%Boundary%TboundNodID) )then
        
        if(obj%step==1)then
            do i=1,size(obj%FEMDomain%Boundary%TboundNodID,1)
                if(obj%FEMDomain%Boundary%TboundNodID(i,1)>=1 )then
                    xvec(obj%FEMDomain%Boundary%TBoundNodID(i,1) )=obj%FEMDomain%Boundary%TboundVal(i,1)
                endif
            enddo
        elseif(obj%step>=2)then
            !=====================================
            ! Export Values to Element-by-Element form
            !n=size(obj%FEMDomain%Mesh%ElemNod,1)    
            !m=size(obj%FEMDomain%Mesh%ElemNod,2)
            !do i=1,n
            !    do j=1,m
            !        nodeid1=obj%FEMDomain%Mesh%ElemNod(i,j)
            !        xvec(nodeid1)=obj%UnknownValue(i,j)
            !    enddo
            !enddo
            !=====================================
        else
            print *, "ERROR :: Diffusion%solve() >> not initialized by %setup()"
            stop 
        endif
    endif
    
    !===================================
    ! show initial value
    ! 
    if(allocated(obj%FEMDomain%Boundary%TboundNodID) )then
        
        if(obj%step==1)then
            n=size(obj%FEMDomain%Mesh%ElemNod,1)    
            m=size(obj%FEMDomain%Mesh%ElemNod,2)
            do i=1,n
                do j=1,m
                    nodeid1=obj%FEMDomain%Mesh%ElemNod(i,j)
                    obj%UnknownValue(i,j)=xvec(nodeid1)
                enddo
            enddo
            call obj%Display(DisplayMode="Gmsh",OptionalStep=0)

        elseif(obj%step>=2)then
            !=====================================
            ! Export Values to Element-by-Element form
            !n=size(obj%FEMDomain%Mesh%ElemNod,1)    
            !m=size(obj%FEMDomain%Mesh%ElemNod,2)
            !do i=1,n
            !    do j=1,m
            !        nodeid1=obj%FEMDomain%Mesh%ElemNod(i,j)
            !        xvec(nodeid1)=obj%UnknownValue(i,j)
            !    enddo
            !enddo
            !=====================================
        else
            print *, "ERROR :: Diffusion%solve() >> not initialized by %setup()"
            stop 
        endif
    endif
    !===================================


    !===================================
    ! introduce D.B.C
    n=size(obj%FEMDomain%Boundary%DBoundNodID,1)
    do i=1,n
        nodeid1=obj%FEMDomain%Boundary%DBoundNodID(i,1)
        if(nodeid1<1)then
            cycle
        else
            val=obj%FEMDomain%Boundary%DBoundVal(i,1)
            
            do j=1,size(bvec)
                bvec(j)=bvec(j)-Amat(nodeid1,j)*val
            enddo
            Amat(nodeid1,:)         = 0.0d0
            Amat(:,nodeid1)         = 0.0d0
            Amat(nodeid1,nodeid1)   = 1.0d0
            bvec(nodeid1)           = val
            xvec(nodeid1)           = val
        endif
    enddo
    !===================================
    
    
        
    
    
    itrmax=1000
    er=1.0e-15
    n=size(bvec)
    
    
    if(trim(solver)=="GaussJordan")then
        print *, "Solver type :: GaussJordan" 
        call  gauss_jordan_pv(Amat, xvec, bvec,size(xvec) )
    elseif(trim(solver)=="BiCGSTAB")then
        print *, "Solver type :: BiCGSTAB"
        call bicgstab_Diffusion(Amat, bvec, xvec, size(xvec), itrmax, er,obj%FEMDomain%Boundary%DBoundNodID,&
        obj%FEMDomain%Boundary%DBoundVal)
    else
        print *, "Critical ERROR :: No solvers are selected in DiffusionEqCl"
        stop 
    endif
    
    
    !=====================================
    ! Export Values to Element-by-Element form
    n=size(obj%FEMDomain%Mesh%ElemNod,1)    
    m=size(obj%FEMDomain%Mesh%ElemNod,2)
    do i=1,n
        do j=1,m
            nodeid1=obj%FEMDomain%Mesh%ElemNod(i,j)
            obj%UnknownValue(i,j)=xvec(nodeid1)
        enddo
    enddo
    !=====================================
    
    
end subroutine

!######################## Solve DiffusionEq ########################






!######################## Initialize DiffusionEq ########################
subroutine UpdateDiffusionEq(obj)
    class(DiffusionEq_),intent(inout)::obj

    obj%step=obj%step+1
    call UpdateUnknownValue(obj)
    call GetDiffusionMat(obj)
    call GetFlowvector(obj)
    
end subroutine
!######################## Initialize DiffusionEq ########################





!######################## Initialize DiffusionEq ########################
subroutine SetupDiffusionEq(obj)
    class(DiffusionEq_),intent(inout)::obj

    obj%step=1
    call GetUnknownValue(obj)
    call GetDivergence(obj)
    call GetDiffusionMat(obj)
    call GetFlowvector(obj)
    
    
end subroutine
!######################## Initialize DiffusionEq ########################







!######################## Assemble Element Matrix ##########################
subroutine GetDiffusionMat(obj)
    class(DiffusionEq_),intent(inout)::obj

    integer :: nod_num,dim_num,elemnod_num,elem_num
    integer :: i,j,k
    real(8) :: diff_coeff
    real(8),allocatable::DiffMat(:,:),MassMat(:,:),Cvec(:),Flux(:)
 
    nod_num     =   size(obj%FEMDomain%Mesh%NodCoord,1)
    elem_num    =   size(obj%FEMDomain%Mesh%ElemNod,1)
    elemnod_num =   size(obj%FEMDomain%Mesh%ElemNod,2)
    dim_num     =   size(obj%FEMDomain%Mesh%NodCoord,2)

    allocate(Cvec(elemnod_num) ,Flux(dim_num))

    if( .not. allocated( obj%FlowVector  ) ) allocate(obj%FlowVector(elem_num,elemnod_num) )
    if( .not. allocated( obj%UnknownValue) ) allocate(obj%UnknownValue(elem_num,elemnod_num) )
    if( .not. allocated( obj%DiffusionMat) ) allocate(obj%DiffusionMat(elem_num,elemnod_num,elemnod_num) )
    if( .not. allocated( obj%FluxVector3D) ) allocate(obj%FluxVector3D(elem_num,3) )
    obj%FlowVector(:,:)=0.0d0
    obj%FluxVector3D(:,:)=0.0d0
    obj%DiffusionMat(:,:,:)=0.0d0


    obj%FEMDomain%ShapeFunction%ElemType=obj%FEMDomain%Mesh%ElemType
    call SetShapeFuncType(obj%FEMDomain%ShapeFunction)


    do i=1,elem_num
        Cvec(:)=obj%UnknownValue(i,:)
        do j=1,obj%FEMDomain%ShapeFunction%NumOfGp

            diff_coeff=obj%FEMDomain%MaterialProp%MatPara(obj%FEMDomain%Mesh%ElemMat(i),1)
            
            call getAllShapeFunc(obj%FEMDomain%ShapeFunction,elem_id=i,nod_coord=obj%FEMDomain%Mesh%NodCoord,&
                elem_nod=obj%FEMDomain%Mesh%ElemNod,OptionalGpID=j)
            call getElemDiffusionMatrix(obj%FEMDomain%ShapeFunction,diff_coeff,DiffMat)  
            call getElemFluxVec(obj%FEMDomain%ShapeFunction,diff_coeff,Flux,Cvec)      
            call getElemMassMatrix(obj%FEMDomain%ShapeFunction,MassMat)
            
            do k=1,size(DiffMat,1)
                obj%DiffusionMat(i,k,:)=obj%DiffusionMat(i,k,:)-DiffMat(k,:)&
                +2.0d0/obj%dt*MassMat(k,:)    
            enddo

            obj%FluxVector3D(i,:)=obj%FluxVector3D(i,:)+Flux(:) 
            
            
            DiffMat(:,:)=0.0d0
            MassMat(:,:)=0.0d0

            
        enddo
    enddo
    

end subroutine
!######################## Assemble Element Matrix ##########################




!######################## Get Flow-vector ##########################
subroutine GetFlowvector(obj)
    class(DiffusionEq_),intent(inout)::obj

    integer ::  i,j,k,n,m,num_of_nbc,node_id,num_of_elem,num_of_elemnod
    real(8),allocatable::MassMat(:,:),RHSvector(:)
    
    obj%Flowvector(:,:)=0.0d0
    num_of_elem=size(obj%FEMDomain%Mesh%ElemNod,1)
    num_of_elemnod=size(obj%FEMDomain%Mesh%ElemNod,2)
    num_of_nbc=size(obj%FEMDomain%Boundary%NBoundNodID,1)
    
    if(.not.allocated(obj%Flowvector) )then
        allocate(obj%Flowvector(size(obj%FEMDomain%Mesh%ElemNod,1),size(obj%FEMDomain%Mesh%ElemNod,2)  )  )
    else
        if(size(obj%Flowvector,1)/=num_of_elem .or. size(obj%Flowvector,2)/=num_of_elemnod )then
            deallocate(obj%Flowvector)
            allocate(obj%Flowvector(size(obj%FEMDomain%Mesh%ElemNod,1),size(obj%FEMDomain%Mesh%ElemNod,2)  )  )
        endif
        
    endif

    !get scalar value

    do i=1,num_of_nbc
        node_id=obj%FEMDomain%Boundary%NBoundNodID(i,1)
        n=0
        if(node_id>0)then
            do j=1,num_of_elem
                do k=1,num_of_elemnod
                    if(node_id==obj%FEMDomain%Mesh%ElemNod(j,k)  )then
                       obj%Flowvector(j,k) = -obj%FEMDomain%Boundary%NBoundVal(i,1)

                       n=1
                       exit
                    endif
                enddo
                if(n==1)then
                    exit
                endif
            enddo
        else
            cycle
        endif
    enddo 

    !Add time integration terms
    obj%FEMDomain%ShapeFunction%ElemType=obj%FEMDomain%Mesh%ElemType
    call SetShapeFuncType(obj%FEMDomain%ShapeFunction)
    allocate(RHSvector(num_of_elemnod ) )

    do i=1,num_of_elem
        do j=1,obj%FEMDomain%ShapeFunction%NumOfGp
            call GetAllShapeFunc(obj%FEMDomain%ShapeFunction,elem_id=i,nod_coord=obj%FEMDomain%Mesh%NodCoord,&
                elem_nod=obj%FEMDomain%Mesh%ElemNod,OptionalGpID=j)
            call GetElemMassMatrix(obj%FEMDomain%ShapeFunction,MassMat)
            RHSvector(:)=2.0d0/obj%dt*obj%UnknownValueInit(i,:)+obj%UnknownValueRate(i,:)
            
            obj%Flowvector(i,:)=obj%Flowvector(i,:)+matmul(MassMat,RHSvector)+obj%Divergence(i,:)
            
            MassMat(:,:)=0.0d0

            
        enddo
    enddo


end subroutine GetFlowvector
!######################## Get Flow-vector ##########################



!######################## Get UnknownValue ##########################
subroutine GetUnknownValue(obj)
    class(DiffusionEq_),intent(inout)::obj

    integer :: i,j,k,n,m,nodeid
    real(8) :: val
    
    n=size(obj%FEMDomain%Mesh%ElemNod,1)
    m=size(obj%FEMDomain%Mesh%ElemNod,2)
    if(.not.allocated(obj%UnknownValue) )then
        allocate(obj%UnknownValue(n,m) )
    elseif( size(obj%UnknownValue,1)/=n .or. size(obj%UnknownValue,1)/=m )then
        deallocate(obj%UnknownValue)
        allocate(obj%UnknownValue(n,m) )
    else
    endif
    obj%UnknownValue(:,:)=0.0d0

    if(.not.allocated(obj%UnknownValueInit) )then
        allocate(obj%UnknownValueInit(n,m) )
    elseif( size(obj%UnknownValueInit,1)/=n .or. size(obj%UnknownValueInit,1)/=m )then
        deallocate(obj%UnknownValueInit)
        allocate(obj%UnknownValueInit(n,m) )
    else
    endif

    obj%UnknownValueInit(:,:)=0.0d0
    
    if(allocated(obj%FEMDomain%Boundary%TBoundNum ) )then
        if(obj%FEMDomain%Boundary%TBoundNum(1)/=0 )then
            do i=1,obj%FEMDomain%Boundary%TBoundNum(1)
                nodeid=obj%FEMDomain%Boundary%TBoundNodID(i,1)
                val   =obj%FEMDomain%Boundary%TBoundVal(i,1)
                do j=1, size(obj%FEMDomain%Mesh%ElemNod,1)
                    do k=1, size(obj%FEMDomain%Mesh%ElemNod,2)
                        if(obj%FEMDomain%Mesh%ElemNod(j,k)==nodeid )then
                            obj%UnknownValueInit(j,k)=val            
                        endif
                    enddo
                enddo
            enddo
        endif
    endif

    if(.not.allocated(obj%UnknownValueRate) )then
        allocate(obj%UnknownValueRate(n,m) )
    elseif( size(obj%UnknownValueRate,1)/=n .or. size(obj%UnknownValueRate,1)/=m )then
        deallocate(obj%UnknownValueRate)
        allocate(obj%UnknownValueRate(n,m) )
    else
    endif

    obj%UnknownValue(:,:)=obj%UnknownValueInit(:,:)
    obj%UnknownValueRate(:,:)=0.0d0


end subroutine GetUnknownValue
!######################## Get UnknownVector ##########################



!######################## Get UnknownValue ##########################
subroutine GetDivergence(obj)
    class(DiffusionEq_),intent(inout)::obj

    integer :: i,j,k,n,m,nodeid
    real(8) :: val
    
    n=size(obj%FEMDomain%Mesh%ElemNod,1)
    m=size(obj%FEMDomain%Mesh%ElemNod,2)
    if(.not.allocated(obj%Divergence) )then
        allocate(obj%Divergence(n,m) )
    elseif( size(obj%Divergence,1)/=n .or. size(obj%Divergence,1)/=m )then
        deallocate(obj%Divergence)
        allocate(obj%Divergence(n,m) )
    else
    endif
    obj%Divergence(:,:)=0.0d0

    

end subroutine 
!######################## Get UnknownVector ##########################

!######################## Update UnknownVector ##########################
subroutine  UpdateUnknownValue(obj)
    class(DiffusionEq_),intent(inout)::obj

    obj%UnknownValueRate(:,:)=-obj%UnknownValueRate(:,:)+&
        2.0d0/obj%dt*(obj%UnknownValue(:,:)-obj%UnknownValueInit(:,:) )
    obj%UnknownValueInit(:,:)=obj%UnknownValue(:,:)
        
end subroutine
!######################## Update UnknownVector ##########################




!################## Elemental Entities ##################
subroutine GetElemDiffusionMatrix(obj,diff_coeff,DiffMat)
    class(ShapeFunction_),intent(inout)::obj
    real(8),intent(in)::diff_coeff
    real(8),allocatable,intent(inout)::DiffMat(:,:)
    integer :: i,j,n
    real(8) :: signm_modifier
    n=size(obj%dNdgzi,2)
    if(size(DiffMat,1)/=n .or.size(DiffMat,2)/=n )then
        if(allocated(DiffMat)) then
            deallocate(DiffMat)
        endif
        allocate(DiffMat(n,n) )
    endif


    ! diff_coeff should be negative
    if( diff_coeff > 0.0d0)then
        signm_modifier=-1.0d0
    else
        signm_modifier=1.0d0
    endif

    DiffMat(:,:)=0.0d0
    
    DiffMat(:,:)=matmul( transpose(matmul(obj%JmatInv,obj%dNdgzi)),&
    matmul(obj%JmatInv,obj%dNdgzi))*signm_modifier*diff_coeff*det_mat(obj%JmatInv,size(obj%JmatInv,1) )
end subroutine GetElemDiffusionMatrix
!################## Elemental Entities ##################


!################## Elemental Entities ##################
subroutine getElemFluxVec(obj,diff_coeff,Flux,Cvec)
    class(ShapeFunction_),intent(inout)::obj
    real(8),intent(in)::diff_coeff
    real(8),intent(in)::Cvec(:)
    real(8),allocatable,intent(inout)::Flux(:)
    integer :: i,j,n
    real(8) :: signm_modifier
    n=size(obj%dNdgzi,2)
    if(size(Flux,1)/=n )then
        if(allocated(Flux)) then
            deallocate(Flux)
        endif
        allocate(Flux(n) )
    endif


    ! diff_coeff should be negative
    if( diff_coeff > 0.0d0)then
        signm_modifier=-1.0d0
    else
        signm_modifier=1.0d0
    endif

    Flux(:)=0.0d0


    Flux(:)=signm_modifier*diff_coeff*det_mat(obj%Jmat,size(obj%Jmat,1) )&
    *matmul( obj%JmatInv,matmul(obj%dNdgzi,cvec) )

end subroutine GetElemFluxVec
!################## Elemental Entities ##################



!################## Elemental Entities ##################
subroutine GetElemMassMatrix(obj,MassMat)
    class(ShapeFunction_),intent(inout)::obj
    real(8),allocatable,intent(inout)::MassMat(:,:)
    integer :: i,j,n

    n=size(obj%dNdgzi,2)
    if(.not.allocated(MassMat) )allocate(MassMat(n,n) )
    if(size(MassMat,1)/=n .or.size(MassMat,2)/=n )then
        if(allocated(MassMat)) then
            deallocate(MassMat)
        endif
        allocate(MassMat(n,n) )
    endif

    MassMat(:,:)=0.0d0
    MassMat(:,:)=diadic( obj%Nmat,obj%Nmat ) *det_mat(obj%Jmat,size(obj%Jmat,1) )

    
end subroutine GetElemMassMatrix
!################## Elemental Entities ##################




!################## Elemental Entities ##################
subroutine DisplayDiffusionEq(obj,OptionalProjectName,DisplayMode,OptionalStep,Name)
    class(DiffusionEq_),intent(inout)::obj
    character(*),optional,intent(in) :: OptionalProjectName,DisplayMode,Name
    logical::withMsh
    integer,optional,intent(in)::OptionalStep
    integer :: i,j,n,m,step

    if(present(OptionalStep) )then
        step=OptionalStep
    else
        step=1
        
    endif

    if(step==1)then
        withMsh=.true.
    else
        withMsh=.false.
    endif

    if(present(DisplayMode) )then
        if(trim(DisplayMode)=="Terminal" .or. trim(DisplayMode)=="terminal")then
            do i=1,size(obj%UnknownValue,1)
                print *, obj%UnknownValue(i,:)
            enddo
        elseif(trim(DisplayMode)=="gmsh" .or. trim(DisplayMode)=="Gmsh" )then   
            call GmshPlotContour(obj%FEMDomain,obj%UnknownValue,OptionalStep=step)
            call GmshPlotVector(obj%FEMDomain,Vector=obj%FluxVector3D,Name=trim(obj%FEMDomain%FileName),&
            FieldName="Flux", Step=step,ElementWize=.true.,withMsh=withMsh)
        elseif(trim(DisplayMode)=="gnuplot" .or. trim(DisplayMode)=="Gnuplot" )then
            call GnuplotPlotContour(obj%FEMDomain,obj%UnknownValue,OptionalStep=step)
            
        else
            print *, "Invalid DisplayMode >> DisplayDiffusionEq "
            print *, "DisplayMode '",trim(DisplayMode),"' is not defined" 
        endif
        return
    endif
end subroutine 
!################## Elemental Entities ##################





end module DiffusionEquationClass