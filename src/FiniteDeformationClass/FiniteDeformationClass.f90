
module FiniteDeformationClass
    use, intrinsic :: iso_fortran_env
    use MathClass
	use LinearSolverClass
	use FEMDomainClass
	use PostProcessingClass
	use ConstitutiveModelClass
	implicit none

    type:: FiniteDeform_
		type(FEMDomain_),pointer ::FEMDomain
	    real(real64),allocatable ::DeformStress(:,:,:)
	    real(real64),allocatable ::DeformStrain(:,:,:)
        real(real64),allocatable ::DeformStressInit(:,:,:)
        real(real64),allocatable ::DeformStressinc(:,:,:)
		real(real64),allocatable ::DeformStressMat(:,:,:)
		real(real64),allocatable ::DeformStressRHS(:,:)
		real(real64),allocatable ::DeformVecEBETot(:,:)
		real(real64),allocatable ::DeformVecEBEInc(:,:)
        real(real64),allocatable ::DeformVecGloTot(:)
		real(real64),allocatable ::DeformVecGloInc(:) 
		real(real64),allocatable ::TractionVecGlo(:)
		real(real64),allocatable ::ResidualVecGlo(:)
		real(real64),allocatable ::InternalVecGlo(:)
		real(real64),allocatable ::VolInitCurrEBE(:,:)
		real(real64),allocatable ::YoungsModulus(:) ! directly give parameter #1
		real(real64),allocatable ::PoissonsRatio(:) ! directly give parameter #2
		real(real64),allocatable :: PorePressure(:) ! directly give parameter #2
		real(real64)             ::dt,error,reactionforce
		real(real64)             ::nr_tol=1.0e-8
		logical :: ReducedIntegration = .false.
		logical :: infinitesimal = .false.
		
		integer(int32) :: itr
		integer(int32) :: Step=0
	contains
		procedure :: Solve => SolveFiniteDeformNewton
		procedure :: UpdateSolution => SolveFiniteDeform
		procedure :: DivideBC => DevideBCIntoTimestep
		procedure :: UpdateBC => UpdateBCInTimestep
		procedure :: UpdateInitConfig => UpdateInitConfig
		procedure :: Setup => SetupFiniteDeform
		procedure :: Update => UpdateFiniteDeform
		procedure :: Display => DisplayDeformStress
		procedure :: getDBCVector => getDBCVectorDeform
		procedure :: getDispVector => getDispVectorDeform
		procedure :: getVolume => getVolumeDeform
		procedure :: check => checkFiniteDeform
		procedure :: import => importFiniteDeform
		procedure :: export => exportFiniteDeform
		procedure :: remove => removeFiniteDeform

		procedure :: save => saveFiniteDeform
		procedure :: open => openFiniteDeform
		procedure :: link => linkFiniteDeform
		
  	end type
	
contains


subroutine removeFiniteDeform(obj)
	class(FiniteDeform_),intent(inout) :: obj

	if(associated(obj%FEMDomain) )then
		nullify(obj%FEMDomain)
	endif
	if( allocated(obj%DeformStress)) deallocate(obj%DeformStress)
	if( allocated(obj%DeformStrain)) deallocate(obj%DeformStrain)
	if( allocated(obj%DeformStressInit)) deallocate(obj%DeformStressInit)
	if( allocated(obj%DeformStressinc)) deallocate(obj%DeformStressinc)
	if( allocated(obj%DeformStressMat)) deallocate(obj%DeformStressMat)
	if( allocated(obj%DeformStressRHS)) deallocate(obj%DeformStressRHS)
	if( allocated(obj%DeformVecEBETot)) deallocate(obj%DeformVecEBETot)
	if( allocated(obj%DeformVecEBEInc)) deallocate(obj%DeformVecEBEInc)
	if( allocated(obj%DeformVecGloTot)) deallocate(obj%DeformVecGloTot)
	if( allocated(obj%DeformVecGloInc)) deallocate(obj%DeformVecGloInc)
	if( allocated(obj%TractionVecGlo)) deallocate(obj%TractionVecGlo)
	if( allocated(obj%ResidualVecGlo)) deallocate(obj%ResidualVecGlo)
	if( allocated(obj%InternalVecGlo)) deallocate(obj%InternalVecGlo)
	if( allocated(obj%VolInitCurrEBE)) deallocate(obj%VolInitCurrEBE)
	if( allocated(obj%YoungsModulus))  deallocate(obj%YoungsModulus)
	if( allocated(obj%PoissonsRatio))  deallocate(obj%PoissonsRatio)
	if( allocated(obj% PorePressure))  deallocate(obj% PorePressure)
	obj%dt=1.0d0
	obj%error=0.0d0
	obj%reactionforce=0.0d0
	obj%nr_tol=1.0e-8
	obj%ReducedIntegration = .false.
	obj%infinitesimal = .false.
	
	obj%itr=0
	obj%Step=0
end subroutine

! #######################################################################
subroutine linkFiniteDeform(obj,FEMDomain)
	class(FiniteDeform_),intent(inout) :: obj
	type(FEMDomain_),target,intent(in) :: FEMDomain

	if(associated(obj%FEMDomain) )then
		nullify(obj%FEMDomain)
		obj%FEMDomain => FEMDOmain
	endif

end subroutine
! #######################################################################

! #######################################################################
subroutine openFiniteDeform(obj,path,name)
	class(FiniteDeform_),intent(inout) :: obj
	character(*),intent(in) :: path
	character(*),optional,intent(in) :: name
	character(200) :: pathi
	type(IO_) :: f
	integer(int32) :: n

	if(present(name) )then
		pathi=path
		!if( index(path, "/", back=.true.) == len(path) )then
		!	n=index(path, "/", back=.true.)
		!	pathi(n:n)= " "
		!endif

		call execute_command_line("mkdir -p "//trim(pathi))
		call execute_command_line("mkdir -p "//trim(pathi)//"/"//trim(adjustl(name)) )
		call f%open(trim(pathi)//"/"//trim(adjustl(name)) ,"/"//"FiniteDeform",".prop" )
	else
		pathi=path
		!if( index(path, "/", back=.true.) == len(path) )then
		!	n=index(path, "/", back=.true.)
		!	pathi(n:n)= " "
		!endif
		call execute_command_line("mkdir -p "//trim(pathi))
		call execute_command_line("mkdir -p "//trim(pathi)//"/FiniteDeform")
		call f%open(trim(pathi)//"/FiniteDeform","/FiniteDeform",".prop" )
	endif
	
	! write smt at here!

	call openArray(f%fh, obj%DeformStress)
	call openArray(f%fh, obj%DeformStrain)
	call openArray(f%fh, obj%DeformStressInit)
	call openArray(f%fh, obj%DeformStressinc)
	call openArray(f%fh, obj%DeformStressMat)
	call openArray(f%fh, obj%DeformStressRHS)
	call openArray(f%fh, obj%DeformVecEBETot)
	call openArray(f%fh, obj%DeformVecEBEInc)
	call openArray(f%fh, obj%DeformVecGloTot)
	call openArray(f%fh, obj%DeformVecGloInc) 
	call openArray(f%fh, obj%TractionVecGlo)
	call openArray(f%fh, obj%ResidualVecGlo)
	call openArray(f%fh, obj%InternalVecGlo)
	call openArray(f%fh, obj%VolInitCurrEBE)
	call openArray(f%fh, obj%YoungsModulus) ! directly give parameter #1
	call openArray(f%fh, obj%PoissonsRatio) ! directly give parameter #2
	call openArray(f%fh, obj%PorePressure) ! directly give parameter #2
	read(f%fh,*) obj%dt,obj%error,obj%reactionforce
	read(f%fh,*) obj%nr_tol
	read(f%fh,*) obj%ReducedIntegration 
	read(f%fh,*) obj%infinitesimal 
	
	call f%close()
	
end subroutine 


! #######################################################################
subroutine saveFiniteDeform(obj,path,name)
	class(FiniteDeform_),intent(inout) :: obj
	character(*),intent(in) :: path
	character(*),optional,intent(in) :: name
	character(200) :: pathi
	type(IO_) :: f
	integer(int32) :: n

	if(present(name) )then
		pathi=path
		!if( index(path, "/", back=.true.) == len(path) )then
		!	n=index(path, "/", back=.true.)
		!	pathi(n:n)= " "
		!endif

		call execute_command_line("mkdir -p "//trim(pathi))
		call execute_command_line("mkdir -p "//trim(pathi)//"/"//trim(adjustl(name)) )
		call f%open(trim(pathi)//"/"//trim(adjustl(name)) ,"/"//"FiniteDeform",".prop" )
	else
		pathi=path
		!if( index(path, "/", back=.true.) == len(path) )then
		!	n=index(path, "/", back=.true.)
		!	pathi(n:n)= " "
		!endif
		call execute_command_line("mkdir -p "//trim(pathi))
		call execute_command_line("mkdir -p "//trim(pathi)//"/FiniteDeform")
		call f%open(trim(pathi)//"/FiniteDeform","/FiniteDeform",".prop" )
	endif
	
	! write smt at here!

	call writeArray(f%fh, obj%DeformStress)
	call writeArray(f%fh, obj%DeformStrain)
	call writeArray(f%fh, obj%DeformStressInit)
	call writeArray(f%fh, obj%DeformStressinc)
	call writeArray(f%fh, obj%DeformStressMat)
	call writeArray(f%fh, obj%DeformStressRHS)
	call writeArray(f%fh, obj%DeformVecEBETot)
	call writeArray(f%fh, obj%DeformVecEBEInc)
	call writeArray(f%fh, obj%DeformVecGloTot)
	call writeArray(f%fh, obj%DeformVecGloInc) 
	call writeArray(f%fh, obj%TractionVecGlo)
	call writeArray(f%fh, obj%ResidualVecGlo)
	call writeArray(f%fh, obj%InternalVecGlo)
	call writeArray(f%fh, obj%VolInitCurrEBE)
	call writeArray(f%fh, obj%YoungsModulus) ! directly give parameter #1
	call writeArray(f%fh, obj%PoissonsRatio) ! directly give parameter #2
	call writeArray(f%fh, obj%PorePressure) ! directly give parameter #2
	write(f%fh,*) obj%dt,obj%error,obj%reactionforce
	write(f%fh,*) obj%nr_tol
	write(f%fh,*) obj%ReducedIntegration 
	write(f%fh,*) obj%infinitesimal 
	
	call f%close()
	
end subroutine 
! #######################################################################

! ###############################################################
subroutine importFiniteDeform(obj, YoungsModulus, PoissonsRatio, PorePressure)
	class(FiniteDeform_),intent(inout) :: obj
	real(real64),optional,intent(in) :: YoungsModulus(:), PoissonsRatio(:),&
		PorePressure(:)
	integer(int32) :: i,j,n

	if(present(YoungsModulus) )then
		if(allocated(obj%YoungsModulus) )then
			deallocate(obj%YoungsModulus)
		endif
		n=size(YoungsModulus)
		allocate(obj%YoungsModulus(n) )
		obj%YoungsModulus(:)=YoungsModulus(:)
	endif

	if(present(PoissonsRatio) )then
		if(allocated(obj%PoissonsRatio) )then
			deallocate(obj%PoissonsRatio)
		endif
		n=size(PoissonsRatio)
		allocate(obj%PoissonsRatio(n) )
		obj%PoissonsRatio(:)=PoissonsRatio(:)
	endif

	if(present(PorePressure) )then
		if(allocated(obj%PorePressure) )then
			deallocate(obj%PorePressure)
		endif
		n=size(PorePressure)
		allocate(obj%PorePressure(n) )
		obj%PorePressure(:)=PorePressure(:)
	endif

end subroutine importFiniteDeform
! ###############################################################

! #######################################################
subroutine checkFiniteDeform(obj)
	class(FiniteDeform_),intent(in) :: obj
	integer(int32) :: i,j,error_counter
	! check conditions and return alartes.
	error_counter=0
	print *, "checkFiniteDeform :: Analyzing imported data and checking consistency..."
	if(.not.associated(obj%FEMDomain) )then
		print *, "Alert! checkFiniteDeform >> FEMDomain is not improted."
		stop 
	endif
	if(size(obj%FEMDomain%Boundary%DBoundNodID,2)/=size(obj%FEMDomain%Mesh%NodCoord,2))then
		print *, "size(obj%FEMDomain%Boundary%DBoundNodID,2)/=size(obj%FEMDomain%Mesh%NodCoord,2)"
		stop 
	endif


end subroutine
! #######################################################


!######################## Solve deformation by Netwon's method ########################
subroutine SolveFiniteDeformNewton(obj,OptionItr,Solvertype,nr_tol,infinitesimal,restart)
	class(FiniteDeform_),intent(inout)::obj
	integer(int32),optional,intent(in)::OptionItr
	character(*),optional,intent(in)::Solvertype
	real(real64),optional,intent(in)::nr_tol
	character*70 ::solver,defaultsolver
	logical,optional,intent(in) :: infinitesimal,restart

    real(real64),allocatable::Amat(:,:),bvec(:),xvec(:)
    real(real64)::val,er,residual,tolerance
    integer(int32) ::i,j,n,m,k,l,dim1,dim2,nodeid1,nodeid2,localid,itrmax,SetBC,itr_tol,itr
	integer(int32) :: dim_num,node_num,elem_num,node_num_elmtl
	character*70 :: gmsh,GaussJordan
	logical :: skip=.false.
    gmsh="Gmsh"
	GaussJordan="GaussJordan"
	if(present(infinitesimal) )then
		obj%infinitesimal = infinitesimal
	endif


	if(present(OptionItr) )then
		itr_tol = OptionItr
	else
		itr_tol = obj%FEMDomain%ControlPara%ItrTol
	endif
	if(present(nr_tol) )then
		obj%nr_tol=nr_tol
	endif
	if(present(restart) )then
		if(restart .eqv. .true.)then
			skip=.true.
		endif
	endif
	do
		itr=itr + 1
		obj%Itr=itr
		if(itr==1 .and. skip .eqv. .false.)then
			
			call SetupFiniteDeform(obj)
			if(obj%Step==1)then
				!call DisplayDeformStress(obj,DisplayMode=gmsh,OptionalStep=0)     
			endif

			call SolveFiniteDeform(obj,SolverType=Solvertype)
			!call DisplayDeformStress(obj,DisplayMode=gmsh,OptionalStep=itr)  
			if(obj%infinitesimal .eqv. .true.)then
				exit
			endif

			   
		else

			call UpdateFiniteDeform(obj)

			!call DisplayDeformStress(obj,DisplayMode=gmsh,OptionalStep=0) 
			call SolveFiniteDeform(obj,OptionItr=itr,SolverType=Solvertype)
			!call DisplayDeformStress(obj,DisplayMode=gmsh,OptionalStep=itr)
			

	    	

	
			!print *, "Residual r*r = ",dot_product(obj%ResidualVecGlo,obj%ResidualVecGlo)
			print *, "Step, Itr, ERROR :: ",obj%step ,obj%itr,obj%error
			if(obj%error<obj%nr_tol )then
				print *,"itr=",itr, "Netwton's Method is converged !"
				exit
			endif
		endif



		if(itr == itr_tol)then
			print *, "SolveFiniteDeformNewton >> Newton's method did not converge"
			stop "debug"
			exit
		endif

	enddo
	call UpdateStressMeasure(obj)
	call UpdateStrainMeasure(obj)

	
end subroutine 
!######################## Solve deformation by Netwon's method ########################




!########################## Initialize Boundary Conditions of Finite Deformation  ###################################
subroutine DevideBCIntoTimestep(obj)
	class(FiniteDeform_),intent(inout)::obj

	integer(int32) ::n,m, timestep

	!debug Display Dirichlet Boundary Condition
	!call obj%FEMDomain%GmshPlotMesh(onlyDirichletBC=.true.)
	timestep=obj%FEMDomain%ControlPara%Timestep
	
	if(obj%FEMDomain%ControlPara%SimMode==1)then
		

	
		n=size(obj%FEMDomain%Boundary%DBoundVal,1)
		m=size(obj%FEMDomain%Boundary%DBoundVal,2)

		if(.not.allocated(obj%FEMDomain%Boundary%DBoundValInc ) )then
			allocate(obj%FEMDomain%Boundary%DBoundValInc(n,m) )
		else
			if( size(obj%FEMDomain%Boundary%DBoundValInc,1)/=n .or. &
				size(obj%FEMDomain%Boundary%DBoundValInc,2)/=m)then
				deallocate(obj%FEMDomain%Boundary%DBoundValInc)
			endif
		endif
		obj%FEMDomain%Boundary%DBoundValInc(:,:)=1.0d0/dble(timestep)*obj%FEMDomain%Boundary%DBoundVal(:,:)
		
		
		!obj%FEMDomain%Boundary%DBoundVal(:,:)=obj%FEMDomain%Boundary%DBoundValInc(:,:)
	elseif(obj%FEMDomain%ControlPara%SimMode==2)then
		n=size(obj%FEMDomain%Boundary%NBoundVal,1)
		m=size(obj%FEMDomain%Boundary%NBoundVal,2)
		if(.not.allocated(obj%FEMDomain%Boundary%NBoundValInc ) )then
			allocate(obj%FEMDomain%Boundary%NBoundValInc(n,m) )
		else
			if( size(obj%FEMDomain%Boundary%NBoundValInc,1)/=n .or. &
				size(obj%FEMDomain%Boundary%NBoundValInc,2)/=m)then
				deallocate(obj%FEMDomain%Boundary%DBoundValInc)
			endif
		endif
		
		obj%FEMDomain%Boundary%NBoundValInc(:,:)=1.0d0/dble(timestep)*obj%FEMDomain%Boundary%NBoundVal(:,:)
		obj%FEMDomain%Boundary%NBoundVal(:,:)=obj%FEMDomain%Boundary%NBoundValInc(:,:)
		
	else
		print *, "ERROR :: Displacement Control or Force Control?"
	endif
	
end subroutine 
!########################## Initialize Boundary Conditions of Finite Deformation  ###################################










!########################## Initialize Boundary Conditions of Finite Deformation  ###################################
subroutine UpdateBCInTimestep(obj)
	class(FiniteDeform_),intent(inout)::obj

	integer(int32) :: timestep

	if(obj%FEMDomain%ControlPara%SimMode==1)then
		
		!obj%FEMDomain%Boundary%DBoundVal(:,:)=obj%FEMDomain%Boundary%DBoundValInc(:,:)
		
	elseif(obj%FEMDomain%ControlPara%SimMode==2)then
		
		!obj%FEMDomain%Boundary%NBoundVal(:,:)=obj%FEMDomain%Boundary%NBoundValInc(:,:)
	else
		print *, "ERROR :: Displacement Control or Force Control?"
	endif
	
end subroutine 
!########################## Initialize Boundary Conditions of Finite Deformation  ###################################












!#############################################################
subroutine ImportFEMDomainFiDe(obj,OptionalFileFormat,OptionalProjectName)
    class(FEMDomain_),intent(inout)::obj
    character*4,optional,intent(in)::OptionalFileFormat
    character(*),optional,intent(in)::OptionalProjectName

    character*4::FileFormat
    character*70::ProjectName
    character*74 ::FileName
    integer(int32),allocatable::IntMat(:,:)
    real(real64),allocatable::RealMat(:,:)
    integer(int32) :: fh,i,j,NumOfDomain,n,m,DimNum
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
            obj%Mesh%SubMeshElemFromTo(i,2)=obj%Mesh%SubMeshElemFromTo(i-1,3) + 1
        endif
    enddo
    
    read(fh,*) n,m
    DimNum=m
    allocate(obj%Mesh%NodCoord(n,m) )
    call ImportArray(obj%Mesh%NodCoord,OptionalFileHandle=fh)
	call CopyArray(obj%Mesh%NodCoord,obj%Mesh%NodCoordInit )

    read(fh,*) n,m
    read(fh,*)obj%Mesh%ElemType
    obj%ShapeFunction%ElemType=obj%Mesh%ElemType
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
    DimNum=size(obj%Mesh%NodCoord,2)
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
    if(n==0)then
        allocate( obj%Boundary%NBoundNum(0))
        allocate(obj%Boundary%NBoundNodID(0,0)  )  
        allocate(obj%Boundary%NBoundVal( 0,0)  )  
    else
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
    endif
    !######### Neumann boundary conditions #################




    !######### Initial conditions #################
    read(fh,*) n
    if(n==0)then
        allocate( obj%Boundary%TBoundNum(0))
        allocate(obj%Boundary%TBoundNodID(0,0)  )  
        allocate(obj%Boundary%TBoundVal( 0,0)  )  
    else
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
    endif
    !######### Initial conditions #################
    
    read(fh,*) obj%ControlPara%SimMode, obj%ControlPara%ItrTol,obj%ControlPara%Timestep
    close(fh)
else
    !!print *, "ERROR :: ExportFEMDomain >> only .scf file can be exported."
endif


end subroutine
!#############################################################





!#############################################################
subroutine SetupFiniteDeform(obj,tol)
	class(FiniteDeform_),intent(inout)::obj
	integer(int32),optional,intent(in) :: tol
	
	if(obj%dt==0.0d0 .or. obj%dt/=obj%dt)then
        obj%dt=1.0d0
	endif
	!if(present(tol) )then
	!	obj%nr_tol = tol
	!else
	!	obj%nr_tol = 1.0e-08
	!endif
	
	call UpdateCurrConfig(obj)
    call GetDeformStressMatAndVector(obj)
end subroutine
!#############################################################





!#############################################################
subroutine UpdateFiniteDeform(obj,restart)
	class(FiniteDeform_),intent(inout)::obj
	logical,optional,intent(in) :: restart

	call UpdateCurrConfig(obj)
	
	call GetDeformStressMatAndVector(obj)
	
end subroutine
!#############################################################




!#############################################################
subroutine UpdateCurrConfig(obj,restart)
    class(FiniteDeform_),intent(inout)::obj
	logical,optional,intent(in) :: restart
    integer(int32) :: i,j,n,m
    integer(int32) :: num_node,num_elem,num_dim

    num_node=size(obj%FEMDomain%Mesh%NodCoord,1)
    num_dim =size(obj%FEMDomain%Mesh%NodCoord,2)
	num_elem=size(obj%FEMDomain%Mesh%ElemNod,1)
	! Displacement :: u
    if(.not.allocated(obj%DeformVecGloTot) ) then
        allocate(obj%DeformVecGloTot(num_node*num_dim) )
        obj%DeformVecGloTot(:)=0.0d0
	endif
	! ⊿u = v
    if(.not.allocated(obj%DeformVecGloInc) ) then
        allocate(obj%DeformVecGloInc(num_node*num_dim) )
        obj%DeformVecGloInc(:)=0.0d0
	endif
	if(.not. allocated(obj%FEMDomain%Mesh%NodCoordInit) )then
		allocate(obj%FEMDomain%Mesh%NodCoordInit(num_node,num_dim) )
		obj%FEMDomain%Mesh%NodCoordInit(:,:)=obj%FEMDomain%Mesh%NodCoord(:,:)
	endif
	
	!call showArraySIze(obj%FEMDomain%Mesh%NodCoord)
	!call showArraySIze(obj%FEMDomain%Mesh%NodCoordInit)
	!call showArraySize(obj%DeformVecGloInc)
	!call showArraySize(obj%DeformVecGloTot)
	!stop 
	do i=1,num_node
        do j=1,num_dim
            obj%FEMDomain%Mesh%NodCoord(i,j )=obj%FEMDomain%Mesh%NodCoordInit(i,j) + &
                obj%DeformVecGloInc(num_dim*(i-1) + j) + obj%DeformVecGloTot(num_dim*(i-1) + j)
        enddo
	enddo

	 
end subroutine
!#############################################################


!#############################################################
subroutine UpdateInitConfig(obj)
    class(FiniteDeform_),intent(inout)::obj

    integer(int32) :: i,j,n,m
    integer(int32) :: num_node,num_elem,num_dim

    num_node=size(obj%FEMDomain%Mesh%NodCoord,1)
    num_dim =size(obj%FEMDomain%Mesh%NodCoord,2)
    num_elem=size(obj%FEMDomain%Mesh%ElemNod,1)
    if(.not.allocated(obj%DeformVecGloTot) ) then
        allocate(obj%DeformVecGloTot(num_node*num_dim) )
        obj%DeformVecGloTot(:)=0.0d0
    endif
    if(.not.allocated(obj%DeformVecGloInc) ) then
        allocate(obj%DeformVecGloInc(num_node*num_dim) )
        obj%DeformVecGloInc(:)=0.0d0
    endif

    do i=1,num_node
		do j=1,num_dim
			! Updated Lagrangian
            obj%FEMDomain%Mesh%NodCoordInit(i,j )=obj%FEMDomain%Mesh%NodCoord(i,j)
        enddo
    enddo
end subroutine
!#############################################################



!================================================================================
! �S�̍����}�g���N�X�̌v�Z
  subroutine GetDeformStressMatAndVector(obj,OptionalStep,restart)
		class(FiniteDeform_),intent(inout)::obj
		type(ConstModel_)	::mdl
		logical,optional,intent(in) :: restart
		


	 integer(int32), optional,intent(in) :: OptionalStep
	 integer(int32) :: itr_rm,itr,itr_contact
	 integer(int32) :: nod_num,dim_num,elemnod_num,elem_num
   real(real64), allocatable :: g(:,:), Bmat(:,:), Ce_neoHK(:,:), BTmat(:,:), Psymat(:,:), &
      xymat(:,:),xymat_c(:,:), Jmat(:,:), s(:), Kmat_e(:,:),c_nod_coord(:,:),cc_nod_coord(:,:),&
	  dNdxi(:,:),F_iJ_n(:,:),F_iJ(:,:),C_IJ(:,:),Cp_IJ_n(:,:),Cp_IJ(:,:),b_ij(:,:),F_inv(:,:),&
	  F_T_inv(:,:),F_T(:,:),dNdx(:,:),M_IJ(:,:),Cp_IJ_inv(:,:),gvec_e(:)
	  integer(int32),allocatable::ij(:,:)
   integer(int32) i, j,k, m, p, q,gp_num,NumOfStrainMeasure
	 real(real64) detJ,Lamda,mu,c,phy,psy,E,v,xx,Tol
	 real(real64) ,allocatable::MatPara(:)
	 
	 itr_rm =obj%FEMDomain%ControlPara%ItrTol
	 itr		=obj%itr
	 itr_contact=obj%FEMDomain%ControlPara%ItrTol
	 Tol		=obj%FEMDomain%ControlPara%Tol
	 !if( abs(tol)<1.0e-15 )then
		tol = 1.0e-15
	 !endif
	 
    nod_num     =   size(obj%FEMDomain%Mesh%NodCoord,1)
    elem_num    =   size(obj%FEMDomain%Mesh%ElemNod,1)
    elemnod_num =   size(obj%FEMDomain%Mesh%ElemNod,2)
    dim_num     =   size(obj%FEMDomain%Mesh%NodCoord,2)
    
    if( .not. allocated( obj%DeformVecEBEInc) ) then
        allocate(obj%DeformVecEBEInc(elem_num,elemnod_num*dim_num) )
        obj%DeformVecEBEInc(:,:)=0.0d0
    endif
    if( .not. allocated( obj%DeformVecEBETot) ) then
        allocate(obj%DeformVecEBETot(elem_num,elemnod_num*dim_num) )
        obj%DeformVecEBETot=0.0d0
    endif
    if( .not. allocated( obj%DeformStressRHS) )then
        allocate(obj%DeformStressRHS(elem_num,elemnod_num*dim_num) )
        obj%DeformStressRHS(:,:)=0.0d0
    endif
    if( .not. allocated( obj%DeformStressMat) ) then
        allocate(obj%DeformStressMat(elem_num,elemnod_num*dim_num,elemnod_num*dim_num) )
        obj%DeformStressMat(:,:,:)=0.0d0
    endif
    if( .not. allocated( obj%VolInitCurrEBE) ) then
        allocate(obj%VolInitCurrEBE(elem_num,3) ) !initial, current
        obj%VolInitCurrEBE(:,:)=1.0d0
    endif
    
    obj%DeformStressRHS(:,:)  =0.0d0
	obj%DeformStressMat(:,:,:)=0.0d0
	obj%FEMDomain%ShapeFunction%ElemType=obj%FEMDomain%Mesh%GetElemType()
	call SetShapeFuncType(obj%FEMDomain%ShapeFunction, ReducedIntegration=obj%ReducedIntegration)
	
	
	gp_num=obj%FEMDomain%ShapeFunction%NumOfGp
	if(dim_num<=2)then
		NumOfStrainMeasure=15
	else
		NumOfStrainMeasure=49
	endif
	allocate(MatPara(7))
    if( .not. allocated( obj%DeformStrain )) then
        allocate(obj%DeformStrain(elem_num,gp_num,NumOfStrainMeasure) )
        obj%DeformStrain(:,:,:) =0.0d0  
        obj%DeformStrain(:,:,1) =1.0d0  ! Cp_iJ_n(1,1)
        obj%DeformStrain(:,:,2) =1.0d0  ! Cp_iJ_n(2,2)
        obj%DeformStrain(:,:,4) =1.0d0  ! Cp_iJ(1,1)
        obj%DeformStrain(:,:,5) =1.0d0  ! Cp_iJ(2,2)
        obj%DeformStrain(:,:,7) =1.0d0  ! F_iJ_n(1,1)
        obj%DeformStrain(:,:,8) =1.0d0  ! F_iJ_n(2,2)
        obj%DeformStrain(:,:,11)=1.0d0  ! F_iJ(1,1) 
		obj%DeformStrain(:,:,12)=1.0d0  ! F_iJ(2,2)
		if(NumOfStrainMeasure==49)then
			obj%DeformStrain(:,:,16) = 1.0d0 ! Cp_iJ_n(3,3)
			obj%DeformStrain(:,:,19) = 1.0d0 ! Cp_iJ(3,3)
			obj%DeformStrain(:,:,22) = 1.0d0 ! F_iJ_n(3,3)
			obj%DeformStrain(:,:,27) = 1.0d0 ! F_iJ(3,3)
		endif

    endif	
	!initialize
	if(.not.allocated(obj%DeformStress))then
        allocate(obj%DeformStress(elem_num,gp_num,6) )
		obj%DeformStress(:,:,:)=0.0d0
	endif
	if(.not.allocated(obj%DeformStressinc))then
        allocate(obj%DeformStressinc(elem_num,gp_num,6) )
		obj%DeformStressinc(:,:,:)=0.0d0
	endif
	m = elemnod_num*dim_num
	allocate (Kmat_e(elemnod_num*dim_num,elemnod_num*dim_num),gvec_e(elemnod_num*dim_num))
	obj%VolInitCurrEBE(:,2)=0.0d0        
		do i = 1, elem_num !�v�f���ƃ��[�
			Kmat_e(:,:) = 0.0d0
			gvec_e(:)   = 0.0d0
			E = obj%FEMDomain%MaterialProp%MatPara(obj%FEMDomain%Mesh%ElemMat(i),1)
			v = obj%FEMDomain%MaterialProp%MatPara(obj%FEMDomain%Mesh%ElemMat(i),2)

			! allow direct-import of Youngs modulus and Poissons ratio
			if(allocated(obj%YoungsModulus) )then
				if(size(obj%YoungsModulus)/=elem_num) then
					print *, "ERROR :: FiniteDeform :: size(obj%YoungsModulus/=elem_num)"
				else
					E = obj%YoungsModulus(i)
				endif
			endif

			! allow direct-import of Youngs modulus and Poissons ratio
			if(allocated(obj%PoissonsRatio) )then
				if(size(obj%PoissonsRatio)/=elem_num) then
					print *, "ERROR :: FiniteDeform :: size(obj%PoissonsRatio/=elem_num)"
				else
					v = obj%PoissonsRatio(i)
				endif
			endif
			mdl%Lamda=v*E/(1.0d0 + v)/(1.0d0-2.0d0*v)
			mdl%mu=E/2.0d0/(1.0d0 + v)
			c=obj%FEMDomain%MaterialProp%MatPara(obj%FEMDomain%Mesh%ElemMat(i),4)
			phy=obj%FEMDomain%MaterialProp%MatPara(obj%FEMDomain%Mesh%ElemMat(i),5)
			psy=obj%FEMDomain%MaterialProp%MatPara(obj%FEMDomain%Mesh%ElemMat(i),6)

			MatPara(1)=E
			MatPara(2)=v
			MatPara(3)=Lamda
			MatPara(4)=mu
			MatPara(5)=c
			MatPara(6)=phy
			MatPara(7)=psy

			do j = 1, obj%FEMDomain%ShapeFunction%NumOfGp !�K�E�X�ϕ����ƃ��[�v
				! -----J�}�g���N�X�̌v�Z-----------------------------------------
				call GetAllShapeFunc(obj%FEMDomain%ShapeFunction,elem_id=i,nod_coord=obj%FEMDomain%Mesh%NodCoord,&
				elem_nod=obj%FEMDomain%Mesh%ElemNod,OptionalGpID=j,ReducedIntegration=obj%ReducedIntegration)
				
	
				if(obj%infinitesimal .eqv. .false.)then
					! ---Compute stress/strain measures of the Finite Elasto-Plast-
					call F_tensor_ICU(obj,i,j,mdl%F_iJ_n,mdl%F_iJ)
					


					call trans_rank_2(mdl%F_iJ,mdl%F_T)
					
					
					call C_tensor(mdl%F_iJ,mdl%C_IJ,mdl%b_ij,itr,dim_num)
					
                	!call Cp_tensor(i,j,obj%DeformStrain,mdl%Cp_IJ_n,mdl%Cp_IJ,mdl%Cp_IJ_inv,dim_num)
					
                	call inverse_rank_2(mdl%F_iJ,mdl%F_inv_iJ)
					call inverse_rank_2(mdl%F_T,mdl%F_T_inv_iJ)

                	!call M_neo_Hookean(mdl%C_IJ,mdl%Cp_IJ,mdl%Cp_IJ_inv,mdl%M_IJ,mdl%Lamda,mdl%mu,i,j)
					
					!----Return-Mapping algorithm----------------------------------
					
					!call Return_Mapping_MCDP(dim_num,i,j,mdl%C_IJ,mdl%Cp_IJ,mdl%Cp_IJ_n,mdl%Cp_IJ_inv,mdl%M_IJ,MatPara,&
					!itr_rm,tol,obj%DeformStress,mdl%F_T,mdl%F_T_inv_ij,itr,itr_contact,obj%DeformStrain,OptionalStep)
					
					
					! -----current config. Ce matrix-----------------------------------------
					!call Ce_neoHK_current(dim_num, i,j,mdl%Lamda,mdl%mu,mdl%C_IJ,mdl%Cp_IJ,mdl%b_ij,mdl%M_IJ,Ce_neoHK,mdl%F_T,mdl%F_T_inv,ij)

                	! -----B,W�}�g���N�X�̌v�Z----------------------------------------
					call B_mat(dim_num,obj%FEMDomain%ShapeFunction%dNdgzi, &
						obj%FEMDomain%ShapeFunction%Jmat, obj%FEMDomain%ShapeFunction%detJ, mdl%Bmat,m)
					
					
                	! -----BT�}�g���N�X�̌v�Z-----------------------------------------
					!call trans_rank_2(Bmat,BTmat)
					!-----2-D neo-Hookean current stifness matrix-------------------
					
					! Get Volumetric change
					obj%VolInitCurrEBE(i,2)=obj%VolInitCurrEBE(i,2)+&
						det_mat(mdl%F_iJ,size(mdl%F_iJ,1) )/dble(obj%FEMDomain%ShapeFunction%NumOfGp)
					mdl%ModelType="NeoHookean"
					call GetKmat(obj,mdl,obj%FEMDomain%ShapeFunction,Kmat_e,gvec_e,dim_num,elemnod_num,i,j)
				else
					! infinitesimal strain theory

					! ---Compute stress/strain measures of the Finite Elasto-Plast-
					call F_tensor_ICU(obj,i,j,mdl%F_iJ_n,mdl%F_iJ)
					! -----B,W�}�g���N�X�̌v�Z----------------------------------------
					call B_mat(dim_num,obj%FEMDomain%ShapeFunction%dNdgzi, &
						obj%FEMDomain%ShapeFunction%Jmat, obj%FEMDomain%ShapeFunction%detJ, mdl%Bmat,m)
					call GetKmat(obj,mdl,obj%FEMDomain%ShapeFunction,Kmat_e,gvec_e,dim_num,elemnod_num,i,j)
					
				endif
				!do k=1,size(Kmat_e,1)
				!	write(200,*) Kmat_e(k,:),"|",gvec_e(k)
				!enddo
				!stop "debug"
				!call K_mat_e(j,obj%FEMDomain%ShapeFunction%GaussIntegWei, &
				!	BTmat, Ce_neoHK, Bmat, obj%FEMDomain%ShapeFunction%detJ, Kmat_e,F_iJ)
				!call GetGvec(obj,mdl,obj%FEMDomain%ShapeFunction,gvec_e,dim_num,elemnod_num,i)
				!call g_vector_e(i,j,obj%FEMDomain%ShapeFunction%GaussIntegWei,&
				!  BTmat,obj%DeformStress, obj%FEMDomain%ShapeFunction%detJ, gvec_e)
				
                  ! -----�v�f�����}�g���N�X�̑�������--------------------------------
            enddo
            
			! ------�S�̍����}�g���N�X�ւ̏d�ˍ��킹------------------------------
        
		    
            call K_mat_ICU(obj%DeformStressMat, obj%FEMDomain%Mesh%ElemNod, i, Kmat_e)
            
            call g_vector_ICU(i,obj%FEMDomain%Mesh%ElemNod,gvec_e,obj%DeformStressRHS)
        
		enddo
		
   end subroutine 
!================================================================================

!================================================================================
subroutine GetKmat(obj,mdl,sf,Kmat_e,gvec_e,dim_num,elemnod_num,elem,gp)
	type(FiniteDeform_),intent(inout) :: obj
	type(ConstModel_),intent(inout)::mdl
	type(ShapeFunction_),intent(in)::sf
	real(real64),intent(inout) :: Kmat_e(:,:),gvec_e(:)
	integer(int32),intent(in)::dim_num,elemnod_num,elem,gp
	real(real64):: a0(3,3),dXdgzi(3,3)
	real(real64):: Xmat(elemnod_num,3)
	real(real64),allocatable:: a0_inv(:,:), dgzidX(:,:),Jgzimat_inv(:,:),Dmat(:,:),Sigma(:),Sigma_i(:,:),&
		dumat_i(:,:),Sigma_tot(:,:)
	real(real64):: a1(elemnod_num,3)
	real(real64):: K1(elemnod_num*dim_num,elemnod_num*dim_num)
	real(real64):: a2(elemnod_num,3)
	real(real64):: A3(elemnod_num,elemnod_num)
	real(real64)::dumat(elemnod_num,3)
	real(real64):: a4(elemnod_num,3,elemnod_num,3)
	real(real64)::Jgzimat(3,3),dxxdgzi(3,3),dNdX(elemnod_num,3)
	
	integer(int32) ::alpha,beta,ganma,ii,I,J,jj,K,kk,L,ll,n,m,o,p,q,rr,R,s,loc_1,loc_2
	real(real64) :: detJgzi
	real(real64) :: PorePressure
	character*70 :: DerType
	!DerType="F_iJ"
	DerType="c_current"

	allocate(dumat_i(elemnod_num*3,1) )
	!mdl%infinitesimal = obj%infinitesimal
	n=size(sf%ElemCoord,1)
	m=size(sf%ElemCoord,2)
	do i=1,n !num of node per elems
		do j=1,m ! num of dim
			dumat(i,j)=obj%DeformVecEBEInc(elem, m*(i-1) + j )
			dumat_i( (i-1)*3 + j ,1) =dumat(i,j)
		enddo
	enddo
	!Xmat(:,:)   =sf%ElemCoord(:,:) !- dumat(:,:)
	dXdgzi(:,:)=matmul(sf%dNdgzi,sf%ElemCoord)
	!dXdgzi(:,:)=matmul(sf%dNdgzi,Xmat)
	call inverse_rank_2(dXdgzi,dgzidX)
	dNdX(:,:) =matmul(transpose(sf%dNdgzi),dgzidX )
	!a0(:,:)=matmul(sf%dNdgzi,sf%ElemCoord) !dxdgzi
	Jgzimat(:,:)=matmul(mdl%F_iJ,dXdgzi)
	call inverse_rank_2(Jgzimat,Jgzimat_inv)
	detJgzi=det_mat(Jgzimat,size(Jgzimat,1) )

	if(obj%infinitesimal .eqv. .false.)then
		
		!call inverse_rank_2(a0,a0_inv)
		call HyperElasticStress(mdl)
		call HyperElasticDer(mdl,DerType) !get cc mat  
		call GetDmat(Dmat,mdl%StressDer,dim_num)
		call GetSigmaVec(Sigma,mdl%sigma,dim_num)
		
		K1(:,:)=0.0d0
		A3(:,:)=matmul( matmul( dNdX  , mdl%sigma )      ,   transpose(dNdX))   
		do i=1,elemnod_num
			do j=1,elemnod_num
				K1( (I-1)*dim_num+1,(J-1)*dim_num+1 ) =K1( (I-1)*dim_num+1,(J-1)*dim_num+1 ) + A3(I,J)
				K1( (I-1)*dim_num+2,(J-1)*dim_num+2 ) =K1( (I-1)*dim_num+2,(J-1)*dim_num+2 ) + A3(I,J)
				K1( (I-1)*dim_num+3,(J-1)*dim_num+3 ) =K1( (I-1)*dim_num+3,(J-1)*dim_num+3 ) + A3(I,J) 
			enddo
		enddo
		! debug
		!print *, "size(obj%PorePressure)" ,size(obj%PorePressure)
		!write(3001,*) maxval(obj%PorePressure),minval(obj%PorePressure)
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		! For water-soil coupling analysis

		if(.not. allocated(obj%PorePressure) )then
			PorePressure=0.0d0
		elseif(size(obj%PorePressure) ==0 )then
			PorePressure=0.0d0
		else
			PorePressure=obj%PorePressure(elem)
		endif
		Sigma(1)=Sigma(1)-PorePressure/dble(obj%FEMDomain%ShapeFunction%NumOfGp)
		Sigma(2)=Sigma(2)-PorePressure/dble(obj%FEMDomain%ShapeFunction%NumOfGp)
		Sigma(3)=Sigma(3)-PorePressure/dble(obj%FEMDomain%ShapeFunction%NumOfGp)
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		

		Kmat_e(:,:)=Kmat_e(:,:)+matmul(matmul(transpose(mdl%Bmat),Dmat),mdl%Bmat )/det_mat(mdl%F_ij,size(mdl%F_ij,1) )*sf%detJ &
			 +K1(:,:)*sf%detJ
		!stop "debug l3"
		gvec_e(:)=gvec_e(:)+matmul(transpose(mdl%Bmat),Sigma)*sf%detJ
	else
		!call inverse_rank_2(a0,a0_inv)
		!call HyperElasticStress(mdl)
		!call HyperElasticDer(mdl,DerType) !get cc mat  
		!call GetDmat(Dmat,mdl%StressDer,dim_num)
		!call GetSigmaVec(Sigma,mdl%sigma,dim_num)
		
		! clear Dmat and import St. Venant stiffness matrix
		allocate(Dmat(6,6) )
		Dmat(:,:)=0.0d0
		Dmat(1,1)= 2.0d0*mdl%mu + mdl%lamda
		Dmat(1,2)= mdl%lamda
		Dmat(1,3)= mdl%lamda
		Dmat(2,1)= mdl%lamda
		Dmat(2,2)= 2.0d0*mdl%mu + mdl%lamda
		Dmat(2,3)= mdl%lamda
		Dmat(3,1)= mdl%lamda
		Dmat(3,2)= mdl%lamda
		Dmat(3,3)= 2.0d0*mdl%mu + mdl%lamda
		Dmat(4,4)= mdl%mu
		Dmat(5,5)= mdl%mu
		Dmat(6,6)= mdl%mu

		! stress dsigma = [D][B]{du}
		allocate(Sigma_i(6,1))
		allocate(Sigma_tot(6,1))
		Sigma_i(:,:) = matmul(Dmat, matmul(mdl%Bmat, dumat_i) ) !\partial \sigma / \artial t * dt
		Sigma_tot(1:6,1)  =obj%DeformStress(elem,gp,1:6)
		! debug
		!print *, "size(obj%PorePressure)" ,size(obj%PorePressure)
		!write(3001,*) maxval(obj%PorePressure),minval(obj%PorePressure)
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		! For water-soil coupling analysis

		if(.not. allocated(obj%PorePressure) )then
			PorePressure=0.0d0
		elseif(size(obj%PorePressure) ==0 )then
			PorePressure=0.0d0
		else
			PorePressure=obj%PorePressure(elem)
		endif
		Sigma_tot(1,1)=Sigma_tot(1,1)-PorePressure!/dble(obj%FEMDomain%ShapeFunction%NumOfGp)
		Sigma_tot(2,1)=Sigma_tot(2,1)-PorePressure!/dble(obj%FEMDomain%ShapeFunction%NumOfGp)
		Sigma_tot(3,1)=Sigma_tot(3,1)-PorePressure!/dble(obj%FEMDomain%ShapeFunction%NumOfGp)
		!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		Kmat_e(:,:)=Kmat_e(:,:)+matmul(matmul(transpose(mdl%Bmat),Dmat),mdl%Bmat )*sf%detJ 
		!stop "debug l3"
		gvec_e(:)=gvec_e(:)+matmul(transpose(mdl%Bmat),Sigma_tot(:,1) )*sf%detJ&
			+matmul(transpose(mdl%Bmat),Sigma_i(:,1) )*sf%detJ
		! update d \sigma
		obj%DeformStressinc(elem,gp,1:6)=Sigma_i(1:6,1)
	endif


	!
	!do I=1,elemnod_num
	!	do ii=1,3
	!		do K=1,elemnod_num
	!			do kk=1,3
	!				do R=1,3
	!					do j=1,3
	!						a4(II,ii,K,kk)=a4(II,ii,K,kk)&
	!						+a1(I,j)*mdl%StressDer(ii,j,kk,R)*dNdX(K,R)*detJgzi
	!					enddo
	!				enddo
	!			enddo
	!		enddo
	!	enddo
	!enddo
!
	!do I=1,elemnod_num
	!	do ii=1,3
	!		do K=1,elemnod_num
	!			do kk=1,3
	!				loc_1=(I - 1)*3+ii
	!				loc_2=(K - 1)*3+kk
	!				a0(:,:)=matmul(mdl%F_inv_iJ,mdl%sigma)
	!				a1(:,:)=matmul(dNdX,a0)
	!				a2(:,:)=matmul(dNdX,mdl%F_inv_iJ)
	!				Kmat_e(loc_1,loc_2)=Kmat_e(loc_1,loc_2)&
	!				-a1(K,ii)*a2(I,kk)*detJgzi
!
	!				a0(:,:)=matmul(mdl%F_inv_iJ,mdl%sigma)
	!				a1(:,:)=matmul(dNdX,a0)
	!				a2(:,:)=matmul(dNdX,dXdgzi)
	!				a3(:,:)=matmul(a2,Jgzimat_inv)
	!				Kmat_e(loc_1,loc_2)=Kmat_e(loc_1,loc_2)&
	!				+a1(I,ii)*a3(K,kk)*detJgzi
	!				
	!				Kmat_e(loc_1,loc_2)=Kmat_e(loc_1,loc_2)&
	!				+a4(II,ii,K,kk)*detJgzi
	!			enddo
	!		enddo
	!	enddo
	!enddo	
	!
	!do i=1,size(Kmat_e,1)
	!	write(10,*) Kmat_e(i,:)
	!enddo
	!write (10,*) " "
!	do alpha=1,elemnod_num
!		do ganma=1,3
!			do I=1,elemnod_num
!				do ii=1,3
!					do m=1,3
!						do j=1,3
!							a4(alpha,ganma,I,ii)=a4(alpha,ganma,I,ii)&
!							+a1(alpha,j)*a3(I,m)*mdl%StressDer(ii,j,ganma,m)*sf%detJ
!						enddo
!					enddo
!				enddo
!			enddo
!		enddo
!	enddo
!
!	do alpha=1,elemnod_num
!		do ganma=1,3
!			do I=1,elemnod_num
!				do ii=1,3
!					loc_1=(alpha-1)*3+ii
!					loc_2=(I - 1)*3+ganma
!					Kmat_e(loc_1,loc_2)=Kmat_e(loc_1,loc_2)&
!					-a1(alpha,ganma)*a2(I,ii)*sf%detJ&
!					+a2(alpha,ii)*a1(I,ganma)*sf%detJ&
!					-a4(alpha,ganma,I,ii)
!				enddo
!			enddo
!		enddo
!	enddo
!


end subroutine
!=================================================================================


!================================================================================
subroutine GetGvec(obj,mdl,sf,gvec_e,dim_num,elemnod_num,elem)
	type(FiniteDeform_),intent(in) :: obj
	type(ConstModel_),intent(inout)::mdl
	type(ShapeFunction_),intent(in)::sf
	real(real64),intent(inout) :: gvec_e(:)
	integer(int32),intent(in)::dim_num,elemnod_num,elem

	real(real64):: a0(3,3),Jgzimat(3,3),dXdgzi(3,3)
	real(real64),allocatable:: a0_inv(:,:),dgzidX(:,:)
	real(real64):: a1(elemnod_num,3)
	real(real64):: a2(elemnod_num,3)
	real(real64):: dumat(elemnod_num,3)
	real(real64):: Xmat(elemnod_num,3)
	real(real64):: a3(elemnod_num,3)
	real(real64):: dNdX(elemnod_num,3)
	real(real64):: a4(elemnod_num,3,elemnod_num,3),detJgzi
	
	integer(int32) ::alpha,beta,ganma,ii,I,j,k,l,m,o,p,q,r,s,loc_1,n
	character*70 :: DerType
	DerType="F_iJ"


	n=size(sf%ElemCoord,1)
	m=size(sf%ElemCoord,2)
	do i=1,n !num of node per elems
		do j=1,m ! num of dim
			dumat(i,j)=obj%DeformVecEBEInc(elem, m*(i-1) + j )
		enddo
	enddo
	Xmat(:,:)   =sf%ElemCoord(:,:) - dumat(:,:)
	dXdgzi(:,:)=matmul(sf%dNdgzi,Xmat)

	a0(:,:)=matmul(sf%dNdgzi,sf%ElemCoord)
	call inverse_rank_2(a0,a0_inv)
	call HyperElasticStress(mdl)
	call HyperElasticDer(mdl,DerType)
	a1(:,:)=matmul(transpose(sf%dNdgzi),a0)
	a2(:,:)=matmul(a1,mdl%sigma)
	a3(:,:)=matmul(a1,mdl%F_ij)
	a4(:,:,:,:)=0.0d0
	Jgzimat(:,:)=matmul(mdl%F_iJ,dXdgzi)
	detJgzi=det_mat(Jgzimat,size(Jgzimat,1) )
	dXdgzi(:,:)=matmul(sf%dNdgzi,Xmat)
	call inverse_rank_2(dXdgzi,dgzidX)
	dNdX(:,:) =matmul(transpose(sf%dNdgzi),dgzidX )

	a1(:,:)=matmul(dNdX,mdl%F_inv_iJ)

	do I=1,elemnod_num
		do ii=1,3
			do j=1,3
				loc_1=(I-1)*3+ii
				gvec_e(loc_1)=gvec_e(loc_1)&
				+a1(I,j)*mdl%sigma(ii,j)*detJgzi
			enddo
		enddo
	enddo
	
	


end subroutine



!=================================================================================


!=================================================================================
! K�}�g���N�X�ւ̏d�ˍ��킹
subroutine K_mat_ICU(Kmat, elem_nod, i, Kemat)
   	integer(int32), intent(in) :: i, elem_nod(:,:)
   	real(real64), intent(in) :: Kemat(:,:)
	real(real64), intent(inout) :: Kmat(:,:,:)
	   
	Kmat(i,:,:)=Kmat(i,:,:) + Kemat(:,:)
	   
   end subroutine
!=================================================================================


!=================================================================================
subroutine g_vector_ICU(elem,elem_nod,gvec_e,gvec)

	integer(int32), intent(in) :: elem_nod(:,:),elem
	real(real64),intent(in):: gvec_e(:)
	real(real64),intent(inout)::gvec(:,:)
	
	gvec(elem,:)=gvec_e(:)

  end subroutine 
!=================================================================================

!=================================================================================
subroutine F_tensor_ICU(obj,elem,gauss,F_iJ_n,F_iJ)
	class(FiniteDeform_),intent(inout)::obj
	integer(int32), intent(in)::elem,gauss

	real(real64),allocatable:: F_iJ_n(:,:),F_iJ(:,:),dNdx(:,:),x_dNdxi_inv(:,:),x_dNdxi(:,:),&
	dumat_t(:,:),f_n_n_1(:,:),dumat(:,:),x_u(:,:),tr_dNdgzi(:,:),dF_iJ(:,:)
	real(real64)::LungeKutta(3,3),LungeKutta_1(3,3),LungeKutta_2(3,3),LungeKutta_3(3,3),&
	LungeKutta_4(3,3),delta(3,3),dt
	integer(int32) n,m,i,j


	n=size(obj%FEMDomain%ShapeFunction%ElemCoord,1)
	m=size(obj%FEMDomain%ShapeFunction%ElemCoord,2)
	if(.not.allocated(  F_iJ_n  )) allocate(  F_iJ_n(3,3) )
	if(.not.allocated(  f_n_n_1 )) allocate( f_n_n_1(3,3)  )
	if(.not.allocated(  F_iJ    )) allocate( F_iJ(3,3)  )
	if(.not.allocated(  dF_iJ    )) then
	allocate( dF_iJ(3,3)  )
		dF_iJ(:,:)=0.0d0
	endif
	if(.not.allocated(  dNdx    )) allocate(  dNdx(n,m) )
	if(.not.allocated(  x_dNdxi )) allocate( x_dNdxi(m,m)  )
	if(.not.allocated(  x_dNdxi_inv )) allocate( x_dNdxi_inv(m,m)  )
	if(.not.allocated(  dumat_t )) allocate( dumat_t(m,n)  )
	if(.not.allocated(  dumat )) allocate( dumat(n,m)  )
	if(.not.allocated(  x_u )) allocate( x_u(n,m)  )
	if(.not.allocated(  tr_dNdgzi )) allocate(  tr_dNdgzi(n,m) )
	
	n=size(obj%FEMDomain%ShapeFunction%ElemCoord,1)
	m=size(obj%FEMDomain%ShapeFunction%ElemCoord,2)
	do i=1,n !num of node per elems
		do j=1,m ! num of dim
			dumat(i,j)=obj%DeformVecEBEInc(elem, m*(i-1) + j )
			
		enddo
	enddo
	dumat_t(:,:)=transpose(dumat)
	delta(:,:)=0.0d0
	delta(1,1)=1.0d0
	delta(2,2)=1.0d0
	delta(3,3)=1.0d0
	
	x_dNdxi(:,:)=0.0d0
	tr_dNdgzi(:,:)=transpose(obj%FEMDomain%ShapeFunction%dNdgzi)
	x_u(:,:)=obj%FEMDomain%ShapeFunction%ElemCoord(:,:) -dumat(:,:)
	x_dNdxi(:,:)=transpose(matmul(obj%FEMDomain%ShapeFunction%dNdgzi,x_u)) !dxndgzi at tn


	call inverse_rank_2(x_dNdxi,x_dNdxi_inv)!dgzidxn at tn

    dNdx(:,:)=matmul(tr_dNdgzi,x_dNdxi_inv)
	
	if(m==2)then
		f_n_n_1(:,:)=0.0d0
		f_n_n_1(3,3)=1.0d0
		
		F_iJ(:,:)=0.0d0
		F_iJ(3,3)=1.0d0
		
		
		F_iJ(1,1)=1.0d0
		F_iJ(1,2)=0.0d0
		F_iJ(2,1)=0.0d0
		F_iJ(2,2)=1.0d0
		
    	F_iJ_n(:,:)=0.0d0
		F_iJ_n(1,1)=obj%DeformStrain(elem,gauss,7)
		F_iJ_n(1,2)=obj%DeformStrain(elem,gauss,9)
		F_iJ_n(1,3)=0.0d0
		F_iJ_n(2,1)=obj%DeformStrain(elem,gauss,10)
		F_iJ_n(2,2)=obj%DeformStrain(elem,gauss,8)
		F_iJ_n(2,3)=0.0d0
		F_iJ_n(3,1)=0.0d0
		F_iJ_n(3,2)=0.0d0
		F_iJ_n(3,3)=1.0d0
		

		f_n_n_1(1:2,1:2)=F_iJ(1:2,1:2) + matmul(transpose(dumat),dNdx)
		F_iJ(:,:)=matmul(f_n_n_1,F_iJ_n)
		
		obj%DeformStrain(elem,gauss,11)=F_iJ(1,1)
		obj%DeformStrain(elem,gauss,12)=F_iJ(2,2)
		obj%DeformStrain(elem,gauss,13)=F_iJ(1,2)
		obj%DeformStrain(elem,gauss,14)=F_iJ(2,1)
	elseif(m==3)then

		f_n_n_1(:,:)=0.0d0
		
		F_iJ(:,:)=0.0d0
		F_iJ(1,1)=1.0d0
		F_iJ(2,2)=1.0d0
		F_iJ(3,3)=1.0d0
		
    	F_iJ_n(:,:)=0.0d0
		F_iJ_n(1,1)=obj%DeformStrain(elem,gauss,7)
		F_iJ_n(1,2)=obj%DeformStrain(elem,gauss,9)
		F_iJ_n(1,3)=obj%DeformStrain(elem,gauss,23)
		F_iJ_n(2,1)=obj%DeformStrain(elem,gauss,10)
		F_iJ_n(2,2)=obj%DeformStrain(elem,gauss,8)
		F_iJ_n(2,3)=obj%DeformStrain(elem,gauss,24)
		F_iJ_n(3,1)=obj%DeformStrain(elem,gauss,25)
		F_iJ_n(3,2)=obj%DeformStrain(elem,gauss,26)
		F_iJ_n(3,3)=obj%DeformStrain(elem,gauss,22)

		dF_iJ(1,1)=obj%DeformStrain(elem,gauss,32)
		dF_iJ(1,2)=obj%DeformStrain(elem,gauss,33)
		dF_iJ(1,3)=obj%DeformStrain(elem,gauss,34)
		dF_iJ(2,1)=obj%DeformStrain(elem,gauss,35)
		dF_iJ(2,2)=obj%DeformStrain(elem,gauss,36)
		dF_iJ(2,3)=obj%DeformStrain(elem,gauss,37)
		dF_iJ(3,1)=obj%DeformStrain(elem,gauss,38)
		dF_iJ(3,2)=obj%DeformStrain(elem,gauss,39)
		dF_iJ(3,3)=obj%DeformStrain(elem,gauss,40)
		
		! Clank-Nicolson
		f_n_n_1(:,:)=F_iJ(:,:) + 0.50d0*matmul(transpose(dumat),dNdx) + 0.50d0*dF_iJ(:,:)
		! 4th order Lunge-Kutta
		!dt=1.0d0
		!f_n_n_1(:,:)=matmul(transpose(dumat),dNdx)
		!LungeKutta_1(:,:)=f_n_n_1(:,:)
		!LungeKutta_2(:,:)=f_n_n_1(:,:)+dt/2.0d0*matmul(f_n_n_1,LungeKutta_1)
		!LungeKutta_3(:,:)=f_n_n_1(:,:)+dt/2.0d0*matmul(f_n_n_1,LungeKutta_2)
		!LungeKutta_4(:,:)=f_n_n_1(:,:)+dt*matmul(f_n_n_1,LungeKutta_3)
		!LungeKutta(:,:)=dt/6.0d0*( LungeKutta_1(:,:)+2.0d0*LungeKutta_2(:,:)+2.0d0*LungeKutta_3(:,:)+LungeKutta_4(:,:) )
		!F_iJ(:,:)=F_iJ_n(:,:) + matmul(LungeKutta,F_iJ_n)
		F_iJ(:,:)=matmul(f_n_n_1,F_iJ_n)
		
		obj%DeformStrain(elem,gauss,11)= F_iJ(1,1)
		obj%DeformStrain(elem,gauss,12)= F_iJ(2,2)
		obj%DeformStrain(elem,gauss,13)= F_iJ(1,2)
		obj%DeformStrain(elem,gauss,14)= F_iJ(2,1)
		obj%DeformStrain(elem,gauss,27)= F_iJ(3,3)
		obj%DeformStrain(elem,gauss,28)= F_iJ(1,3)
		obj%DeformStrain(elem,gauss,29)= F_iJ(2,3)
		obj%DeformStrain(elem,gauss,30)= F_iJ(3,1)
		obj%DeformStrain(elem,gauss,31)= F_iJ(3,2)

		dF_iJ(:,:)=matmul(transpose(dumat),dNdx)
		obj%DeformStrain(elem,gauss,41) = dF_iJ(1,1)
		obj%DeformStrain(elem,gauss,42) = dF_iJ(2,2)
		obj%DeformStrain(elem,gauss,43) = dF_iJ(1,2)
		obj%DeformStrain(elem,gauss,44) = dF_iJ(2,1)
		obj%DeformStrain(elem,gauss,45) = dF_iJ(3,3)
		obj%DeformStrain(elem,gauss,46) = dF_iJ(1,3)
		obj%DeformStrain(elem,gauss,47) = dF_iJ(2,3)
		obj%DeformStrain(elem,gauss,48) = dF_iJ(3,1)
		obj%DeformStrain(elem,gauss,49) = dF_iJ(3,2)




	else
		print*,  "FiniteDeformationClass/F_tensor_ICU :: >> Dimension",m,"is not Supported."
		stop
	endif
	
	
   end subroutine 
!=================================================================================


!=================================================================================
subroutine C_tensor(F,C_IJ,b_ij,itr,dim_num)
	real(real64),allocatable::C_IJ(:,:),b_ij(:,:),F_T(:,:)
	real(real64),intent(in)::F(:,:)
	integer(int32),intent(in)::itr,dim_num

	integer(int32) i
	
	if(.not.allocated(  C_IJ )) allocate( C_IJ(3,3)  )
	if(.not.allocated(  b_ij )) allocate( b_ij(3,3)  )
	

	call trans_rank_2(F,F_T)

	if(dim_num==2)then
		C_IJ(:,:)=0.0d0
		b_ij(:,:)=0.0d0
		C_IJ(3,3)=1.0d0
		b_ij(3,3)=1.0d0
		
		C_IJ(1:2,1:2)=matmul(F_T(1:2,1:2) ,F(1:2,1:2) )
		b_ij(1:2,1:2)=matmul(F(1:2,1:2) ,F_T(1:2,1:2) )
	elseif(dim_num==3)then
		
		C_IJ(:,:)=matmul(F_T ,F )
		b_ij(:,:)=matmul(F ,transpose(F) )

	else
		print *, "C_tensor :: dimension : ",dim_num,"is not supported"
		stop 
	endif

  end subroutine C_tensor
!=================================================================================
subroutine Cp_tensor(elem,gauss,strain_measure,Cp_IJ_n,Cp_IJ,Cp_IJ_inv,dim_num)
	real(real64),allocatable:: Cp_iJ_n(:,:),Cp_iJ(:,:),Cp_IJ_inv(:,:)
	real(real64),intent(in)::strain_measure(:,:,:)
	integer(int32), intent(in)::elem,gauss,dim_num

	integer(int32) i
	
	if(.not.allocated( Cp_iJ_n  )) allocate( Cp_iJ_n(3,3)  )
	if(.not.allocated( Cp_iJ    )) allocate(  Cp_iJ(3,3) )
	
	Cp_iJ(:,:)  =0.0d0
	Cp_iJ_n(:,:)=0.0d0
	Cp_iJ(3,3)  =1.0d0
	Cp_iJ_n(3,3)=1.0d0
		
	Cp_iJ(1,1)=strain_measure(elem,gauss,4)
	Cp_iJ(1,2)=strain_measure(elem,gauss,6)
	Cp_iJ(2,1)=strain_measure(elem,gauss,6)
	Cp_iJ(2,2)=strain_measure(elem,gauss,5)
	
	Cp_iJ_n(1,1)=strain_measure(elem,gauss,1)
	Cp_iJ_n(1,2)=strain_measure(elem,gauss,3)
	Cp_iJ_n(2,1)=strain_measure(elem,gauss,3)
	Cp_iJ_n(2,2)=strain_measure(elem,gauss,2)
	
	if(dim_num==3)then

		Cp_iJ_n(3,3)  =  strain_measure(elem,gauss,16)
		Cp_iJ_n(1,3)  =  strain_measure(elem,gauss,17)
		Cp_iJ_n(2,3)  =  strain_measure(elem,gauss,18)
		Cp_iJ_n(3,1)  =  strain_measure(elem,gauss,17)
		Cp_iJ_n(3,2)  =  strain_measure(elem,gauss,18)

		Cp_iJ(3,3)  =  strain_measure(elem,gauss,19)
		Cp_iJ(1,3)  =  strain_measure(elem,gauss,20)
		Cp_iJ(2,3)  =  strain_measure(elem,gauss,21)	
		Cp_iJ(3,1)  =  strain_measure(elem,gauss,20)
		Cp_iJ(3,2)  =  strain_measure(elem,gauss,21)	
			
	endif

	call inverse_rank_2(Cp_IJ,Cp_IJ_inv)
	
   end subroutine Cp_tensor
!=================================================================================
subroutine M_neo_Hookean(C_IJ,Cp_IJ,Cp_IJ_inv,M_IJ,Lamda,mu,elem,gauss)
	real(real64),intent(in)::C_IJ(:,:),Lamda,mu,Cp_IJ(:,:),Cp_IJ_inv(:,:)
	real(real64),allocatable::M_IJ(:,:),G_IJ(:,:),C_Cp_1(:,:)
	integer(int32), intent(in):: elem,gauss
	integer(int32) i,j,n
	
	if(.not. allocated(M_IJ) )allocate(M_IJ(3,3))
	
	allocate(C_Cp_1(3,3),G_IJ(3,3)) !3-D for constitutive equations
	
	n=size(C_IJ,1)
	
	!Metric tensor
	G_IJ(:,:)=0.0d0
	G_IJ(1,1) =1.0d0
	G_IJ(2,2) =1.0d0
	G_IJ(3,3) =1.0d0
	

	
	C_Cp_1(:,:)=matmul(C_IJ,Cp_IJ_inv)
	

	M_IJ(:,:)=Lamda/2.0d0*( det_mat(C_IJ,n)/det_mat(Cp_IJ,n)-1.0d0)*G_IJ(:,:) + mu*(C_Cp_1(:,:)-G_IJ(:,:))	

	
	
  end subroutine M_neo_Hookean 
!=================================================================================
subroutine Return_Mapping_MCDP(dim_num,elem,gauss,C_IJ,Cp_IJ,Cp_IJ_n,Cp_IJ_inv,M_IJ,MatPara,&
   itr_rm,tol,sigma,F_T,F_T_inv,itr,itr_contact,strain_measure,step)
	real(real64),intent(in)::C_IJ(:,:),Cp_IJ_n(:,:),F_T(:,:),F_T_inv(:,:),MatPara(:)
	real(real64),intent(inout)::Cp_IJ(:,:),sigma(:,:,:),strain_measure(:,:,:)
	real(real64),allocatable,intent(inout)::M_IJ(:,:),Cp_IJ_inv(:,:)
	integer(int32), intent (in)::elem,gauss,itr,itr_rm,step,itr_contact,dim_num
	
	real(real64),allocatable::G_IJ(:,:),Jmat(:,:),Xvec(:),Yvec(:),dXvec(:),Zmat(:,:),sigm(:,:),M_FT(:,:),C_IJ_inv(:,:),&
	M_1(:,:),M_2(:,:),M_2_T(:,:),M_3(:,:),M_4(:,:),M_5(:,:),C_1(:,:),C_2(:,:),C_3(:,:),B_6(:,:),B_7(:,:),B_8(:,:),B_9(:,:),B_10(:,:)
	real(real64) detF,I1_M,J2_M,J3_M,Theta_M,BI_MC,BI_DP,fc_MC,er,tol,residual_0,yield_function_mc,detC,detCp,alpha,beta,gunma,omega,&
	a11,a12,a13,a14,a15,a16,a17,a18,a21,a22,a23,a24,a25,a26,MM,xx,c1,c2,c3,c4,c5,c6,c7,val,E,v,Lamda,mu,c,phy,psy
	integer(int32) n,A,B,I,J,K,L,R,M,P,Q,nn,itrmax,retmap_itr,mesh
	
	
	
	
	allocate(G_IJ(3,3),Jmat(4,4),Xvec(4),dXvec(4),Yvec(4),Zmat(3,3),sigm(3,3),M_FT(3,3) )
	allocate(M_1(3,3),M_2(3,3),M_2_T(3,3),M_3(3,3),M_4(3,3),M_5(3,3),C_1(3,3),C_2(3,3),C_3(3,3),&
	B_6(3,3),B_7(3,3),B_8(3,3),B_9(3,3),B_10(3,3))
	
	
		E=MatPara(1)
		v=MatPara(2)
		Lamda=MatPara(3)
		mu=MatPara(4)
		c =MatPara(5)
		phy=MatPara(6)
		psy=MatPara(7)
	!compute vriables for R-M algorithm
	
	G_IJ(:,:)=0.0d0
	G_IJ(1,1)=1.0d0
	G_IJ(2,2)=1.0d0
	G_IJ(3,3)=1.0d0

	
	n      =size(C_IJ,1)
	detF   =sqrt(det_mat(C_IJ,n))
	I1_M   =M_iJ(1,1) + M_iJ(2,2) + M_iJ(3,3)
	
	
	J2_M   =0.0d0	
	do i=1, 3
		do j=1,3
			
			J2_M=J2_M + 1.0d0/2.0d0*( M_IJ(i,j)- I1_M/3.0d0*G_IJ(i,j) )*( M_IJ(i,j)- I1_M/3.0d0*G_IJ(i,j) )
		enddo
	enddo
	
	J3_M   =0.0d0
	do I=1, 3
		do J=1,3
			do K=1,3
				J3_M=J3_M + 1.0d0/3.0d0*( M_IJ(i,j)- I1_M/3.0d0*G_IJ(i,j) )*( M_IJ(j,k)- I1_M/3.0d0*G_IJ(j,k) )&
				*( M_IJ(k,i)- I1_M/3.0d0*G_IJ(k,i) )
				
			enddo
		enddo
	enddo

	if(J2_M==0.0d0)then 
		Theta_M=0.0d0
	else 
		val=1.50d0*sqrt(3.0d0)*J3_M/(J2_M*sqrt(J2_M) )
		if(val<-1.0d0)then
			val=-1.0d0
		elseif(val>1.0d0)then
			val=1.0d0
		endif
		Theta_M=1.0d0/3.0d0*asin(val)
	endif
		
	BI_MC=( 1.0d0 + sin(phy) )/( 1.0d0-sin(phy) )
	BI_DP=( 1.0d0 + sin(psy) )/( 1.0d0-sin(psy) )
	fc_MC=(2.0d0*c*cos(phy))/( 1.0d0-sin(phy) )



	
	if( ( 1.0d0-sin(phy) )   ==0.0d0)  stop "  ( 1.0d0-sin(phy) )  =0"
	
	Yield_function_MC=1.0d0/detF*( sqrt(J2_M) +  ( (BI_MC-1.0d0)*I1_M-3.0d0*detF*fc_MC)/(3.0d0*(BI_MC + 1.0d0)*cos(Theta_M) + &
	sqrt(3.0d0)*(BI_MC-1.0d0)*sin(Theta_M)  )     )
	!write(54,*)&
	!		sqrt(2.0d0/3.0d0)*sqrt(2.0d0*J2_M)*sin(Theta_M + 2.0d0/3.0d0*3.141590d0) + I1_M/3.0d0,&
	!		sqrt(2.0d0/3.0d0)*sqrt(2.0d0*J2_M)*sin(Theta_M                       ) + I1_M/3.0d0,&
	!		sqrt(2.0d0/3.0d0)*sqrt(2.0d0*J2_M)*sin(Theta_M-2.0d0/3.0d0*3.141590d0) + I1_M/3.0d0
	!Plastic corrector algorithm

	!print *, sqrt(J2_M),Yield_function_MC,(BI_MC-1.0d0),I1_M,3.0d0*fc_MC
	
	!print *, Yield_function_MC
	!write(109,*) step,itr,Yield_function_MC,detF,I1_M,fc_Mc
	

	if(Yield_function_MC>0.0d0 .and. itr_contact >= 3 )then
		
		!initialization
		retmap_itr=1
	
		
		Xvec(1)=Cp_IJ_n(1,1)
		Xvec(2)=Cp_IJ_n(2,2)
		Xvec(3)=Cp_IJ_n(1,2)
		
		
		Xvec(4)=0.0d0
		

		Yvec(1)=0.0d0
		Yvec(2)=0.0d0
		Yvec(3)=0.0d0
		Yvec(4)=yield_function_mc
		
		call inverse_rank_2(C_IJ,C_IJ_inv)
		
		!Loop for Return-Mapping algorithm
		do  
			dXvec(:) =0.0d0
			Zmat(:,:)=0.0d0
			Jmat(:,:)=0.0d0
			
			I1_M   =M_iJ(1,1) + M_iJ(2,2) + M_iJ(3,3)
		
			J2_M   =0.0d0	
			do i=1, 3
				do j=1,3
					J2_M=J2_M + 1.0d0/2.0d0*( M_IJ(i,j)- I1_M/3.0d0*G_IJ(i,j) )*( M_IJ(i,j)- I1_M/3.0d0*G_IJ(i,j) )
				enddo
			enddo
			
			J3_M   =0.0d0
			do I=1, 3
				do J=1,3
					do K=1,3
						J3_M=J3_M + 1.0d0/3.0d0*( M_IJ(i,j)- I1_M/3.0d0*G_IJ(i,j) )*( M_IJ(j,k)- I1_M/3.0d0*G_IJ(j,k) )&
						*( M_IJ(k,i)- I1_M/3.0d0*G_IJ(k,i) )
						
					enddo
				enddo
			enddo
			
			MM=0.0d0
			do i=1,3
				do j=1, 3
					MM=MM + M_IJ(I,J)*M_IJ(J,I)
				enddo
			enddo
			
			if(J2_M==0.0d0)then 
				Theta_M=0.0d0
			else 
				Theta_M=1.0d0/3.0d0*asin(1.50d0*sqrt(3.0d0)*J3_M/(J2_M*sqrt(J2_M) ))
				if(J2_M==0.0d0)  stop "J2_M=0"
			endif
			
			Yield_function_MC=1.0d0/detF*( sqrt(J2_M) +  ( (BI_MC-1.0d0)*I1_M-3.0d0*detF*fc_MC)/(3.0d0*(BI_MC + 1.0d0)*cos(Theta_M) + &
				sqrt(3.0d0)*(BI_MC-1.0d0)*sin(Theta_M)  )     )
			Yvec(4)=yield_function_mc
			if(  detF  ==0.0d0)  stop "  detF   =0"
			
			!debug
			print *, "YC", yield_function_mc
			
			if(retmap_itr==1)then
				residual_0=sqrt(dot_product(Yvec,Yvec))
			endif
			
			print *, "itr, residual R-M",retmap_itr,sqrt(dot_product(Yvec,Yvec))
			
			detC=det_mat(C_IJ,n)
			detCp=det_mat(Cp_IJ,n)
			
			!update dYij/dCpkl
			a18=-Lamda/2*detC/detCp
			
			c1 = -4.0d0*(1.0d0/J2_M*(MM-I1_M*I1_M/3.0d0) + ((BI_DP-1.0d0)/(BI_DP + 2.0d0))**2.0d0 )**(-3.0d0/2.0d0)
			c2 =  2.0d0*(1.0d0/J2_M*(MM-I1_M*I1_M/3.0d0) + ((BI_DP-1.0d0)/(BI_DP + 2.0d0))**2.0d0 )**(-1.0d0/2.0d0)
			c3 = -I1_M*I1_M/3.0d0*a18*xvec(4)*c1/J2_M/sqrt(J2_M)
			c4 = -mu/2.0d0*xvec(4)*c1/J2_M/sqrt(J2_M)
			c5 =  xvec(4)*c1/sqrt(3.0d0)*(BI_DP-1.0d0)/(BI_DP + 2.0d0)/J2_M
			c6 =  c3 + a18*c5*(3.0d0-I1_M)
			
			M_1(:,:)=M_IJ(:,:)-I1_M/3.0d0*G_IJ(:,:)
			
			!M_2(:,:)=matmul(matmul(C_IJ,M_IJ),Cp_IJ_inv)
			
			!M_3(:,:)=matmul(Cp_IJ_inv,M_2)

			M_4(:,:)=matmul(M_1,Cp_IJ_n)
			
			M_5(:,:)=matmul(matmul(Cp_IJ_inv,C_IJ ),matmul(M_1,Cp_IJ_inv ) )
			
			C_1(:,:)=matmul(C_IJ,Cp_IJ_inv )
			
			C_2(:,:)=matmul(Cp_IJ_inv,Cp_IJ_n )
			
			C_3(:,:)=matmul(Cp_IJ_inv,matmul(C_IJ,Cp_IJ_inv) )
			
			alpha=-0.50d0/J2_M/sqrt(J2_M)
			beta=1.0d0/sqrt(J2_M)
			gunma=-xvec(4)*mu*c2
			
			c7=c4 + alpha*gunma

			!in case of the edge
			if((3.0d0*(BI_MC + 1.0d0)*cos(Theta_M) + sqrt(3.0d0)*(BI_MC-1.0d0)*sin(Theta_M) )==0.0d0)then
				 stop "singular stress of Mohr-Coulomb"
			endif
			
			a11=0.50d0/detF/sqrt(J2_M)
			a12=-(BI_MC-1.0d0)/detF/(3.0d0*(BI_MC + 1.0d0)*cos(Theta_M) + sqrt(3.0d0)*(BI_MC-1.0d0)*sin(Theta_M) )
			a13=-1.0d0/detF*( (BI_MC-1.0d0)*I1_M + 3.0d0*detF*fc_MC )/&
				(3.0d0*(BI_MC + 1.0d0)*cos(Theta_M) + sqrt(3.0d0)*(BI_MC-1.0d0)*sin(Theta_M) )**2.0d0&
				*(3.0d0*(BI_MC + 1.0d0)*sin(Theta_M)-sqrt(3.0d0)*(BI_MC-1.0d0)*cos(Theta_M) )
			a14=sqrt(3.0d0)/2.0d0/sqrt(J2_M)/J2_M/&
				sqrt(1.0d0-( 27.0d0/4.0d0*J3_M*J3_M/J2_M/J2_M/J2_M )**2.0d0 )
			a15=-3.0d0*sqrt(3.0d0)/4.0d0/sqrt(J2_M)/J2_M/J2_M*J3_M/&
				sqrt(1.0d0-( 27.0d0/4.0d0*J3_M*J3_M/J2_M/J2_M/J2_M )**2.0d0 )
			a16=-2.0d0*I1_M/3.0d0
			a17=2.0d0*I1_M*I1_M/9.0d0
			a18=-Lamda/2*detC/detCp
			
			if( (3.0d0*(BI_MC + 1.0d0)*cos(Theta_M) + sqrt(3.0d0)*(BI_MC-1.0d0)*sin(Theta_M) )  ==0.0d0)&
				 stop "  (3.0d0*(BI_MC + 1.0d0)*cos(Theta_M) + sqrt(3.0d0)*(BI_MC-1.0d0)*sin(Theta_M) )  =0"
			
			

			a21=-a13*a14
			a22=a11-a13*a15-a13*a14*a16
			a23=-I1_M/3.0d0*a11-a12 + a13*a15*I1_M/3.0d0 + a13*a14/3.0d0*MM-a13*a14*a17

			a25=a18*(a21*MM + a22*I1_M + 3.0d0*a23)
			
			B_6(:,:)=matmul(C_IJ,matmul(M_IJ,Cp_IJ_inv ))
			B_7(:,:)=matmul(Cp_IJ_inv,B_6)
			B_8(:,:)=matmul(Cp_IJ_inv,matmul(C_IJ,Cp_IJ_inv) )
			
			B_6(:,:)=matmul(Cp_IJ_inv,matmul(M_IJ,C_IJ ))
			B_9(:,:)=matmul(B_6,Cp_IJ_inv)
			
			B_6(:,:)=matmul(Cp_IJ_inv,matmul(M_IJ,M_IJ ))
			B_10(:,:)=matmul(matmul(B_6,C_IJ),Cp_IJ_inv )
			
			Jmat(:,:)=0.0d0
			I=1
			J=1
			K=1
			L=1
			Jmat(1,1)=1.0d0-c6*Cp_IJ_n(I,J)*Cp_IJ_inv(L,K)-c7*M_4(I,J)*M_5(K,L) + mu*c5*Cp_IJ_n(I,J)*M_5(K,L)&
				-beta*gunma*C_1(I,K)*C_2(L,J) + 1.0d0/3.0d0*beta*gunma*C_3(L,K)*Cp_IJ_n(I,J) 
			
			I=1
			J=1
			K=2
			L=2
			Jmat(1,2)=-c6*Cp_IJ_n(I,J)*Cp_IJ_inv(L,K)-c7*M_4(I,J)*M_5(K,L) + mu*c5*Cp_IJ_n(I,J)*M_5(K,L)&
				-beta*gunma*C_1(I,K)*C_2(L,J) + 1.0d0/3.0d0*beta*gunma*C_3(L,K)*Cp_IJ_n(I,J) 
			
			I=1
			J=1
			K=1
			L=2					
			Jmat(1,3)=-c6*Cp_IJ_n(I,J)*Cp_IJ_inv(L,K)-c7*M_4(I,J)*M_5(K,L) + mu*c5*Cp_IJ_n(I,J)*M_5(K,L)&
				-beta*gunma*C_1(I,K)*C_2(L,J) + 1.0d0/3.0d0*beta*gunma*C_3(L,K)*Cp_IJ_n(I,J) 
			
			I=2
			J=2
			K=1
			L=1
			Jmat(2,1)=-c6*Cp_IJ_n(I,J)*Cp_IJ_inv(L,K)-c7*M_4(I,J)*M_5(K,L) + mu*c5*Cp_IJ_n(I,J)*M_5(K,L)&
				-beta*gunma*C_1(I,K)*C_2(L,J) + 1.0d0/3.0d0*beta*gunma*C_3(L,K)*Cp_IJ_n(I,J) 
			
			I=2
			J=2
			K=2
			L=2					
			Jmat(2,2)=1.0d0-c6*Cp_IJ_n(I,J)*Cp_IJ_inv(L,K)-c7*M_4(I,J)*M_5(K,L) + mu*c5*Cp_IJ_n(I,J)*M_5(K,L)&
				-beta*gunma*C_1(I,K)*C_2(L,J) + 1.0d0/3.0d0*beta*gunma*C_3(L,K)*Cp_IJ_n(I,J) 
			
			I=2
			J=2
			K=1
			L=2					
			Jmat(2,3)=-c6*Cp_IJ_n(I,J)*Cp_IJ_inv(L,K)-c7*M_4(I,J)*M_5(K,L) + mu*c5*Cp_IJ_n(I,J)*M_5(K,L)&
				-beta*gunma*C_1(I,K)*C_2(L,J) + 1.0d0/3.0d0*beta*gunma*C_3(L,K)*Cp_IJ_n(I,J) 
			
			I=1
			J=2
			K=1
			L=1					
			Jmat(3,1)=-c6*Cp_IJ_n(I,J)*Cp_IJ_inv(L,K)-c7*M_4(I,J)*M_5(K,L) + mu*c5*Cp_IJ_n(I,J)*M_5(K,L)&
				-beta*gunma*C_1(I,K)*C_2(L,J) + 1.0d0/3.0d0*beta*gunma*C_3(L,K)*Cp_IJ_n(I,J) 
			
			I=1
			J=2
			K=2
			L=2					
			Jmat(3,2)=-c6*Cp_IJ_n(I,J)*Cp_IJ_inv(L,K)-c7*M_4(I,J)*M_5(K,L) + mu*c5*Cp_IJ_n(I,J)*M_5(K,L)&
				-beta*gunma*C_1(I,K)*C_2(L,J) + 1.0d0/3.0d0*beta*gunma*C_3(L,K)*Cp_IJ_n(I,J) 
			
			I=1
			J=2
			K=1
			L=2					
			Jmat(3,3)=1.0d0-c6*Cp_IJ_n(I,J)*Cp_IJ_inv(L,K)-c7*M_4(I,J)*M_5(K,L) + mu*c5*Cp_IJ_n(I,J)*M_5(K,L)&
				-beta*gunma*C_1(I,K)*C_2(L,J) + 1.0d0/3.0d0*beta*gunma*C_3(L,K)*Cp_IJ_n(I,J) 
			
			do i=1,size(zmat,1)
				do j=1, size(zmat,1)
					zmat(i,j)=zmat(i,j) + (1.0d0/sqrt(J2_M)*( M_IJ(i,j)-I1_M/3.0d0*G_IJ(i,j)  ) + &
					2.0d0*(BI_DP-1.0d0)/sqrt(3.0d0)/(BI_DP + 2.0d0)*G_IJ(i,j)  )
					if(  (BI_DP + 2.0d0)  ==0.0d0)  stop "  (BI_DP + 2.0d0)  =0"
					
				enddo
			enddo
			zmat(:,:)=2.0d0*( (MM-I1_M*I1_M/3.0d0)/J2_M + &
			((BI_DP-1.0d0)/(BI_DP + 2.0d0))**2.0d0 )**(-1.0d0/2.0d0)*zmat(:,:)
			
			do M=1,3
				Jmat(1,4)=Jmat(1,4)-Zmat(1,M)*Cp_IJ_n(M,1)
				Jmat(2,4)=Jmat(2,4)-Zmat(2,M)*Cp_IJ_n(M,2)
				Jmat(3,4)=Jmat(3,4)-Zmat(1,M)*Cp_IJ_n(M,2)
			enddo			
			I=1
			J=1	
			Jmat(4,1)=a25*Cp_IJ_inv(J,I)-mu*(a21*B_10(J,I) + a22*B_7(I,J) + a23*B_8(J,I))

			I=2
			J=2
			Jmat(4,2)=a25*Cp_IJ_inv(J,I)-mu*(a21*B_10(J,I) + a22*B_7(I,J) + a23*B_8(J,I))

			I=1
			J=2			
			Jmat(4,3)=a25*Cp_IJ_inv(J,I)-mu*(a21*B_10(J,I) + a22*B_7(I,J) + a23*B_8(J,I))

			Jmat(4,4)=0.0d0
			
			!control parameters
			!write(53,*)"I,yield_function_mc,norm",retmap_itr,yield_function_mc,sqrt(dot_product(Yvec,Yvec))
			
			! stop "debug r-m"
			itrmax=itr_rm
			er=1.0e-14
			nn=size(dXvec)
			print *, "detJmat=",det_mat(Jmat,4),det_mat(zmat,2)
			
			!do i=1,size(Jmat,1)
			!	write(52,*)Jmat(i,:)
			!enddo		
			

			Yvec(1)=Cp_IJ(1,1)
			Yvec(2)=Cp_IJ(2,2)
			Yvec(3)=Cp_IJ(1,2)
			Yvec(4)=yield_function_mc
			do K=1,3
				Yvec(1)=Yvec(1)-(G_IJ(1,K) + Xvec(4)*Zmat(1,K)  )*Cp_IJ_n(K,1)
				Yvec(2)=Yvec(2)-(G_IJ(2,K) + Xvec(4)*Zmat(2,K)  )*Cp_IJ_n(K,2)
				Yvec(3)=Yvec(3)-(G_IJ(1,K) + Xvec(4)*Zmat(1,K)  )*Cp_IJ_n(K,2)
			enddo
			
			!solve
			call gauss_jordan_pv(Jmat, dXvec, Yvec, nn)	
			
			
			
			Xvec(:)=Xvec(:)-dXvec(:)
			
			!update variables
			Cp_IJ(:,:)=0.0d0
			Cp_IJ(3,3)=1.0d0
			Cp_IJ(1,1)=Xvec(1)
			Cp_IJ(2,2)=Xvec(2)
			Cp_IJ(1,2)=Xvec(3)
			Cp_IJ(2,1)=Xvec(3)

			call inverse_rank_2(Cp_IJ,Cp_IJ_inv)
			call M_neo_Hookean(C_IJ,Cp_IJ,Cp_IJ_inv,M_IJ,Lamda,mu,elem,gauss)



			!Judgement of convergence
			if( sqrt(dot_product(Yvec,Yvec)) <=TOL)then
				!converged
				if(  residual_0  ==0.0d0)  stop "  residual_0  =0"
				
				print *, "R-M converge itr=",itr
				exit
			elseif(retmap_itr>=itrmax)then
				print *, "Return-Mapping (E-P) did not converge"

				!debug: print yield surface
				xx=0.40d0
				mesh=20
				do i=1, mesh

					do j=1, 12		!1	
						I1_M=xx*(1.0d0-2.0d0/dble(mesh)*dble(i))
						Theta_M=3.14159d0/6.0d0-3.14159d0/3.0d0/12.0d0*dble(j)
						J2_M=( -(BI_MC-1.0d0)*I1_M + 3.0d0*detF*fc_MC)/(3.0d0*(BI_MC + 1.0d0)*cos(Theta_M) + &
						sqrt(3.0d0)*(BI_MC-1.0d0)*sin(Theta_M)  )   
						
						write(55,*)&
						2.0d0/sqrt(3.0d0)*J2_M*sin(Theta_M + 2.0d0/3.0d0*3.141590d0) + I1_M/3.0d0,&
						2.0d0/sqrt(3.0d0)*J2_M*sin(Theta_M                       ) + I1_M/3.0d0,&
						2.0d0/sqrt(3.0d0)*J2_M*sin(Theta_M-2.0d0/3.0d0*3.141590d0) + I1_M/3.0d0
					enddo
					do j=1, 12			!6
						I1_M=xx*(1.0d0-2.0d0/dble(mesh)*dble(i))
						Theta_M=-3.14159d0/6.0d0 + 3.14159d0/3.0d0/12.0d0*dble(j)
						J2_M=( -(BI_MC-1.0d0)*I1_M + 3.0d0*detF*fc_MC)/(3.0d0*(BI_MC + 1.0d0)*cos(Theta_M) + &
						sqrt(3.0d0)*(BI_MC-1.0d0)*sin(Theta_M)  )  
						write(55,*)&
						2.0d0/sqrt(3.0d0)*J2_M*sin(Theta_M + 2.0d0/3.0d0*3.141590d0) + I1_M/3.0d0,&
						2.0d0/sqrt(3.0d0)*J2_M*sin(Theta_M-2.0d0/3.0d0*3.141590d0) + I1_M/3.0d0,&
						2.0d0/sqrt(3.0d0)*J2_M*sin(Theta_M                       ) + I1_M/3.0d0
					enddo
					
					do j=1, 12		!3	
						I1_M=xx*(1.0d0-2.0d0/dble(mesh)*dble(i))
						Theta_M=3.14159d0/6.0d0-3.14159d0/3.0d0/12.0d0*dble(j)
						J2_M=( -(BI_MC-1.0d0)*I1_M + 3.0d0*detF*fc_MC)/(3.0d0*(BI_MC + 1.0d0)*cos(Theta_M) + &
						sqrt(3.0d0)*(BI_MC-1.0d0)*sin(Theta_M)  ) 
						write(55,*)&
						2.0d0/sqrt(3.0d0)*J2_M*sin(Theta_M                       ) + I1_M/3.0d0,&
						2.0d0/sqrt(3.0d0)*J2_M*sin(Theta_M-2.0d0/3.0d0*3.141590d0) + I1_M/3.0d0,&
						2.0d0/sqrt(3.0d0)*J2_M*sin(Theta_M + 2.0d0/3.0d0*3.141590d0) + I1_M/3.0d0
					enddo
					
					
					do j=1, 12			!5
						I1_M=xx*(1.0d0-2.0d0/dble(mesh)*dble(i))
						Theta_M=-3.14159d0/6.0d0 + 3.14159d0/3.0d0/12.0d0*dble(j)
						J2_M=( -(BI_MC-1.0d0)*I1_M + 3.0d0*detF*fc_MC)/(3.0d0*(BI_MC + 1.0d0)*cos(Theta_M) + &
						sqrt(3.0d0)*(BI_MC-1.0d0)*sin(Theta_M)  ) 
						write(55,*)&
						2.0d0/sqrt(3.0d0)*J2_M*sin(Theta_M-2.0d0/3.0d0*3.141590d0) + I1_M/3.0d0,&
						2.0d0/sqrt(3.0d0)*J2_M*sin(Theta_M                       ) + I1_M/3.0d0,&
						2.0d0/sqrt(3.0d0)*J2_M*sin(Theta_M + 2.0d0/3.0d0*3.141590d0) + I1_M/3.0d0
					enddo
					
					do j=1, 12			!4
						I1_M=xx*(1.0d0-2.0d0/dble(mesh)*dble(i))
						Theta_M=3.14159d0/6.0d0-3.14159d0/3.0d0/12.0d0*dble(j)
						J2_M=( -(BI_MC-1.0d0)*I1_M + 3.0d0*detF*fc_MC)/(3.0d0*(BI_MC + 1.0d0)*cos(Theta_M) + &
						sqrt(3.0d0)*(BI_MC-1.0d0)*sin(Theta_M)  ) 
						write(55,*)&
						2.0d0/sqrt(3.0d0)*J2_M*sin(Theta_M-2.0d0/3.0d0*3.141590d0) + I1_M/3.0d0,&
						2.0d0/sqrt(3.0d0)*J2_M*sin(Theta_M + 2.0d0/3.0d0*3.141590d0) + I1_M/3.0d0,&
						2.0d0/sqrt(3.0d0)*J2_M*sin(Theta_M                       ) + I1_M/3.0d0
					enddo
					do j=1, 12		!2	
						I1_M=xx*(1.0d0-2.0d0/dble(mesh)*dble(i))
						Theta_M=-3.14159d0/6.0d0 + 3.14159d0/3.0d0/12.0d0*dble(j)
						J2_M=( -(BI_MC-1.0d0)*I1_M + 3.0d0*detF*fc_MC)/(3.0d0*(BI_MC + 1.0d0)*cos(Theta_M) + &
						sqrt(3.0d0)*(BI_MC-1.0d0)*sin(Theta_M)  ) 
						write(55,*)&
						2.0d0/sqrt(3.0d0)*J2_M*sin(Theta_M                       ) + I1_M/3.0d0,&
						2.0d0/sqrt(3.0d0)*J2_M*sin(Theta_M + 2.0d0/3.0d0*3.141590d0) + I1_M/3.0d0,&
						2.0d0/sqrt(3.0d0)*J2_M*sin(Theta_M-2.0d0/3.0d0*3.141590d0) + I1_M/3.0d0
					enddo
				enddo
				 stop 
			else
				retmap_itr=retmap_itr + 1
				cycle
			endif
		enddo
	else
		!print *, "elastic"
	endif
	
	M_FT(:,:)=matmul(M_IJ,F_T)
	sigm(:,:)=1.0d0/detF*matmul(  F_T_inv  ,  M_FT     )
	
	if(dim_num==2)then
	sigma(elem,gauss,1)=sigm(1,1)
	sigma(elem,gauss,2)=sigm(2,2)
	sigma(elem,gauss,3)=sigm(1,2)
	sigma(elem,gauss,4)=sigm(3,3)
	elseif(dim_num==3)then

		sigma(elem,gauss,1)=sigm(1,1)
		sigma(elem,gauss,2)=sigm(2,2)
		sigma(elem,gauss,3)=sigm(3,3)
		sigma(elem,gauss,4)=sigm(1,2)
		sigma(elem,gauss,5)=sigm(2,3)
		sigma(elem,gauss,6)=sigm(3,1)
	else
		stop "Return_Mapping_MCDP >> dimension is to be 2 or 3"
	endif
	strain_measure(elem,gauss,4)=Cp_IJ(1,1)
	strain_measure(elem,gauss,6)=Cp_IJ(1,2)
	strain_measure(elem,gauss,6)=Cp_IJ(2,1)
	strain_measure(elem,gauss,5)=Cp_IJ(2,2)

	if(dim_num==3)then
		strain_measure(elem,gauss,19) = Cp_iJ(3,3)
		strain_measure(elem,gauss,20) = Cp_iJ(1,3)
		strain_measure(elem,gauss,21) = Cp_iJ(2,3)
	endif
	
	
	
   end subroutine Return_Mapping_MCDP
!=================================================================================
! D�}�g���N�X�̌v�Z
subroutine Ce_neoHK_current(dim_num, elem, gauss,Lame1,Lame2,C_IJ,Cp_IJ,b_ij,M_IJ,Ce_neoHK,F_T,F_T_inv,ij)
     integer(int32), intent(in) :: dim_num,elem, gauss
     real(real64), intent(in) :: Lame1,Lame2,C_IJ(:,:),Cp_IJ(:,:),b_ij(:,:),F_T(:,:),F_T_inv(:,:),M_IJ(:,:)
     real(real64), allocatable, intent(out) :: Ce_neoHK(:,:)
	 integer(int32),allocatable,intent(out)::ij(:,:)
	 real(real64), allocatable::t_ij(:,:),G_IJ(:,:)
     integer(int32)  n,m,p,q,i,j,k,l
	 real(real64) detF,detCp,detC
	
	allocate(G_IJ(3,3)) 
	
	G_IJ(:,:)=0.0d0
	G_IJ(1,1) =1.0d0
	G_IJ(2,2) =1.0d0
	G_IJ(3,3) =1.0d0
	if(allocated(ij) ) deallocate(ij)
	 if(dim_num==2)then
     	n = 3 ! �Ђ��݂���11,��22,��12��3����
		
		allocate(ij(3,1) )
		ij(1,1)=1 ; ij(1,2)=1;
		ij(2,1)=2 ; ij(2,2)=2;
		ij(3,1)=1 ; ij(3,2)=2;
		 
		m=size(C_IJ,1)
		if(.not.allocated(Ce_neoHK) ) allocate(Ce_neoHK(n,n))
		 
	 	allocate (t_ij(n,n))
		
	 	detC=det_mat(C_IJ,m)
	 	detCp=det_mat(Cp_IJ,m)
	 	detF=sqrt(detC)
		
	 	t_ij=matmul(  F_T_inv    ,matmul(M_IJ,F_T)  )
		

		do p=1,3
			do q=1,3
				i=ij(p,1)
				j=ij(p,2)
				k=ij(q,1)
				l=ij(q,2)

				Ce_neoHK(p,q)=Lame1*(detC/detCp*b_ij(i,j)*b_ij(l,k)&
				 - Lame1*(detC/detCp-1)*b_ij(i,k)*b_ij(l,j) )&
				  +  2.0d0*Lame2*b_ij(i,k)*b_ij(l,j)&
				  +  G_IJ(i,k)*t_ij(l,j)
			enddo
		enddo
		Ce_neoHK(:,:)=1.0d0/detF*Ce_neoHK(:,:)

	 elseif(dim_num==3)then
     	n = 6 ! �Ђ��݂���11,��22,��12��3����
		allocate(ij(6,2) )
		ij(1,1)=1 ; ij(1,2)=1;
		ij(2,1)=2 ; ij(2,2)=2;
		ij(3,1)=3 ; ij(3,2)=3;
		ij(4,1)=1 ; ij(4,2)=2;
		ij(5,1)=2 ; ij(5,2)=3;
		ij(6,1)=3 ; ij(6,2)=1;
		 
		m=size(C_IJ,1)

		if(.not.allocated(Ce_neoHK) ) allocate(Ce_neoHK(n,n))
	 	allocate (t_ij(n,n))
		
	 	detC=det_mat(C_IJ,m)
	 	detCp=det_mat(Cp_IJ,m)
	 	detF=sqrt(detC)
		
	 	t_ij=matmul(  F_T_inv    ,matmul(M_IJ,F_T)  )
		
		do p=1,6
			do q=1,6
				i=ij(p,1)
				j=ij(p,2)
				k=ij(q,1)
				l=ij(q,2)

				Ce_neoHK(p,q)=Lame1*(detC/detCp*b_ij(i,j)*b_ij(l,k)&
				 - Lame1*(detC/detCp-1)*b_ij(i,k)*b_ij(l,j) )&
				  +  2.0d0*Lame2*b_ij(i,k)*b_ij(l,j) !+  G_IJ(i,k)*t_ij(l,j)
			enddo
		enddo
		Ce_neoHK(:,:)=1.0d0/detF*Ce_neoHK(:,:)
		
	 else
		print *, "Ce_neoHK_current >> Dimension, ",dim_num," is not supported"
		stop
	 endif

	 
   end subroutine Ce_neoHK_current
!=================================================================================

!=================================================================================
! D�}�g���N�X�̌v�Z
subroutine GetSigmaVec(Sigma,Sigma_ij,dim_num)
     real(real64), intent(in) :: Sigma_ij(:,:)
     real(real64), allocatable, intent(inout) :: Sigma(:)
	 integer(int32),allocatable::ij(:,:)
	 integer(int32),intent(in)::dim_num
     integer(int32)  n,m,p,q,i,j,k,l
	 real(real64) detF,detCp,detC
	
	if(allocated(ij) ) deallocate(ij)
	 if(dim_num==2)then
     	n = 3 ! �Ђ��݂���11,��22,��12��3����
		
		allocate(ij(3,1) )
		ij(1,1)=1 ; ij(1,2)=1;
		ij(2,1)=2 ; ij(2,2)=2;
		ij(3,1)=1 ; ij(3,2)=2;
		

		if(.not.allocated(Sigma) ) allocate(Sigma(n))
		do p=1,3
			i=ij(p,1)
			j=ij(p,2)
			Sigma(p)=Sigma_ij(i,j)
			
		enddo

	 elseif(dim_num==3)then
     	n = 6 ! �Ђ��݂���11,��22,��12��3����
		allocate(ij(6,2) )
		ij(1,1)=1 ; ij(1,2)=1;
		ij(2,1)=2 ; ij(2,2)=2;
		ij(3,1)=3 ; ij(3,2)=3;
		ij(4,1)=1 ; ij(4,2)=2;
		ij(5,1)=2 ; ij(5,2)=3;
		ij(6,1)=3 ; ij(6,2)=1;
		
		if(.not.allocated(Sigma) ) allocate(Sigma(n))
	 	
	 	
		do p=1,6
			Sigma(p)=Sigma_ij(ij(p,1),ij(p,2))
		enddo
		
	 else
		print *, "Dmat >> Dimension, ",dim_num," is not supported"
		stop
	 endif

	 
   end subroutine 
!=================================================================================
!=================================================================================
! D�}�g���N�X�̌v�Z
subroutine GetDmat(Dmat,c_ijkl,dim_num)
     real(real64), intent(in) :: c_ijkl(:,:,:,:)
     real(real64), allocatable, intent(inout) :: Dmat(:,:)
	 integer(int32),allocatable::ij(:,:)
	 integer(int32),intent(in)::dim_num
     integer(int32)  n,m,p,q,i,j,k,l
	 real(real64) detF,detCp,detC
	
	if(allocated(ij) ) deallocate(ij)
	 if(dim_num==2)then
     	n = 3 ! �Ђ��݂���11,��22,��12��3����
		
		allocate(ij(3,1) )
		ij(1,1)=1 ; ij(1,2)=1;
		ij(2,1)=2 ; ij(2,2)=2;
		ij(3,1)=1 ; ij(3,2)=2;
		 
		if(.not.allocated(Dmat) ) allocate(Dmat(n,n))

		do p=1,3
			do q=1,3
				i=ij(p,1)
				j=ij(p,2)
				k=ij(q,1)
				l=ij(q,2)

				Dmat(p,q)=c_ijkl(i,j,k,l)
			enddo
		enddo

	 elseif(dim_num==3)then
     	n = 6 ! �Ђ��݂���11,��22,��12��3����
		allocate(ij(6,2) )
		ij(1,1)=1 ; ij(1,2)=1;
		ij(2,1)=2 ; ij(2,2)=2;
		ij(3,1)=3 ; ij(3,2)=3;
		ij(4,1)=1 ; ij(4,2)=2;
		ij(5,1)=2 ; ij(5,2)=3;
		ij(6,1)=3 ; ij(6,2)=1;
		
		if(.not.allocated(Dmat) ) allocate(Dmat(n,n))
	 	
	 	
		do p=1,6
			do q=1,6
				i=ij(p,1)
				j=ij(p,2)
				k=ij(q,1)
				l=ij(q,2)

				Dmat(p,q)=c_ijkl(i,j,k,l)
			enddo
		enddo
		
	 else
		print *, "Dmat >> Dimension, ",dim_num," is not supported"
		stop
	 endif

	 
   end subroutine 
!=================================================================================
   !B�}�g���N�X�̌v�Z
subroutine B_mat(dim_num,Psymat, Jmat, detJ, Bmat,mm)
     real(real64), intent(in) :: Psymat(:,:), Jmat(:,:), detJ ! J�̋t�s��
     real(real64), allocatable, intent(inout) :: Bmat(:,:)
	 integer(int32),intent(in)::dim_num
	 real(real64), allocatable :: JPsy(:,:), Jin(:,:)
     integer(int32)  k, l,m, n, a, b, p,mm,i,j,q
	 
	 if(dim_num==2)then
		k=3
	 elseif(dim_num==3)then
		k=6
	 else
		stop "B_mat >> dim_num = tobe 2 or 3 "
	 endif
	 !k = size(ij,1)   ! �Ђ��݂���11,��22,��12��3����
     l = mm  ! 8�ړ_�v�f*2����
     m = size(Psymat, 1)
	 n = size(Psymat, 2)
	 
	 if(allocated(Bmat )) deallocate(Bmat)
     allocate (Bmat(k, l))
     allocate(JPsy(m,n))
	 allocate(Jin(m,m))
 
! J:Psymat�̌v�Z
     if(mm/2==3)then
	     stop 'Now Constructing'
	 elseif(mm/2==4)then
		if(detJ==0.0d0)  stop "Bmat,detJ=0"
	    Jin(1,1) = (1.0d0 / detJ) * Jmat(2,2)
        Jin(2,2) = (1.0d0 / detJ) * Jmat(1,1)
        Jin(1,2) = (-1.0d0 / detJ) * Jmat(1,2)
        Jin(2,1) = (-1.0d0 / detJ) * Jmat(2,1)
        JPsy(:,:) = matmul(Jin, Psymat)   
		
		Bmat(1,1) =  JPsy(1,1)
        Bmat(1,2) = 0.0d0
        Bmat(1,3) =  1.0d0 * JPsy(1,2)
        Bmat(1,4) = 0.0d0
        Bmat(1,5) =  1.0d0 * JPsy(1,3)
        Bmat(1,6) = 0.0d0
        Bmat(1,7) =  1.0d0 * JPsy(1,4)
        Bmat(1,8) = 0.0d0
        Bmat(2,1) = 0.0d0
        Bmat(2,2) =  1.0d0 * JPsy(2,1)
        Bmat(2,3) = 0.0d0
        Bmat(2,4) =  1.0d0 * JPsy(2,2)
        Bmat(2,5) = 0.0d0
        Bmat(2,6) =  1.0d0 * JPsy(2,3)
        Bmat(2,7) = 0.0d0
        Bmat(2,8) =  1.0d0 * JPsy(2,4)
        Bmat(3,1) = Bmat(2,2)
        Bmat(3,2) = Bmat(1,1)
        Bmat(3,3) = Bmat(2,4)
        Bmat(3,4) = Bmat(1,3)
        Bmat(3,5) = Bmat(2,6)
        Bmat(3,6) = Bmat(1,5)
        Bmat(3,7) = Bmat(2,8)
        Bmat(3,8) = Bmat(1,7)
		
	 elseif(mm/2==8)then
		Jin(1,1) = (1.0d0 / detJ) * Jmat(2,2)
        Jin(2,2) = (1.0d0 / detJ) * Jmat(1,1)
        Jin(1,2) = (-1.0d0 / detJ) * Jmat(2,1)
        Jin(2,1) = (-1.0d0 / detJ) * Jmat(1,2)
        JPsy(:,:) = matmul(Jin, Psymat)      
        
		Bmat(1,1) =  -JPsy(1,1)
        Bmat(1,2) = 0.0d0
        Bmat(1,3) =  -1.0d0 * JPsy(1,2)
        Bmat(1,4) = 0.0d0
        Bmat(1,5) =  -1.0d0 * JPsy(1,3)
        Bmat(1,6) = 0.0d0
        Bmat(1,7) =  -1.0d0 * JPsy(1,4)
        Bmat(1,8) = 0.0d0
        Bmat(1,9) =  -1.0d0 * JPsy(1,5)
        Bmat(1,10) = 0.0d0
        Bmat(1,11) =  -1.0d0 * JPsy(1,6)
        Bmat(1,12) = 0.0d0
        Bmat(1,13) =  -1.0d0 * JPsy(1,7)
        Bmat(1,14) = 0.0d0
        Bmat(1,15) =  -1.0d0 * JPsy(1,8)
        Bmat(1,16) = 0.0d0
        Bmat(2,1) = 0.0d0
        Bmat(2,2) =  -1.0d0 * JPsy(2,1)
        Bmat(2,3) = 0.0d0
        Bmat(2,4) =  -1.0d0 * JPsy(2,2)
        Bmat(2,5) = 0.0d0
        Bmat(2,6) =  -1.0d0 * JPsy(2,3)
        Bmat(2,7) = 0.0d0
        Bmat(2,8) =  -1.0d0 * JPsy(2,4)
        Bmat(2,9) = 0.0d0
        Bmat(2,10) =  -1.0d0 * JPsy(2,5)
        Bmat(2,11) = 0.0d0
        Bmat(2,12) =  -1.0d0 * JPsy(2,6)
        Bmat(2,13) = 0.0d0
        Bmat(2,14) =  -1.0d0 * JPsy(2,7)
        Bmat(2,15) = 0.0d0
        Bmat(2,16) =  -1.0d0 * JPsy(2,8)
        Bmat(3,1) = Bmat(2,2)
        Bmat(3,2) = Bmat(1,1)
        Bmat(3,3) = Bmat(2,4)
        Bmat(3,4) = Bmat(1,3)
        Bmat(3,5) = Bmat(2,6)
        Bmat(3,6) = Bmat(1,5)
        Bmat(3,7) = Bmat(2,8)
        Bmat(3,8) = Bmat(1,7)
        Bmat(3,9) = Bmat(2,10)
        Bmat(3,10) = Bmat(1,9)
        Bmat(3,11) = Bmat(2,12)
        Bmat(3,12) = Bmat(1,11)
        Bmat(3,13) = Bmat(2,14)
        Bmat(3,14) = Bmat(1,13)
        Bmat(3,15) = Bmat(2,16)
		Bmat(3,16) = Bmat(1,15)
	 elseif(k==6 )then
		
		if(detJ==0.0d0)  stop "Bmat,detJ=0"
		
		call  inverse_rank_2(Jmat,Jin)
		

		JPsy(:,:) = transpose(matmul(transpose(Psymat),Jin)) !dNdgzi* dgzidx
		Bmat(:,:)=0.0d0
		do q=1,size(JPsy,2)
			do p=1,dim_num
				Bmat(p,dim_num*(q-1) + p )=JPsy(p,q)
			enddo
			Bmat(4,dim_num*(q-1) + 1 )=JPsy(2,q); Bmat(4, dim_num*(q-1) + 2 )=JPsy(1,q);Bmat(4, dim_num*(q-1) + 3 )=0.0d0    ; 
			Bmat(5,dim_num*(q-1) + 1 )=0.0d0    ; Bmat(5, dim_num*(q-1) + 2 )=JPsy(3,q);Bmat(5, dim_num*(q-1) + 3 )=JPsy(2,q);
			Bmat(6,dim_num*(q-1) + 1 )=JPsy(3,q); Bmat(6, dim_num*(q-1) + 2 )=0.0d0    ;Bmat(6, dim_num*(q-1) + 3 )=JPsy(1,q);
		enddo

		Bmat(4:6,:)=0.50d0*Bmat(4:6,:)



	 else
		stop "Bmat >> The element is not supported."
	endif

	
    end subroutine B_mat
!=================================================================================	
! �K�E�X�ϕ�
subroutine K_mat_e(j,s, BTmat, Ce_neoHK, Bmat, detJ, Kmat_e,F_iJ)
   integer(int32), intent(in) :: j
   real(real64), intent(in) :: BTmat(:,:), Ce_neoHK(:,:), Bmat(:,:), detJ, s(:),F_iJ(:,:)
   real(real64), intent(out) :: Kmat_e(:,:)
   real(real64), allocatable :: DBmat(:,:)
   integer(int32) nm, e,n
   
   n= size(F_iJ,1)
   nm = size(Bmat, 2)
   e = size(Bmat,1)
   allocate ( DBmat(e, nm))
   
   DBmat = matmul(Ce_neoHK, Bmat)
   Kmat_e(:,:) = Kmat_e(:,:) + s(j) * detJ *matmul(BTmat, DBmat) !/det_mat(F_iJ,n)


   end subroutine K_mat_e
!=================================================================================
subroutine g_vector_e(elem,gauss,s, BTmat,sigma, detJ, gvec_e)

    integer(int32), intent(in) :: elem,gauss
    real(real64), intent(in) :: BTmat(:,:), sigma(:,:,:), detJ, s(:)
    real(real64), intent(inout) :: gvec_e(:)
    real(real64), allocatable :: sigm(:)
    integer(int32) nm, i
	 
	if(size(BTmat,2)==3)then
       allocate(sigm(3))
	   nm = size(BTmat, 1)
	   
	   do i=1,size(BTmat,2)
			sigm(i)=sigma(elem,gauss,i)
	   enddo
	elseif(size(BTmat,2)==6)then
   		allocate(sigm(6))
	   nm = size(BTmat, 1)
	   
	   do i=1,size(BTmat,2)
			sigm(i)=sigma(elem,gauss,i)
	   enddo
	else
		stop "Error :: g_vec_e :: Invalid size of Bmat"
	endif

	gvec_e(:)=gvec_e(:) + s(gauss)*detJ*matmul(BTmat,sigm)	



	   
	
  end subroutine g_vector_e
!=================================================================================




!######################## Solve DiffusionEq ########################
subroutine SolveFiniteDeform(obj,OptionItr,Solvertype,nr_tol)
	class(FiniteDeform_),intent(inout)::obj
	integer(int32),optional,intent(in)::OptionItr
    character(*),optional,intent(in)::Solvertype
    real(real64) ,optional,intent(in) :: nr_tol
	character*70 ::solver,defaultsolver
	type(IO_) :: f

    real(real64),allocatable::Amat(:,:),bvec(:),xvec(:)
    real(real64)::val,er
    integer(int32) ::i,j,n,m,k,l,dim1,dim2,nodeid1,nodeid2,localid,itrmax,SetBC,int1,int2
	integer(int32) :: dim_num,node_num,elem_num,node_num_elmtl
	!obj%nr_tol=1.0e-08
	if(present(nr_tol) )then
		obj%nr_tol = nr_tol
	endif

	node_num=size(obj%FEMDomain%Mesh%NodCoord,1)
	dim_num=size(obj%FEMDomain%Mesh%NodCoord,2)
	elem_num=size(obj%FEMDomain%Mesh%ElemNod,1)
	node_num_elmtl=size(obj%FEMDomain%Mesh%ElemNod,2)
	if(present(OptionItr) )then
		! Interation in Netwon's Loop >> Set all BCs for zero.
		if(OptionItr<=0)then
			SetBC=1
		elseif(OptionItr>=1)then
			SetBC=0
		else
			print *, "ERROR :: FiniteDeformationClass.f90 >> SolveDeformStress :: Please check OptionalItr"
		endif
	else
		SetBC=1
	endif

	!if sorving initial value
	if(SetBC==1)then
		obj%DeformVecEBEInc(:,:)=0.0d0
		obj%DeformVecGloInc(:)=0.0d0
	endif

	if(present(SolverType) )then
		solver=SolverType
	else
		solver="BiCGSTAB"
	endif
	
	n=node_num
	m=dim_num
	allocate(Amat(n*m,n*m) ,bvec(n*m),xvec(n*m) )
    Amat(:,:)=0.0d0
	bvec(:)  =0.0d0
	
	!print *, "stop debug"
	!call showArray(Amat, Name="Amat.txt")
	!stop 

    !===================================
    ! assemble matrix
    do i=1,elem_num
        do j=1, node_num_elmtl
            nodeid1=obj%FEMDomain%Mesh%ElemNod(i,j)
            do k=1,node_num_elmtl
				nodeid2=obj%FEMDomain%Mesh%ElemNod(i,k)
				do dim1=1,dim_num
					do dim2=1,dim_num
						int1=(dim_num*(j-1)) + dim1
						int2=(dim_num*(k-1)) + dim2
						Amat(dim_num*(nodeid1-1) + dim1, dim_num*(nodeid2-1) + dim2 )=&
						Amat(dim_num*(nodeid1-1) + dim1, dim_num*(nodeid2-1) + dim2 ) + &
							 (obj%DeformStressMat(i, int1, int2 ))
					enddo
				enddo
			enddo
        enddo
    enddo
    !===================================
    
    
	call GetTractionVector(obj)

	call GetInternalVector(obj)
	call GetResidualVector(obj)
	bvec(:)=obj%ResidualVecGlo(:)


    !===================================
	! introduce D.B.C
	do k=1,size(obj%FEMDomain%Boundary%DBoundNum)
    	do i=1,size(obj%FEMDomain%Boundary%DBoundNodID,1)
    	    nodeid1=obj%FEMDomain%Boundary%DBoundNodID(i,k)
    	    if(nodeid1<1)then
    	        cycle
			else
				


    	        val=obj%FEMDomain%Boundary%DBoundValInc(i,k)
				do j=1,size(bvec)
    	            bvec(j)=bvec(j)-Amat(j,dim_num*(nodeid1-1) + k)*val*dble(SetBC)
    	        enddo
    	        Amat(dim_num*(nodeid1-1) + k,:)         = 0.0d0
    	        Amat(:,dim_num*(nodeid1-1) + k)         = 0.0d0
				Amat(dim_num*(nodeid1-1) + k,dim_num*(nodeid1-1) + k)   = 1.0d0
    	        bvec(dim_num*(nodeid1-1) + k)           = val*dble(SetBC)
    	    endif
		enddo
	enddo
    !===================================

	obj%ResidualVecGlo(:)=bvec(:)
    
    
    itrmax=1000
    er=1.0e-15
    n=size(bvec)
	xvec(:)=0.0d0

    if(trim(solver(1:11) )=="GaussJordan")then
        print *, "Solver type :: GaussJordan"
        call  gauss_jordan_pv(Amat, xvec, bvec, n)
    elseif(trim(solver(1:8) )=="BiCGSTAB")then
		print *, "Solver type :: BiCGSTAB"
		call bicgstab_dirichlet(Amat, bvec, xvec, size(xvec), itrmax, er,&
			obj%FEMDomain%Boundary%DBoundNodID,obj%FEMDomain%Boundary%DBoundValInc,SetBC)
	else
		print *, "Solver Name :",trim(solver)
        print *, "Critical ERROR :: No solvers are selected in FiniteDeform_"
        stop 
    endif
	
!	print *, size(bvec)
!	call showArray(Amat,name="Amat.txt")
!	call f%open("Bvec.txt")
!	do i=1,size(bvec)
!		writE(f%fh,*) bvec(:)
!	enddo
!	call f%close()
!	
!	stop

    
    !=====================================
    ! Export Values to Element-by-Element form
    do i=1,elem_num
        do j=1, node_num_elmtl
            nodeid1=obj%FEMDomain%Mesh%ElemNod(i,j)
            do dim1=1,dim_num
				obj%DeformVecEBEInc(i, dim_num*(j-1) + dim1)=&
				obj%DeformVecEBEInc(i, dim_num*(j-1) + dim1) + &
				   (xvec( (dim_num*(nodeid1-1)) + dim1 ))
			enddo
        enddo
	enddo
	obj%DeformVecGloInc(:)=obj%DeformVecGloInc(:) + xvec(:)

	if(dot_product(obj%DeformVecGloInc,obj%DeformVecGloInc) ==0.0d0)then
		obj%error=	dot_product(xvec,xvec)
	else
		obj%error = abs(1.0d0- dot_product(obj%DeformVecGloInc-xvec,obj%DeformVecGloInc-xvec)/&
		dot_product(obj%DeformVecGloInc,obj%DeformVecGloInc))
	endif

    !=====================================
	
    
end subroutine

!######################## Solve DiffusionEq ########################



!######################## Display Finite Deformation ########################
subroutine resultFiniteDeform(obj,path,name,step)
    class(FiniteDeform_),intent(inout)::obj
	character(*),intent(in) :: Name,path
	integer(int32),optional,intent(in)::step
	real(real64),allocatable::DBCvec(:,:),DispVector(:,:)
    integer(int32) :: i,j,n,m,fstep,dim_num

	if(size(obj%FEMDomain%Mesh%NodCoord,2)==2)then
		! 2-D condition
    	if(present(step) )then
    	    fstep=step
    	else
    	    fstep=1
    	endif


		call GmshExportStress(obj%FEMDomain,obj%DeformVecGloTot,obj%DeformStress,&
		obj%DeformStrain,fstep,Name=trim(path)//"/"//trim(adjustl(name)))
		call obj%getDispVector(DispVector)
		call GmshPlotVector(obj%FEMDomain,Vector=DispVector,Name=trim(path)//"/"&
		//trim(adjustl(name)),FieldName="Displacement",&
		Step=step,withMsh=.true.,NodeWize=.true.,onlyDirichlet=.true.)

	elseif(size(obj%FEMDomain%Mesh%NodCoord,2)==3)then
		
		! 3-D condition
		if(present(step) )then
    	    fstep=step
    	else
    	    fstep=1
    	endif
		call GmshExportStress(obj%FEMDomain,obj%DeformVecGloTot,obj%DeformStress,&
		obj%DeformStrain,step,Name=trim(path)//"/"//trim(adjustl(name)) )
		call obj%getDBCVector(DBCvec)
		call GmshPlotVector(obj=obj%FEMDomain,Vector=DBCvec,Name=trim(path)//"/"&
		//trim(adjustl(name)),Step=fstep,FieldName="DispBoundary",&
		NodeWize=.true. )
		!call obj%exportAsPly()

	endif
end subroutine 
!######################## Display Finite Deformation ########################


subroutine exportAsPlyFiniteDeform(obj)
	class(FiniteDeform_),intent(inout) :: obj
	real(real64),allocatable :: scalar(:)
	integer(int32) :: n,m
	n=size(obj%FEMDomain%Mesh%NodCoord,1)

	allocate(scalar(n) )

	
end subroutine

!######################## Display Finite Deformation ########################
subroutine DisplayDeformStress(obj,DisplayMode,OptionalStep,Name,withDirichlet)
    class(FiniteDeform_),intent(inout)::obj
	character(*),optional,intent(in) :: Name,DisplayMode
	logical,optional,intent(in) :: withDirichlet
	integer(int32),optional,intent(in)::OptionalStep
	real(real64),allocatable::DBCvec(:,:),DispVector(:,:)
    integer(int32) :: i,j,n,m,step,dim_num

    if(.not.associated(obj%FEMDomain) )then
        return
    endif
	if(obj%FEMDomain%Mesh%empty() .eqv. .true. )then
		return
	endif
	if(size(obj%FEMDomain%Mesh%NodCoord,2)==2)then
		! 2-D condition
    	if(present(OptionalStep) )then
    	    step=OptionalStep
    	else
    	    step=1
    	endif

    	if(present(DisplayMode) )then
    	    if(trim(DisplayMode)=="Terminal" .or. trim(DisplayMode)=="terminal")then
    	        do i=1,size(obj%DeformVecEBEInc,1)
    	            print *, obj%DeformVecEBEInc(i,:)
    	        enddo
    	    elseif(trim(DisplayMode)=="gmsh" .or. trim(DisplayMode)=="Gmsh" )then   
				call GmshExportStress(obj%FEMDomain,obj%DeformVecGloTot,obj%DeformStress,obj%DeformStrain,step,Name=Name)
				call obj%getDispVector(DispVector)
				call GmshPlotVector(obj%FEMDomain,Vector=DispVector,Name=Name,FieldName="Displacement",&
				Step=step,withMsh=.true.,NodeWize=.true.,onlyDirichlet=.true.)

    	    elseif(trim(DisplayMode)=="gnuplot" .or. trim(DisplayMode)=="Gnuplot" )then
				call GnuplotExportStress(obj%FEMDomain,obj%DeformVecGloTot,obj%DeformStress,obj%DeformStrain,step )
    	    else
    	        print *, "Invalid DisplayMode >> DisplayDiffusionEq "
    	        print *, "DisplayMode '",trim(DisplayMode),"' is not defined" 
    	    endif
    	    return
		endif
	elseif(size(obj%FEMDomain%Mesh%NodCoord,2)==3)then
		
		! 3-D condition
		if(present(OptionalStep) )then
    	    step=OptionalStep
    	else
    	    step=1
    	endif

    	if(present(DisplayMode) )then
    	    if(trim(DisplayMode)=="Terminal" .or. trim(DisplayMode)=="terminal")then
    	        do i=1,size(obj%DeformVecEBEInc,1)
    	            print *, obj%DeformVecEBEInc(i,:)
    	        enddo
			elseif(trim(DisplayMode)=="gmsh" .or. trim(DisplayMode)=="Gmsh" )then   
	
				call GmshExportStress(obj%FEMDomain,obj%DeformVecGloTot,obj%DeformStress,obj%DeformStrain,step,Name=Name )
				if(present(withDirichlet) )then
					if(withDirichlet .eqv. .true.)then
						! Export dirichlet (Deformation) boundary conditions
						call obj%getDBCVector(DBCvec)
						call GmshPlotVector(obj=obj%FEMDomain,Vector=DBCvec,Name=Name,Step=step,FieldName="DispBoundary",NodeWize=.true. )
					endif
				endif
    	    elseif(trim(DisplayMode)=="gnuplot" .or. trim(DisplayMode)=="Gnuplot" )then
				call GnuplotExportStress(obj%FEMDomain,obj%DeformVecGloTot,obj%DeformStress,obj%DeformStrain,step )
    	    else
    	        print *, "Invalid DisplayMode >> DisplayDiffusionEq "
    	        print *, "DisplayMode '",trim(DisplayMode),"' is not defined" 
    	    endif
    	    return
		endif
	else
		stop "only 2D and 3D can be displayed"
	endif
end subroutine 
!######################## Display Finite Deformation ########################


!############# Get Traction Vector ######################
subroutine GetTractionVector(obj)
	class(FiniteDeform_),intent(inout)::obj

	integer(int32) :: i,j,dim_num,node

	if(allocated(obj%TractionVecGlo) .and. size(obj%DeformVecGloTot )/=size(obj%TractionVecGlo) )then
		deallocate(obj%TractionVecGlo)
	endif
	if(.not.allocated(obj%TractionVecGlo))then
		allocate(obj%TractionVecGlo(size(obj%DeformVecGloTot ) ) )
	endif
	
	
	obj%TractionVecGlo(:)=0.0d0
	if(allocated(obj%FEMDomain%Boundary%NBoundNodID) )then
		dim_num=size(obj%FEMDomain%Boundary%NBoundNodID,2 )
		if(dim_num==0)then
			return
		endif
		
		do i=1, size(obj%FEMDomain%Boundary%NBoundNodID,1 )
			do j=1, size(obj%FEMDomain%Boundary%NBoundNodID,2 )
				if(obj%FEMDomain%Boundary%NBoundNodID(i,j)>0 )then
					node=obj%FEMDomain%Boundary%NBoundNodID(i,j)
					obj%TractionVecGlo(dim_num*(node-1) + j )=obj%TractionVecGlo(dim_num*(node-1) + j ) + &
						obj%FEMDomain%Boundary%NBoundVal(i,j)
				endif
			enddo
		enddo
	endif
end subroutine
!############# Get Traction Vector ######################


!############# Get Internal Vector ######################
subroutine GetInternalVector(obj)
	class(FiniteDeform_),intent(inout)::obj

	integer(int32) :: i,j,dim_num,node_num,nodeid1,node_num_elmtl,elem_num,dim1


	node_num=size(obj%FEMDomain%Mesh%NodCoord,1)
	dim_num=size(obj%FEMDomain%Mesh%NodCoord,2)
	elem_num=size(obj%FEMDomain%Mesh%ElemNod,1)
	node_num_elmtl=size(obj%FEMDomain%Mesh%ElemNod,2)
	if(allocated(obj%InternalVecGlo) .and. size(obj%DeformVecGloTot )/=size(obj%InternalVecGlo) )then
		deallocate(obj%InternalVecGlo)
	endif
	if(.not.allocated(obj%InternalVecGlo))then
		allocate(obj%InternalVecGlo(size(obj%DeformVecGloTot ) ) )
	endif
	obj%InternalVecGlo(:)=0.0d0
	do i=1,elem_num
        do j=1, node_num_elmtl
            nodeid1=obj%FEMDomain%Mesh%ElemNod(i,j)
            do dim1=1,dim_num
				obj%InternalVecGlo(dim_num*(nodeid1-1) + dim1 )=&
				obj%InternalVecGlo(dim_num*(nodeid1-1) + dim1 ) + &
				  (obj%DeformStressRHS(i, (dim_num*(j-1)) + dim1)	)	
			enddo
        enddo
    enddo
	
end subroutine
!############# Get Internal Vector ######################



!############# Get Residual Vector ######################
subroutine GetResidualVector(obj)
	class(FiniteDeform_),intent(inout)::obj

	integer(int32) :: i,j,dim_num,node

	if(allocated(obj%ResidualVecGlo) .and. size(obj%DeformVecGloTot )/=size(obj%ResidualVecGlo) )then
		deallocate(obj%ResidualVecGlo)
	endif
	if(.not.allocated(obj%ResidualVecGlo))then
		allocate(obj%ResidualVecGlo(size(obj%DeformVecGloTot ) ) )
	endif
	
	
	obj%ResidualVecGlo(:)=obj%TractionVecGlo(:)-obj%InternalVecGlo(:)

end subroutine
!############# Get Residual Vector ######################


subroutine UpdateStressMeasure(obj)
	class(FiniteDeform_),intent(inout)::obj
	if(obj%infinitesimal .eqv. .true.)then
		obj%DeformStress(:,:,:)=obj%DeformStress(:,:,:)+obj%DeformStressinc(:,:,:)
		obj%DeformStressinc(:,:,:)=0.0d0
	endif
end subroutine

!############# Uodate strain variables ######################
subroutine UpdateStrainMeasure(obj)
	class(FiniteDeform_),intent(inout)::obj

	integer(int32) :: i,j,dim_num

	dim_num=size(obj%FEMDomain%Mesh%NodCoord,2)

	obj%VolInitCurrEBE(:,3)=obj%VolInitCurrEBE(:,2)-obj%VolInitCurrEBE(:,1)
	obj%VolInitCurrEBE(:,1)=obj%VolInitCurrEBE(:,2)
	do i=1,size(obj%DeformStrain,1)
		
		do j=1,size(obj%DeformStrain,2)
			obj%DeformStrain(i,j, 1)=obj%DeformStrain(i,j, 4)			
			obj%DeformStrain(i,j, 2)=obj%DeformStrain(i,j, 5)
			obj%DeformStrain(i,j, 3)=obj%DeformStrain(i,j, 6)
			obj%DeformStrain(i,j, 7)=obj%DeformStrain(i,j, 11)
			obj%DeformStrain(i,j, 8)=obj%DeformStrain(i,j, 12)
			obj%DeformStrain(i,j, 9)=obj%DeformStrain(i,j, 13)
			obj%DeformStrain(i,j, 10)=obj%DeformStrain(i,j, 14)		

			if(dim_num==3)then
				obj%DeformStrain(:,:,16) = obj%DeformStrain(:,:,19) 
				obj%DeformStrain(:,:,17) = obj%DeformStrain(:,:,20) 
				obj%DeformStrain(:,:,18) = obj%DeformStrain(:,:,21) 
				obj%DeformStrain(:,:,22) = obj%DeformStrain(:,:,27)
				obj%DeformStrain(:,:,23) = obj%DeformStrain(:,:,28)
				obj%DeformStrain(:,:,24) = obj%DeformStrain(:,:,29)
				obj%DeformStrain(:,:,25) = obj%DeformStrain(:,:,30)
				obj%DeformStrain(:,:,26) = obj%DeformStrain(:,:,31)


				obj%DeformStrain(:,:,32) =  obj%DeformStrain(:,:,41)
				obj%DeformStrain(:,:,33) =  obj%DeformStrain(:,:,42)
				obj%DeformStrain(:,:,34) =  obj%DeformStrain(:,:,43)
				obj%DeformStrain(:,:,35) =  obj%DeformStrain(:,:,44)
				obj%DeformStrain(:,:,36) =  obj%DeformStrain(:,:,45)
				obj%DeformStrain(:,:,37) =  obj%DeformStrain(:,:,46)
				obj%DeformStrain(:,:,38) =  obj%DeformStrain(:,:,47)
				obj%DeformStrain(:,:,39) =  obj%DeformStrain(:,:,48)
				obj%DeformStrain(:,:,40) =  obj%DeformStrain(:,:,49)
		
			endif

		enddo
	enddo
	
end subroutine
!############# Uodate strain variables ######################


!############# Reaction Force at Loading Dirichlet Boundary ######################
subroutine DisplayReactionForce(obj,id)
	class(FiniteDeform_),intent(in)::obj
	integer(int32),optional,intent(in) :: id

	integer(int32) :: i,j,k,dim_num,dbc_num
	character(200) :: filename
	real(real64),allocatable :: ReactionForce(:),ReactionForce_wall(:)
	real(real64) :: val

	dim_num=size(obj%FEMDomain%Boundary%DBoundNodID,2)
	dbc_num=size(obj%FEMDomain%Boundary%DBoundNodID,1)
	allocate(ReactionForce(dim_num),ReactionForce_wall(dim_num) )
	ReactionForce(:)=0.0d0
	ReactionForce_wall(:)=0.0d0
	do i=1,dim_num
		do j=1,dbc_num
			k=obj%FEMDomain%Boundary%DBoundNodID(j,i)
			val=obj%FEMDomain%Boundary%DBoundValInc(j,i)
			if(k<1)then
				cycle
			elseif(k>=1 .and. Val/=0.0d0 )then
				ReactionForce(i)=ReactionForce(i) + obj%InternalVecGlo( dim_num*(k-1) + i )
			elseif(k>=1 .and. Val==0.0d0 )then
				ReactionForce_wall(i)=ReactionForce_wall(i) + abs(obj%InternalVecGlo( dim_num*(k-1) + i ))
			else
				cycle
			endif
		enddo
	enddo

	k=input(default=0,option=id)
	filename="ReactionForce"//trim(str(k))//"_wall.txt"
	if(obj%Step==1)then
		open(101,file="all_"//trim(filename),status="replace")
	else
		open(101,file="all_"//trim(filename),position="append")
	endif
	write(101,*) obj%Step,ReactionForce(:)
	close(101)

	if(obj%Step==1)then
		open(101,file=trim(filename),status="replace")
	else
		open(101,file=trim(filename),position="append")
	endif
	write(101,*) obj%Step,ReactionForce_wall(:)
	close(101)

end subroutine
!############# Reaction Force at Loading Dirichlet Boundary ######################

! ##################################################
subroutine getDBCVectorDeform(obj,DBCvec)
	class(FiniteDeform_),intent(in)::obj
	real(real64),allocatable,intent(inout)::DBCvec(:,:)
	integer(int32) :: i,j,n,m,k,l
	n=size(obj%FEMDomain%Mesh%NodCoord,1)
	m=size(obj%FEMDomain%Mesh%NodCoord,2)
	if(.not. allocated(DBCvec ) )then
		allocate(DBCvec(n,m) )
		DBCvec(:,:)=0.0d0
	endif

	! check number of DBC
	do i=1,size(obj%FEMDomain%Boundary%DBoundNum)
		k=countif(Array=obj%FEMDomain%Boundary%DBoundNodID(:,i),Value=-1,notEqual=.true.)
		l=obj%FEMDomain%Boundary%DBoundNum(i)
		if(k /= l)then
			print *, "Caution :: FiniteDeformationClass::getDBCVector :: check number of DBC :: k /= l"
		endif
	enddo

	do i=1,size(obj%FEMDomain%Boundary%DBoundNodID,1)
		do j=1,size(obj%FEMDomain%Boundary%DBoundNodID,2)
			if(obj%FEMDomain%Boundary%DBoundNodID(i,j) <=0)then
				cycle
			endif
			DBCvec(obj%FEMDomain%Boundary%DBoundNodID(i,j),j )=obj%FEMDomain%Boundary%DBoundVal(i,j)
		enddo
	enddo


end subroutine
! ##################################################



! ##################################################
subroutine getDispVectorDeform(obj,Vector)
	class(FiniteDeform_),intent(in)::obj
	real(real64),allocatable,intent(inout)::Vector(:,:)
	integer(int32) :: i,j,n,m

	n=size(obj%FEMDomain%Mesh%NodCoord,1)
	m=size(obj%FEMDomain%Mesh%NodCoord,2)
	if(.not.allocated(Vector) )then
		allocate(Vector(n,m) )
		Vector(:,:)=0.0d0
	endif

	do i=1,n
		do j=1,m
			Vector(i,j)=obj%DeformVecGloTot( m*(i-1)+j)
		enddo
	enddo
end subroutine
! ##################################################


! ##################################################
subroutine exportFiniteDeform(obj,restart,path)
	class(FiniteDeform_),intent(inout) :: obj
	logical,optional, intent(in) :: restart
	character(*),intent(in) :: path
	type(IO_) :: f, ff
	integer(int32) :: fh_

	if(present(restart) )then
		! make new dir 
		call execute_command_line("mkdir -p "//trim(path)//"/FiniteDeform")
		
		if(associated(obj%FEMDomain) )then
    		call obj%FEMDomain%export(path=path//"/FiniteDeform",restart=restart)
		endif

		call f%open("./",trim(path)//"/FiniteDeform","/FiniteDeform.prop")
		write(f%fh, *) obj%DeformStress(:,:,:)
		write(f%fh, *) obj%DeformStrain(:,:,:)
		write(f%fh, *) obj%DeformStressInit(:,:,:)
		write(f%fh, *) obj%DeformStressinc(:,:,:)
		write(f%fh, *) obj%DeformStressMat(:,:,:)
		write(f%fh, *) obj%DeformStressRHS(:,:)
		write(f%fh, *) obj%DeformVecEBETot(:,:)
		write(f%fh, *) obj%DeformVecEBEInc(:,:)
		write(f%fh, *) obj%DeformVecGloTot(:)
		write(f%fh, *) obj%DeformVecGloInc(:) 
		write(f%fh, *) obj%TractionVecGlo(:)
		write(f%fh, *) obj%ResidualVecGlo(:)
		write(f%fh, *) obj%InternalVecGlo(:)
		write(f%fh, *) obj%VolInitCurrEBE(:,:)
		write(f%fh, *) obj%YoungsModulus(:) 
		write(f%fh, *) obj%PoissonsRatio(:) 
		write(f%fh, *) obj%PorePressure(:)  
		write(f%fh, *) obj%dt,obj%error,obj%reactionforce
		write(f%fh, *) obj%nr_tol
		write(f%fh, *) obj%ReducedIntegration
		write(f%fh, *) obj%infinitesimal
		write(f%fh, *) obj%itr
		write(f%fh, *) obj%Step
		call f%close()
	endif

end subroutine
! ##################################################


! ##################################################
function getVolumeDeform(obj) result(volume)
	class(FiniteDeform_),intent(inout) :: obj
	real(real64),allocatable :: volume(:)
	integer(int32) :: numnode, numelem,i,j,gp_num


	obj%FEMDomain%ShapeFunction%ElemType=obj%FEMDomain%Mesh%GetElemType()
	call SetShapeFuncType(obj%FEMDomain%ShapeFunction)
	gp_num=obj%FEMDomain%ShapeFunction%NumOfGp
	allocate(volume(size(obj%FEMDomain%Mesh%ElemNod,1)) )
	do i = 1, size(obj%FEMDomain%Mesh%ElemNod,1)
		do j = 1, obj%FEMDomain%ShapeFunction%NumOfGp !�K�E�X�ϕ����ƃ��[�v
			! -----J�}�g���N�X�̌v�Z-----------------------------------------
			call GetAllShapeFunc(obj%FEMDomain%ShapeFunction,elem_id=i,nod_coord=obj%FEMDomain%Mesh%NodCoord,&
			elem_nod=obj%FEMDomain%Mesh%ElemNod,OptionalGpID=j)
			volume(i) = obj%FEMDomain%ShapeFunction%detJ
		enddo
	enddo

end function getVolumeDeform
! ##################################################


end module FiniteDeformationClass

