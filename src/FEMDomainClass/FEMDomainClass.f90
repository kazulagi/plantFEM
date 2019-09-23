module FEMDomainClass
	use MathClass
    use ArrayOperationClass
    use ShapeFunctionClass
    use MeshOperationClass
    use MaterialPropClass
    use BoundaryConditionClass
    use ControlParaeterClass

    implicit none

    type::FEMDomain_
        type(ShapeFunction_)    :: ShapeFunction
        type(Mesh_)             :: Mesh
        type(MaterialProp_)     :: MaterialProp
        type(Boundary_)         :: Boundary
        type(ControlParameter_) :: ControlPara
        real(8) :: RealTime
        character*200 :: FilePath
        character*200 :: FileName
        character*9 :: Dtype
		character*20 :: SolverType
		integer :: timestep
    contains
        procedure,public :: Init   => InitializeFEMDomain
        procedure,public :: Delete => DeallocateFEMDomain
        procedure,public :: Export => ExportFEMDomain
        procedure,public :: Import => ImportFEMDomain
        procedure,public :: Merge  => MergeFEMDomain
        procedure,public :: AddDBoundCondition => AddDBoundCondition
        procedure,public :: AddNBoundCondition => AddNBoundCondition
        procedure,public :: AddTBoundCondition => AddTBoundCondition
        procedure,public :: AddMaterialID => AddMaterialID
        procedure,public :: SetDataType => SetDataType
        procedure,public :: SetSolver => SetSolver 
        procedure,public :: SetName => SetName 
        procedure,public :: SetUp      => SetUpFEMDomain
        procedure,public :: InitDBC => InitDBC
        procedure,public :: InitNBC => InitNBC
		procedure,public :: InitTBC => InitTBC
		procedure,public :: AddNBC => AddNBCFEMDomain 
        procedure :: MeltingSkelton => MeltingSkeltonFEMDomain
        procedure,public :: SetControlPara =>  SetControlParaFEMDomain
        procedure,public :: GmshPlotMesh => GmshPlotMesh
		procedure,public :: GmshPlotContour => GmshPlotContour
		procedure,public :: GmshPlotVector => GmshPlotVector 
        procedure,public :: GmshPlotContour2D => GmshPlotContour2D
        procedure,public :: GnuplotPlotContour  => GnuplotPlotContour   
		procedure,public :: GnuplotExportStress => GnuplotExportStress  
		procedure,public :: getDBCVector => getDBCVectorFEMDomain
		procedure,public :: move => moveFEMDomain
		procedure,public :: rotate => rotateFEMDomain
		procedure,public :: meshing => meshingFEMDomain

		! for debug
		procedure,public :: CheckConnectivity => CheckConnedctivityFEMDomain
        
    end type FEMDomain_

    type,extends(FEMDomain_) :: STFEMDomain_
        type(ShapeFunction_)    :: TimeShapeFunction
        type(Mesh_)             :: TimeMesh
    end type

contains




!##################################################
subroutine DeallocateFEMDomain(obj)
    class(FEMDomain_),intent(inout)::obj

    call DeallocateMesh(obj%Mesh)
    call DeallocateMaterialProp(obj%MaterialProp)
    call DeallocateBoundary(obj%Boundary)
    call DeallocateShapeFunction(obj%ShapeFunction)    

end subroutine DeallocateFEMDomain
!##################################################






!##################################################
subroutine InitializeFEMDomain(obj,Default)
    class(FEMDomain_),intent(inout)::obj
    logical,optional,intent(in)::Default

    if(Default .eqv. .true.)then
        obj%Dtype="FEMDomain"
    endif
    call InitializeMesh(obj%Mesh)
    call InitializeMaterial(obj%MaterialProp)
	call obj%Boundary%Init(Default)
	obj%timestep=0
    
    
    

end subroutine InitializeFEMDomain
!##################################################


!##################################################
subroutine ImportFEMDomain(obj,OptionalFileFormat,OptionalProjectName,FileHandle)
    class(FEMDomain_),intent(inout)::obj
    character*4,optional,intent(in)::OptionalFileFormat
    character(*),optional,intent(in)::OptionalProjectName
    
    include "./ImportFEMDomain.f90"

end subroutine ImportFEMDomain
!##################################################


!##################################################
subroutine MergeFEMDomain(inobj1,inobj2,outobj)
    class(FEMDomain_),intent(in) ::inobj1,inobj2
    class(FEMDomain_),intent(out)::outobj
    
    include "./MergeFEMDomain.f90"

end subroutine MergeFEMDomain
!##################################################


!##################################################
subroutine ExportFEMDomain(obj,OptionalFileFormat,OptionalProjectName,FileHandle,SolverType,MeshDimension)
    class(FEMDomain_),intent(inout)::obj
    character(*),optional,intent(in)::OptionalFileFormat
    character(*),optional,intent(in)::OptionalProjectName,SolverType
    
    character*4::FileFormat
    character*200::ProjectName
    character*200 ::FileName
    integer,allocatable::IntMat(:,:)
    real(8),allocatable::RealMat(:,:)
    integer,optional,intent(in)::FileHandle,MeshDimension
    integer :: fh,i,j,k,NumOfDomain,n,m,DimNum,GpNum
	character*70 Msg
	
	if(present(OptionalFileFormat) )then
		if(OptionalFileFormat=="stl" .or. OptionalFileFormat==".stl")then
			call ExportFEMDomainAsSTL(obj,OptionalProjectName,FileHandle,SolverType,MeshDimension)
			return
		endif
	endif

	ProjectName = ""
	FileName=""
	Msg=""


    if(present(FileHandle) )then
        fh=FileHandle
    else
        fh =104
    endif



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

    !!print *, "Project : ",ProjectName
    !!print *, "is Exported as : ",FileFormat," format"
    !!print *, "File Name is : ",FileName

    open(fh,file=trim(FileName),status="replace")


    if(trim(FileFormat)==".scf" )then
        
        NumOfDomain=size(obj%Mesh%SubMeshNodFromTo,1)

		obj%Dtype="domain"
        write(fh,'(A)') obj%Dtype
        write(*,'(A)') obj%Dtype,trim(FileName)
        write(fh,*) "  "
        write(fh,'(A)') obj%SolverType
        write(fh,*) "  "
        write(fh,*) NumOfDomain
        write(fh,*) "  "

        print *, "########### Meta Info ###########"
        print *, obj%Dtype
        print *, obj%SolverType
        print *, NumOfDomain
        print *, "########### Meta Info ###########"
        
    
        do i=1,NumOfDomain
            write(fh,*) obj%Mesh%SubMeshNodFromTo(i,2),obj%Mesh%SubMeshNodFromTo(i,3)
        enddo
        write(fh,*) "  "
        do i=1,NumOfDomain
            write(fh,*) obj%Mesh%SubMeshElemFromTo(i,3)
        enddo
        write(fh,*) "  "

        print *, "########### Domain info ###########"
        do i=1,NumOfDomain
            !write(*,*) obj%Mesh%SubMeshNodFromTo(i,2),obj%Mesh%SubMeshNodFromTo(i,3)
        enddo
        do i=1,NumOfDomain
            !write(*,*) obj%Mesh%SubMeshElemFromTo(i,3)
        enddo
        
        print *, "########### Domain info ###########"

        n=size(obj%Mesh%NodCoord,1)
        m=size(obj%Mesh%NodCoord,2)
        if(present(MeshDimension) )then
            m=MeshDimension
        endif
        write(fh,*) n,m
        DimNum=m

        write(fh,*) "  "
        do i=1,n
            write(fh,*) obj%Mesh%NodCoord(i,1:m)
        enddo
        flush(fh)

        print *, " "
        print *, "########### Node info ###########"
        print *, "Number of node : ",n, "Dimension : ",m
        print *, "########### Node info ###########"
        print *, " "


        n=size(obj%Mesh%ElemNod,1)
        m=size(obj%Mesh%ElemNod,2)
        write(fh,*) n,m
		write(fh,*) "  "
		
        write(fh,'(A)') trim(obj%Mesh%getElemType() )
        write(fh,*) "  "
        do i=1,n
            write(fh,*) obj%Mesh%ElemNod(i,:)
            if(obj%Mesh%ElemNod(i,1)==0 )then
                exit
            endif
        enddo
        write(fh,*) "  "
        flush(fh)


        print *, " "
        print *, "########### Element info ###########"
        write(*,'(A)') "Element Type : ",trim(obj%Mesh%ElemType)
        print *, "Number of Element : ",n, "Number of node per element : ",m
        print *, "Successfully Exported"
        print *, "########### Element info ###########"
        print *, " "

        n=size(obj%Mesh%ElemNod,1)
        do i=1,n
            write(fh,*) obj%Mesh%ElemMat(i)
        enddo
        write(fh,*) "  "
        
        n=size(obj%MaterialProp%MatPara,1)
        m=size(obj%MaterialProp%MatPara,2)
        write(fh,*) n,m
        do i=1,n
            write(fh,*) obj%MaterialProp%MatPara(i,:)
        enddo
        write(fh,*) "  "
        flush(fh)

        print *, "########### Material info ###########"
        n=size(obj%Mesh%ElemNod,1)
        !write(*,*) size(obj%Mesh%ElemMat,1)
        n=size(obj%MaterialProp%MatPara,1)
        m=size(obj%MaterialProp%MatPara,2)
        !write(*,*) n,m
        do i=1,n
            !write(*,*) obj%MaterialProp%MatPara(i,:)
        enddo
        print *, "Successfully Exported"
        print *, "########### Material info ###########"
        
        !DirichletBoundary


        if(.not.allocated(obj%Boundary%DBoundNum))then
            
            write(fh,*) "0" !DirichletBoundaryDimension
            write(fh,*) "  "
            stop "gfsf"
            !print *, "ImportFEMDomain >> Caution :: no Dirichlet Boundary Condition is loaded. "
        else

            n=size(obj%Boundary%DBoundNum)
            write(fh,*) n !DirichletBoundaryDimension
            write(fh,*) "  "

            !allocate(obj%Boundary%DBoundNum(n ))
            write(fh,*) obj%Boundary%DBoundNum(:)
            write(fh,*) "  "
            !allocate(obj%Boundary%DBoundNodID( maxval(obj%Boundary%DBoundNum), size(obj%Boundary%DBoundNum)  )  )
            !allocate(obj%Boundary%DBoundVal( maxval(obj%Boundary%DBoundNum), size(obj%Boundary%DBoundNum)  )  )

            !obj%Boundary%DBoundNodID(:,:)=-1
            !obj%Boundary%DBoundVal(:,:)  =0.0d0

            do i=1,size(obj%Boundary%DBoundNum,1)
                do j=1,obj%Boundary%DBoundNum(i)
					write(fh,*) obj%Boundary%DBoundNodID(j,i)
                    !!print *,obj%Boundary%DBoundNodID(j,i)
                enddo
                write(fh,*) "  "
                do j=1,obj%Boundary%DBoundNum(i)
                    write(fh,*) obj%Boundary%DBoundVal(j,i)
                    !!print *,obj%Boundary%DBoundVal(j,i)
                enddo
                write(fh,*) "  "
            enddo

        endif





        print *, "########### Dirichlet Boundary info ###########"
        if(.not.allocated(obj%Boundary%DBoundNum))then
            
            write(*,*) "0" !DirichletBoundaryDimension
            write(*,*) "  "
            stop "ERROR :: FEMDomainClass :: no Dirichlet boundary is found"
            !print *, "ImportFEMDomain >> Caution :: no Dirichlet Boundary Condition is loaded. "
        else

            n=size(obj%Boundary%DBoundNum)
            !write(*,*) n !DirichletBoundaryDimension
            !write(*,*) "  "

            !allocate(obj%Boundary%DBoundNum(n ))
            !write(*,*) obj%Boundary%DBoundNum(:)
            !write(*,*) "  "
            !allocate(obj%Boundary%DBoundNodID( maxval(obj%Boundary%DBoundNum), size(obj%Boundary%DBoundNum)  )  )
            !allocate(obj%Boundary%DBoundVal( maxval(obj%Boundary%DBoundNum), size(obj%Boundary%DBoundNum)  )  )

            !obj%Boundary%DBoundNodID(:,:)=-1
            !obj%Boundary%DBoundVal(:,:)  =0.0d0

            do i=1,size(obj%Boundary%DBoundNum,1)
                do j=1,obj%Boundary%DBoundNum(i)
                    !write(*,*) obj%Boundary%DBoundNodID(j,i)
                    !!print *,obj%Boundary%DBoundNodID(j,i)
                enddo
                !write(*,*) "  "
                do j=1,obj%Boundary%DBoundNum(i)
                    !write(*,*) obj%Boundary%DBoundVal(j,i)
                    !!print *,obj%Boundary%DBoundVal(j,i)
                enddo
                !write(*,*) "  "
            enddo

        endif

        print *, "Successfully Exported"
        print *, "########### Dirichlet Boundary info ###########"
        
        

        if(.not.allocated(obj%Boundary%NBoundNum) )then
            DimNum=0
        else
            DimNum=size(obj%Boundary%NBoundNum,1)
        endif


        write(fh,*) DimNum
        write(fh,*) "  "
        if(DimNum<=0)then
            !print *, "ImportFEMDomain >> Caution :: no Neumann Boundary Condition is loaded. "
        else
            n=size(obj%Boundary%NBoundNodID,1)
            write(fh,*) n
            write(fh,*) "  "
            !allocate( obj%Boundary%NBoundNum(DimNum))
            !allocate(obj%Boundary%NBoundNodID(n, size(obj%Boundary%NBoundNum)  )  )
            !allocate(obj%Boundary%NBoundVal( n, size(obj%Boundary%NBoundNum)  )  )
            !obj%Boundary%NBoundNodID(:,:)=-1
            !obj%Boundary%NBoundVal(:,:)  =0.0d0

            !obj%Boundary%NBoundNum(:)=n
            do i=1,n
                write(fh,*) obj%Boundary%NBoundNodID(i,:)
                !obj%Boundary%NBoundNodID(i,:)=m
            enddo
            write(fh,*) "  "

            do i=1,n
                write(fh,*) obj%Boundary%NBoundVal(i,:)
            enddo
            write(fh,*) "  "
        
        endif
            


        print *, "########### Neumann Boundary info ###########"

        if(.not.allocated(obj%Boundary%NBoundNum) )then
            DimNum=0
        else
            DimNum=size(obj%Boundary%NBoundNum,1)
        endif
        !write(*,*) DimNum
        !write(*,*) "  "
        if(DimNum<=0)then
            !print *, "ImportFEMDomain >> Caution :: no Neumann Boundary Condition is loaded. "
        else
            n=size(obj%Boundary%NBoundNodID,1)
            !write(*,*) n
            !write(*,*) "  "
            !allocate( obj%Boundary%NBoundNum(DimNum))
            !allocate(obj%Boundary%NBoundNodID(n, size(obj%Boundary%NBoundNum)  )  )
            !allocate(obj%Boundary%NBoundVal( n, size(obj%Boundary%NBoundNum)  )  )
            !obj%Boundary%NBoundNodID(:,:)=-1
            !obj%Boundary%NBoundVal(:,:)  =0.0d0

            !obj%Boundary%NBoundNum(:)=n
            do i=1,n
                !write(*,*) obj%Boundary%NBoundNodID(i,:)
                !obj%Boundary%NBoundNodID(i,:)=m
            enddo
            !write(*,*) "  "

            do i=1,n
                !write(*,*) obj%Boundary%NBoundVal(i,:)
            enddo
            !write(*,*) "  "
        
        endif
        print *, "Successfully Exported"
        print *, "########### Neumann Boundary info ###########"
        
        



        print *, "########### Initial Condition info ###########"
        
        !######### Initial conditions #################
        ! For node-wize
        
        if(.not.allocated(obj%Boundary%TBoundVal) )then
            DimNum=0
        else
            DimNum=size(obj%Boundary%TBoundVal,2)
        endif
        write(fh,*) DimNum
        write(fh,*) "  "
        


        if(DimNum<=0)then
            !print *, "Caution :: no Initial Condition (Node-wise) Condition is loaded. "
        else
            n=size(obj%Boundary%TBoundVal,1)
            write(fh,*) n
            write(fh,*) "  "
            !allocate(obj%Boundary%TBoundNodID(n,DimNum) )
            !allocate(obj%Boundary%TBoundVal(  n,DimNum) )
            !allocate(obj%Boundary%TBoundNum(  DimNum) )

            !obj%Boundary%TBoundNum(:)=n
            
            if(n/=0)then
                if(n<0)then
                    print *, "ERROR :: number of initial conditions are to be zero"
                else
                    do i=1,n
                        write(fh,*) obj%Boundary%TBoundNodID(i,:)
                    enddo
                    write(fh,*) "  "
                    do i=1,n
                        write(fh,*) obj%Boundary%TBoundVal(i,:)
                    enddo
                    write(fh,*) "  "
                endif
            endif
        endif
        !######### Initial conditions #################

        print *, "Successfully Exported"
        print *, "########### Initial Condition info ###########"
        



        print *, "########### Initial Condition (Element-wize) info ###########"
        !######### Initial conditions #################
        ! For ElementGP-wize
        if(.not.allocated(obj%Boundary%TBoundElemGpVal) )then
            DimNum=0
        else
            DimNum=size(obj%Boundary%TBoundElemGpVal,3)
        endif
        
        write(fh,*) DimNum 
        write(fh,*) "  "
        if(DimNum<=0)then
            !print *, "Caution :: no Initial Condition (Gp) is loaded. "
        else
            !write(fh,*) 
            GpNum=size(obj%Boundary%TBoundElemGpVal,2)
            write(fh,*) GpNum
            write(fh,*) "  "
            !write(fh,*) 
            n=size(obj%Boundary%TBoundElemGpVal,1)
            write(fh,*) n
            write(fh,*) "  "
            !allocate(obj%Boundary%TBoundElemID(n) )
            !allocate(obj%Boundary%TBoundElemGpVal(n,GpNum,DimNum) )
            
            if(n/=0)then
                if(n<0)then
                    print *, "ERROR :: number of initial conditions are to be zero"
                else
                    do i=1,n
                        write(fh,*) obj%Boundary%TBoundElemID(i)
                    enddo
                    write(fh,*) "  "
                    do i=1,n
                        do j=1,GpNum
                            do k=1,DimNum
                                write(fh,*) obj%Boundary%TBoundElemGpVal(i,j,k)
                            enddo
                        enddo
                    enddo
                    write(fh,*) "  "
                endif
            endif

        endif
        !######### Initial conditions #################

        print *, "Successfully Exported"
        print *, "########### Initial Condition (Element-wize) info ###########"
        
        write(fh,*) obj%ControlPara%SimMode ,obj%ControlPara%ItrTol,obj%ControlPara%Timestep
        flush(fh)
        close(fh)
    else
        print *, "ERROR :: ExportFEMDomain >> only .scf file can be exported."
    endif





end subroutine ExportFEMDomain
!##################################################

!##################################################
subroutine InitDBC(obj,NumOfValPerNod)
    class(FEMDomain_),intent(inout)::obj
    integer,intent(in) :: NumOfValPerNod
    
    integer :: n,m
    !if the facet is not created, create facets (surface elements)
    call GetSurface(obj%Mesh)        
    n=size(obj%Mesh%FacetElemNod,1)
    m=size(obj%Mesh%FacetElemNod,2)

    if(allocated(obj%Boundary%DBoundNum))then
        deallocate(obj%Boundary%DBoundNum)
    endif

    if(allocated(obj%Boundary%DBoundNodID))then
        deallocate(obj%Boundary%DBoundNodID)
    endif

    if(allocated(obj%Boundary%DBoundVal) )then
        deallocate(obj%Boundary%DBoundVal)
    endif

    allocate(obj%Boundary%DBoundNum(NumOfValPerNod) )
    obj%Boundary%DBoundNum(:)=0
    allocate(obj%Boundary%DBoundNodID(n*m,NumOfValPerNod) )
    obj%Boundary%DBoundNodID(:,:)=-1
    allocate(obj%Boundary%DBoundVal(n*m,NumOfValPerNod) )
    obj%Boundary%DBoundVal(:,:)=0.0d0
            
end subroutine
!##################################################


!##################################################
subroutine AddDBoundCondition(obj,xmin,xmax,ymin,ymax,zmin,zmax,&
    tmin,tmax,valx,valy,valz,val,val_id,NumOfValPerNod,Mode2D )
    class(FEMDomain_),intent(inout)::obj
    real(8),optional,intent(in)::xmin,xmax
    real(8),optional,intent(in)::ymin,ymax
    real(8),optional,intent(in)::zmin,zmax
    real(8),optional,intent(in)::tmin,tmax
    real(8),optional,intent(in)::val
    integer,optional,intent(in)::val_id,NumOfValPerNod
    real(8)::x_min,x_max
    real(8)::y_min,y_max
    real(8)::z_min,z_max
    real(8)::t_min,t_max
    
    real(8),optional,intent(in)::valx,valy,valz


    logical,optional,intent(in) :: Mode2D
    logical :: InOut
    real(8) :: minline,maxline,SetDBCound(3)
    integer,allocatable::DBoundNodIDBuf(:,:),CopiedArrayInt(:,:)
    real(8),allocatable::DBoundValBuf(:,:),CopiedArrayReal(:,:),x(:),rmin(:),rmax(:)
    integer :: countnum,i,j,k,node_id,n,m,NumVN,newboundnum,ValID,count_n

    if(present(val_id) )then
        ValID=val_id
    else
        ValID=1
    endif

    if( present(NumOfValPerNod) )then
        NumVN=NumOfValPerNod
    else
        NumVN=3
    endif

    n=size(obj%Mesh%NodCoord,2)
    
    if( present(Mode2D) )then
        if(Mode2D .eqv. .true.)then
            allocate(x(2) )
            allocate(rmin(3) )
            allocate(rmax(3) )
        else
            allocate(x(3) )
            allocate(rmin(3) )
            allocate(rmax(3) )
        endif
    elseif(n==2)then
        allocate(x(2) )
        allocate(rmin(3) )
        allocate(rmax(3) )
    else
        allocate(x(3) )
        allocate(rmin(3) )
        allocate(rmax(3) )
    endif
    if(.not.present(xmin) ) then
        x_min = -1.0e+14
        
    else
        x_min=xmin
    endif
    if(.not.present(xmax) ) then
        x_max = 1.0e+14
    else
        x_max=xmax
    endif

    if(.not.present(ymin) ) then
        y_min = -1.0e+14
    else
        y_min=ymin
    endif
    if(.not.present(ymax) ) then
        y_max = 1.0e+14
    else
        y_max=ymax
    endif

    if(.not.present(zmin) ) then
        z_min = -1.0e+14
    else
        z_min = zmin
    
    endif
    if(.not.present(zmax) ) then
        z_max = 1.0e+14
    else
        z_max=zmax
    endif
    
    if(.not.present(tmin) ) then
        t_min = -1.0e+14
    else
        t_min = tmin
    endif
    if(.not.present(tmax) ) then
        t_max = 1.0e+14
    else
        t_max = tmax
    endif

	print *, "Range is : ",x_max,x_min,y_max,y_min,z_max,z_min,t_max,t_min
    
    ! get node ID and value
    
    !if the facet is not created, create facets (surface elements)
    
    
    if( .not. allocated(obj%Mesh%FacetElemNod))then
        call obj%InitDBC(NumOfValPerNod)
    endif
    rmin(1)=x_min
    rmin(2)=y_min
    rmin(3)=z_min
    
    rmax(1)=x_max
    rmax(2)=y_max
    rmax(3)=z_max
    n=size(obj%Mesh%FacetElemNod,1)
	m=size(obj%Mesh%FacetElemNod,2)
	count_n=0
    do i=1,size(obj%Mesh%FacetElemNod,1)
		do j=1,size(obj%Mesh%FacetElemNod,2)
			
            x(:)=obj%Mesh%NodCoord( obj%Mesh%FacetElemNod(i,j),: )    
			InOut = InOrOut(x,rmax,rmin)
            if(InOut .eqv. .true.)then
                if( (i-1)*m+j > n*m )then
                    stop "sgdssdfssssssssssssss"
				endif
				count_n=count_n+1
                obj%Boundary%DBoundNum(ValID)=obj%Boundary%DBoundNum(ValID)+1
                obj%Boundary%DBoundNodID( (i-1)*m+j ,ValID)=obj%Mesh%FacetElemNod(i,j)
                obj%Boundary%DBoundVal( (i-1)*m+j ,ValID)=val
            endif
            
        enddo
            
    enddo
		
	print *, "Total ",count_n,"boundary conditions are set"

end subroutine AddDBoundCondition
!##################################################



!##################################################
subroutine InitNBC(obj,NumOfValPerNod)
    class(FEMDomain_),intent(inout)::obj
    integer,intent(in) :: NumOfValPerNod
    
    integer :: n,m
    !if the facet is not created, create facets (surface elements)
    if( .not. allocated(obj%Mesh%FacetElemNod) )then
        call GetSurface(obj%Mesh)        
    endif
    n=size(obj%Mesh%FacetElemNod,1)
    m=size(obj%Mesh%FacetElemNod,2)


    if(allocated(obj%Boundary%NBoundNum))then
        deallocate(obj%Boundary%NBoundNum)
    endif

    if(allocated(obj%Boundary%NBoundNodID))then
        deallocate(obj%Boundary%NBoundNodID)
    endif

    if(allocated(obj%Boundary%NBoundVal) )then
        deallocate(obj%Boundary%NBoundVal)
    endif

    allocate(obj%Boundary%NBoundNum(NumOfValPerNod) )
    obj%Boundary%NBoundNum(:)=0
    allocate(obj%Boundary%NBoundNodID(n*m,NumOfValPerNod) )
    obj%Boundary%NBoundNodID(:,:)=-1
    allocate(obj%Boundary%NBoundVal(n*m,NumOfValPerNod) )
    obj%Boundary%NBoundVal(:,:)=0.0d0
    
    return
        
end subroutine
!##################################################





!##################################################
subroutine AddNBoundCondition(obj,xmin,xmax,ymin,ymax,zmin,zmax,&
    tmin,tmax,valx,valy,valz,val,val_id,NumOfValPerNod,Mode2D )
    class(FEMDomain_),intent(inout)::obj
    real(8),optional,intent(in)::xmin,xmax
    real(8),optional,intent(in)::ymin,ymax
    real(8),optional,intent(in)::zmin,zmax
    real(8),optional,intent(in)::tmin,tmax
    real(8),optional,intent(in)::val
    integer,optional,intent(in)::val_id,NumOfValPerNod
    real(8)::x_min,x_max
    real(8)::y_min,y_max
    real(8)::z_min,z_max
    real(8)::t_min,t_max
    
    real(8),optional,intent(in)::valx,valy,valz


    logical,optional,intent(in) :: Mode2D
    logical :: InOut
    real(8) :: minline,maxline,SetDBCound(3)
    integer,allocatable::NBoundNodINBuf(:,:),CopiedArrayInt(:,:)
    real(8),allocatable::NBoundValBuf(:,:),CopiedArrayReal(:,:),x(:),rmin(:),rmax(:)
    integer :: countnum,i,j,k,node_id,n,m,NumVN,newboundnum,ValID

    if(present(val_id) )then
        ValID=val_id
    else
        ValID=1
    endif

    if( present(NumOfValPerNod) )then
        NumVN=NumOfValPerNod
    else
        NumVN=3
    endif



    n=size(obj%Mesh%NodCoord,2)
    
    if( present(Mode2D) )then
        if(Mode2D .eqv. .true.)then
            allocate(x(2) )
            allocate(rmin(3) )
            allocate(rmax(3) )
        else
            allocate(x(3) )
            allocate(rmin(3) )
            allocate(rmax(3) )
        endif
    elseif(n==2)then
        allocate(x(2) )
        allocate(rmin(3) )
        allocate(rmax(3) )
    else
        allocate(x(3) )
        allocate(rmin(3) )
        allocate(rmax(3) )
    endif

    if(.not.present(xmin) ) then
        x_min = -1.0e+14
        
    else
        x_min=xmin
    endif
    if(.not.present(xmax) ) then
        x_max = 1.0e+14
    else
        x_max=xmax
    endif

    if(.not.present(ymin) ) then
        y_min = -1.0e+14
    else
        y_min=ymin
    endif
    if(.not.present(ymax) ) then
        y_max = 1.0e+14
    else
        y_max=ymax
    endif

    if(.not.present(zmin) ) then
        z_min = -1.0e+14
    else
        z_min = zmin
    
    endif
		
	if(.not.present(zmax) ) then
        z_max = 1.0e+14
    else
        z_max=zmax
    endif

	
    if(.not.present(tmin) ) then
        t_min = -1.0e+14
    else
        t_min = tmin
    endif
    if(.not.present(tmax) ) then
        t_max = 1.0e+14
    else
        t_max = tmax
    endif
    
    

    ! get node ID and value
    
    !if the facet is not created, create facets (surface elements)
    
    
    if( .not. allocated(obj%Mesh%FacetElemNod))then
        call obj%InitNBC(NumOfValPerNod)
        
    endif
    
    rmin(1)=x_min
    rmin(2)=y_min
    rmin(3)=z_min
    
    rmax(1)=x_max
    rmax(2)=y_max
    rmax(3)=z_max
    n=size(obj%Mesh%FacetElemNod,1)
    m=size(obj%Mesh%FacetElemNod,2)
        
    do i=1,size(obj%Mesh%FacetElemNod,1)
        do j=1,size(obj%Mesh%FacetElemNod,2)
            x(:)=obj%Mesh%NodCoord( obj%Mesh%FacetElemNod(i,j),: )
            InOut = InOrOut(x,rmax,rmin)
            if(InOut .eqv. .true.)then
                if( (i-1)*m+j > n*m )then
                    stop "sgdssdfssssssssssssss"
                endif
                obj%Boundary%NBoundNum(ValID)=obj%Boundary%NBoundNum(ValID)+1
                obj%Boundary%NBoundNodID( (i-1)*m+j ,ValID)=obj%Mesh%FacetElemNod(i,j)
                obj%Boundary%NBoundVal( (i-1)*m+j ,ValID)=val
            endif
        enddo
    enddo
    return

    

!
!    if(.not.present(valx) ) then
!        SetNBCound(1)=0.0d0
!    else
!        SetNBCound(1)=valx
!    endif
!    if(.not.present(valy) ) then
!        SetNBCound(2)=0.0d0
!    else
!        SetNBCound(2)=valy
!    endif    
!    if(.not.present(valz) ) then
!        SetNBCound(3)=0.0d0
!    else
!        SetNBCound(3)=valz
!    endif
!
!    
!    allocate(NBoundNodINBuf(size(obj%Mesh%SurfaceLine2D),size(obj%Boundary%NBoundNodID,2) ) )
!    allocate(NBoundValBuf  (size(obj%Mesh%SurfaceLine2D),size(obj%Boundary%NBoundNodID,2) ) )
!    
!    NBoundNodINBuf(:,:) = -1
!    NBoundValBuf(:,:)   = -1.0d0
!
!
!    k=0
!    do i=1,size(obj%Mesh%SurfaceLine2D,1)
!        countnum=0
!        node_id=obj%Mesh%SurfaceLine2D(i)
!        
!        do j=1,size(obj%Mesh%NodCoord,2)
!            if(j==1)then
!                minline=x_min
!                maxline=x_max
!            elseif(j==2)then
!                minline=y_min
!                maxline=y_max
!            elseif(j==3)then
!                minline=z_min
!                maxline=z_max
!            elseif(j==4)then
!                minline=t_min
!                maxline=t_max
!            else
!                !print *, "ERROR :: EditClass >> AddNBoundCondition >> dimension should 0 < d < 5"
!            endif
!            if(minline <= obj%Mesh%NodCoord(node_id,j) .and. obj%Mesh%NodCoord(node_id,j) <= maxline )then
!                countnum=countnum+1
!            endif 
!        enddo
!
!        if(countnum==size(obj%Mesh%NodCoord,2))then
!            k=k+1
!            do j=1,size(obj%Mesh%NodCoord,2)
!                if(j==1)then
!                    if(.not.present(valx) ) then
!                        NBoundNodINBuf(k,1)=-1
!                    else
!                        NBoundNodINBuf(k,1)=node_id
!                        NBoundValBuf(k,1)=valx
!                    endif
!                elseif(j==2)then
!                    if(.not.present(valy) ) then
!                        NBoundNodINBuf(k,2)=-1
!                    else
!                        NBoundNodINBuf(k,2)=node_id
!                        NBoundValBuf(k,2)=valy
!                    endif    
!                elseif(j==3)then
!                    if(.not.present(valz) ) then
!                        NBoundNodINBuf(k,3)=-1
!                    else
!                        NBoundNodINBuf(k,3)=node_id
!                        NBoundValBuf(k,3)=valz
!                    endif
!                else
!                    stop "EditClass >Time domain is not implemented "
!                endif
!            enddo   
!        endif
!    enddo
!
!
!    
!    ! MergeArray
! 
!    call TrimArray(DBoundNodIDBuf,k)
!    call TrimArray(DBoundValBuf,k)
!    call CopyArray(obj%Boundary%DBoundNodID,CopiedArrayInt)
!    call CopyArray(obj%Boundary%DBoundVal,CopiedArrayReal)
!    call MergeArray(CopiedArrayInt,DBoundNodIDBuf,obj%Boundary%DBoundNodID)
!    call MergeArray(CopiedArrayReal,DBoundValBuf,obj%Boundary%DBoundVal)
!
!    call DeleteOverlapBoundary(obj%Boundary)

end subroutine AddNBoundCondition
!##################################################

!##################################################
subroutine InitTBC(obj,NumOfValPerNod)
    class(FEMDomain_),intent(inout)::obj
    integer,intent(in) :: NumOfValPerNod
    
    integer :: n,m
    !if the facet is not created, create facets (surface elements)
    if( .not. allocated(obj%Mesh%FacetElemNod) )then
        call GetSurface(obj%Mesh)        
    endif
    n=size(obj%Mesh%NodCoord,1)
    m=size(obj%Mesh%NodCoord,2)

    if(allocated(obj%Boundary%TBoundNum))then
        deallocate(obj%Boundary%TBoundNum)
    endif

    if(allocated(obj%Boundary%TBoundNodID))then
        deallocate(obj%Boundary%TBoundNodID)
    endif

    if(allocated(obj%Boundary%TBoundVal) )then
        deallocate(obj%Boundary%TBoundVal)
    endif

    allocate(obj%Boundary%TBoundNum(NumOfValPerNod) )
    obj%Boundary%TBoundNum(:)=0
    allocate(obj%Boundary%TBoundNodID(n,NumOfValPerNod) )
    obj%Boundary%TBoundNodID(:,:)=-1
    allocate(obj%Boundary%TBoundVal(n,NumOfValPerNod) )
    obj%Boundary%TBoundVal(:,:)=0.0d0
    
    return
        
end subroutine
!##################################################





!##################################################
subroutine AddTBoundCondition(obj,xmin,xmax,ymin,ymax,zmin,zmax,&
    tmin,tmax,valx,valy,valz,val,val_id,NumOfValPerNod,Mode2D )
    class(FEMDomain_),intent(inout)::obj
    real(8),optional,intent(in)::xmin,xmax
    real(8),optional,intent(in)::ymin,ymax
    real(8),optional,intent(in)::zmin,zmax
    real(8),optional,intent(in)::tmin,tmax
    real(8),optional,intent(in)::val
    integer,optional,intent(in)::val_id,NumOfValPerNod
    real(8)::x_min,x_max
    real(8)::y_min,y_max
    real(8)::z_min,z_max
    real(8)::t_min,t_max
    
    real(8),optional,intent(in)::valx,valy,valz


    logical,optional,intent(in) :: Mode2D
    logical :: InOut
    real(8) :: minline,maxline,SetDBCound(3)
    integer,allocatable::TBoundNodITBuf(:,:),CopiedArrayInt(:,:)
    real(8),allocatable::TBoundValBuf(:,:),CopiedArrayReal(:,:),x(:),rmin(:),rmax(:)
    integer :: countnum,i,j,k,node_id,n,m,NumVN,newboundnum,ValID,count_n

    if(present(val_id) )then
        ValID=val_id
    else
        ValID=1
    endif

    if( present(NumOfValPerNod) )then
        NumVN=NumOfValPerNod
    else
        NumVN=3
    endif

    n=size(obj%Mesh%NodCoord,2)
    
    if( present(Mode2D) )then
        if(Mode2D .eqv. .true.)then
            allocate(x(2) )
            allocate(rmin(3) )
            allocate(rmax(3) )
        else
            allocate(x(3) )
            allocate(rmin(3) )
            allocate(rmax(3) )
        endif
    elseif(n==2)then
        allocate(x(2) )
        allocate(rmin(3) )
        allocate(rmax(3) )
    else
        allocate(x(3) )
        allocate(rmin(3) )
        allocate(rmax(3) )
    endif

    if(.not.present(xmin) ) then
        x_min = -1.0e+14
        
    else
        x_min=xmin
    endif
    if(.not.present(xmax) ) then
        x_max = 1.0e+14
    else
        x_max=xmax
    endif

    if(.not.present(ymin) ) then
        y_min = -1.0e+14
    else
        y_min=ymin
    endif
    if(.not.present(ymax) ) then
        y_max = 1.0e+14
    else
        y_max=ymax
    endif

    if(.not.present(zmin) ) then
        z_min = -1.0e+14
    else
        z_min = zmin
    
    endif
    if(.not.present(zmax) ) then
        z_max = 1.0e+14
    else
        z_max=zmax
    endif
    
    if(.not.present(tmin) ) then
        t_min = -1.0e+14
    else
        t_min = tmin
    endif
    if(.not.present(tmax) ) then
        t_max = 1.0e+14
    else
        t_max = tmax
    endif
    
    

    ! get node ID and value
    
    !if the facet is not created, create facets (surface elements)
    
    
    if( size(obj%Mesh%NodCoord,1)/=size(obj%Boundary%TBoundNodID,1)  )then
        call obj%InitTBC(NumOfValPerNod)
        print *, "sifdh"
    endif
    
    rmin(1)=x_min
    rmin(2)=y_min
    rmin(3)=z_min
    
    rmax(1)=x_max
    rmax(2)=y_max
    rmax(3)=z_max
    n=size(obj%Mesh%NodCoord,1)

	count_n=0
    do i=1,n
            x(:)=obj%Mesh%NodCoord( i,: )
            InOut = InOrOut(x,rmax,rmin)
			if(InOut .eqv. .true.)then
				count_n=count_n+1
                obj%Boundary%TBoundNum(ValID)=obj%Boundary%TBoundNum(ValID)+1
                obj%Boundary%TBoundNodID( i ,ValID)=i
                obj%Boundary%TBoundVal( i,ValID)=val
            endif
        
	enddo
	print *, "Initial value is in : ",count_n,"value is : ",val
    return

end subroutine AddTBoundCondition
!##################################################



!!##################################################
!subroutine AddNBoundCondition(obj,xmin,xmax,ymin,ymax,zmin,zmax,&
!    tmin,tmax,valx,valy,valz)
!    class(FEMDomain_),intent(inout)::obj
!    real(8),optional,intent(in)::xmin,xmax
!    real(8),optional,intent(in)::ymin,ymax
!    real(8),optional,intent(in)::zmin,zmax
!    real(8),optional,intent(in)::tmin,tmax
!    real(8)::x_min,x_max
!    real(8)::y_min,y_max
!    real(8)::z_min,z_max
!    real(8)::t_min,t_max
!    
!    real(8),optional,intent(in)::valx,valy,valz
!
!    real(8) :: minline,maxline,SetNBCound(3)
!    integer,allocatable::NBoundNodIDBuf(:,:),CopiedArrayInt(:,:)
!    real(8),allocatable::NBoundValBuf(:,:),CopiedArrayReal(:,:)
!    integer :: countnum,i,j,k,node_id
!
!    
!
!
!
!
!    if(.not.present(xmin) ) then
!        x_min = -1.0e+14
!    else
!        x_min=xmin
!    endif
!    if(.not.present(xmax) ) then
!        x_max = 1.0e+14
!    else
!        x_max=xmax
!    endif
!
!    if(.not.present(ymin) ) then
!        y_min = -1.0e+14
!    else
!        y_min=ymin
!    endif
!    if(.not.present(ymax) ) then
!        y_max = 1.0e+14
!    else
!        y_max=ymax
!    endif
!
!    if(.not.present(zmin) ) then
!        z_min = -1.0e+14
!    else
!        z_min = zmin
!    
!    endif
!    if(.not.present(zmax) ) then
!        z_max = 1.0e+14
!    else
!        z_max=zmin
!    endif
!    
!    if(.not.present(tmin) ) then
!        t_min = -1.0e+14
!    else
!        t_min = tmin
!    endif
!    if(.not.present(tmax) ) then
!        t_max = 1.0e+14
!    else
!        t_max = tmax
!    endif
!    
!    if(.not.present(valx) ) then
!        SetNBCound(1)=0.0d0
!    else
!        SetNBCound(1)=valx
!    endif
!    if(.not.present(valy) ) then
!        SetNBCound(2)=0.0d0
!    else
!        SetNBCound(2)=valy
!    endif    
!    if(.not.present(valz) ) then
!        SetNBCound(3)=0.0d0
!    else
!        SetNBCound(3)=valz
!    endif
!
!
!    ! get node ID and value
!    allocate(NBoundNodIDBuf(size(obj%Mesh%SurfaceLine2D),size(obj%Boundary%NBoundNodID,2) ) )
!    allocate(NBoundValBuf  (size(obj%Mesh%SurfaceLine2D),size(obj%Boundary%NBoundNodID,2) ) )
!    NBoundNodIDBuf(:,:) = -1
!    NBoundValBuf(:,:)   = -1.0d0
!
!
!
!    k=0
!    do i=1,size(obj%Mesh%SurfaceLine2D,1)
!        countnum=0
!        node_id=obj%Mesh%SurfaceLine2D(i)
!        
!        do j=1,size(obj%Mesh%NodCoord,2)
!            if(j==1)then
!                minline=x_min
!                maxline=x_max
!            elseif(j==2)then
!                minline=y_min
!                maxline=y_max
!            elseif(j==3)then
!                minline=z_min
!                maxline=z_max
!            elseif(j==4)then
!                minline=t_min
!                maxline=t_max
!            else
!                !print *, "ERROR :: EditClass >> AddNBoundCondition >> dimension should 0 < d < 5"
!            endif
!            if(minline <= obj%Mesh%NodCoord(node_id,j) .and. obj%Mesh%NodCoord(node_id,j) <= maxline )then
!                countnum=countnum+1
!            endif 
!        enddo
!
!        if(countnum==size(obj%Mesh%NodCoord,2))then
!            k=k+1
!            do j=1,size(obj%Mesh%NodCoord,2)
!                if(j==1)then
!                    if(.not.present(valx) ) then
!                        NBoundNodIDBuf(k,1)=-1
!                    else
!                        NBoundNodIDBuf(k,1)=node_id
!                        NBoundValBuf(k,1)=valx
!                    endif
!                elseif(j==2)then
!                    if(.not.present(valy) ) then
!                        NBoundNodIDBuf(k,2)=-1
!                    else
!                        NBoundNodIDBuf(k,2)=node_id
!                        NBoundValBuf(k,2)=valy
!                    endif    
!                elseif(j==3)then
!                    if(.not.present(valz) ) then
!                        NBoundNodIDBuf(k,3)=-1
!                    else
!                        NBoundNodIDBuf(k,3)=node_id
!                        NBoundValBuf(k,3)=valz
!                    endif
!                else
!                    stop "EditClass >Time domain is not implemented "
!                endif
!            enddo   
!        endif
!    enddo
!
!    
!
!    ! MergeArray
! 
!    call TrimArray(NBoundNodIDBuf,k)
!    call TrimArray(NBoundValBuf,k)
!    call CopyArray(obj%Boundary%NBoundNodID,CopiedArrayInt)
!    call CopyArray(obj%Boundary%NBoundVal,CopiedArrayReal)
!!    call MergeArray(CopiedArrayInt,NBoundNodIDBuf,obj%Boundary%NBoundNodID)
!!    call MergeArray(CopiedArrayReal,NBoundValBuf,obj%Boundary%NBoundVal)
!!    call DeleteOverlapBoundary(obj%Boundary)
!!    call InitializeBoundary(obj%Boundary)
!!    
!!
!!
!!end subroutine 
!!##################################################
!
!
!
!
!
!
!
!
!!##################################################
!subroutine AddTBoundCondition(obj,xmin,xmax,ymin,ymax,zmin,zmax,&
!    tmin,tmax,valx,valy,valz)
!    class(FEMDomain_),intent(inout)::obj
!    real(8),optional,intent(in)::xmin,xmax
!    real(8),optional,intent(in)::ymin,ymax
!    real(8),optional,intent(in)::zmin,zmax
!    real(8),optional,intent(in)::tmin,tmax
!    real(8)::x_min,x_max
!    real(8)::y_min,y_max
!    real(8)::z_min,z_max
!    real(8)::t_min,t_max
!    
!    real(8),optional,intent(in)::valx,valy,valz
!
!    real(8) :: minline,maxline,SetTBCound(3)
!    integer,allocatable::TBoundNodIDBuf(:,:),CopiedArrayInt(:,:)
!    real(8),allocatable::TBoundValBuf(:,:),CopiedArrayReal(:,:)
!    integer :: countnum,i,j,k,node_id
!
!    
!
!
!
!
!    if(.not.present(xmin) ) then
!        x_min = -1.0e+14
!    else
!        x_min=xmin
!    endif
!    if(.not.present(xmax) ) then
!        x_max = 1.0e+14
!    else
!        x_max=xmax
!    endif
!
!    if(.not.present(ymin) ) then
!        y_min = -1.0e+14
!    else
!        y_min=ymin
!    endif
!    if(.not.present(ymax) ) then
!        y_max = 1.0e+14
!    else
!        y_max=ymax
!    endif
!
!    if(.not.present(zmin) ) then
!        z_min = -1.0e+14
!    else
!        z_min = zmin
!    
!    endif
!    if(.not.present(zmax) ) then
!        z_max = 1.0e+14
!    else
!        z_max=zmin
!    endif
!    
!    if(.not.present(tmin) ) then
!        t_min = -1.0e+14
!    else
!        t_min = tmin
!    endif
!    if(.not.present(tmax) ) then
!        t_max = 1.0e+14
!    else
!        t_max = tmax
!    endif
!    
!    if(.not.present(valx) ) then
!        SetTBCound(1)=0.0d0
!    else
!        SetTBCound(1)=valx
!    endif
!    if(.not.present(valy) ) then
!        SetTBCound(2)=0.0d0
!    else
!        SetTBCound(2)=valy
!    endif    
!    if(.not.present(valz) ) then
!        SetTBCound(3)=0.0d0
!    else
!        SetTBCound(3)=valz
!    endif
!
!
!    ! get node ID and value
!    allocate(TBoundNodIDBuf(size(obj%Mesh%SurfaceLine2D),size(obj%Boundary%TBoundNodID,2) ) )
!    allocate(TBoundValBuf  (size(obj%Mesh%SurfaceLine2D),size(obj%Boundary%TBoundNodID,2) ) )
!    TBoundNodIDBuf(:,:) = -1
!    TBoundValBuf(:,:)   = -1.0d0
!
!
!
!    k=0
!    do i=1,size(obj%Mesh%SurfaceLine2D,1)
!        countnum=0
!        node_id=obj%Mesh%SurfaceLine2D(i)
!        
!        do j=1,size(obj%Mesh%NodCoord,2)
!            if(j==1)then
!                minline=x_min
!                maxline=x_max
!            elseif(j==2)then
!                minline=y_min
!                maxline=y_max
!            elseif(j==3)then
!                minline=z_min
!                maxline=z_max
!            elseif(j==4)then
!                minline=t_min
!                maxline=t_max
!            else
!                !print *, "ERROR :: EditClass >> AddTBoundCondition >> dimension should 0 < d < 5"
!            endif
!            if(minline <= obj%Mesh%NodCoord(node_id,j) .and. obj%Mesh%NodCoord(node_id,j) <= maxline )then
!                countnum=countnum+1
!            endif 
!        enddo
!
!        if(countnum==size(obj%Mesh%NodCoord,2))then
!            k=k+1
!            do j=1,size(obj%Mesh%NodCoord,2)
!                if(j==1)then
!                    if(.not.present(valx) ) then
!                        TBoundNodIDBuf(k,1)=-1
!                    else
!                        TBoundNodIDBuf(k,1)=node_id
!                        TBoundValBuf(k,1)=valx
!                    endif
!                elseif(j==2)then
!                    if(.not.present(valy) ) then
!                        TBoundNodIDBuf(k,2)=-1
!                    else
!                        TBoundNodIDBuf(k,2)=node_id
!                        TBoundValBuf(k,2)=valy
!                    endif    
!                elseif(j==3)then
!                    if(.not.present(valz) ) then
!                        TBoundNodIDBuf(k,3)=-1
!                    else
!                        TBoundNodIDBuf(k,3)=node_id
!                        TBoundValBuf(k,3)=valz
!                    endif
!                else
!                    stop "EditClass >Time domain is not implemented "
!                endif
!            enddo   
!        endif
!    enddo
!
!    
!    ! MergeArray
! 
!    call TrimArray(TBoundNodIDBuf,k)
!    call TrimArray(TBoundValBuf,k)
!    call CopyArray(obj%Boundary%TBoundNodID,CopiedArrayInt)
!    call CopyArray(obj%Boundary%TBoundVal,CopiedArrayReal)
!    call MergeArray(CopiedArrayInt,TBoundNodIDBuf,obj%Boundary%TBoundNodID)
!    call MergeArray(CopiedArrayReal,TBoundValBuf,obj%Boundary%TBoundVal)
!    call DeleteOverlapBoundary(obj%Boundary)
!    call InitializeBoundary(obj%Boundary)
!    
!
!
!end subroutine 
!!##################################################




!##################################################
subroutine SetSolver(obj,inSolverType)
    class(FEMDomain_),intent(inout)::obj
    character*200,intent(in) :: inSolverType

    obj%SolverType=inSolverType

end subroutine
!##################################################

!##################################################
subroutine SetName(obj,Name)
    class(FEMDomain_),intent(inout)::obj
    character(*),intent(in) :: Name

    obj%FileName=Name

end subroutine
!##################################################


!##################################################
subroutine SetDataType(obj,inDType)
    class(FEMDomain_),intent(inout)::obj
    character*200,intent(in) :: inDType

    obj%DType = inDType

end subroutine

!##################################################


!##################################################
subroutine SetUpFEMDomain(obj)
    class(FEMDomain_),intent(inout)::obj

    logical :: NodeExist
    logical :: ElementExist

    if(allocated(obj%Mesh%NodCoord)  )then
        NodeExist = .true.
    else
        NodeExist = .false.
    endif

    if(allocated(obj%Mesh%ElemNod)  )then
        ElementExist = .true.
    else
        ElementExist = .false.
    endif

    if( NodeExist .eqv. .false. )then
        print *, "ERROR :: SetUp FEMDomain_ >> No Nodes are imported"
        return
    endif
    if( ElementExist .eqv. .false. )then
        print *, "ERROR :: SetUp FEMDomain_ >> No Elements are imported"
        return
    endif





end subroutine
!##################################################


!##################################################
subroutine SetControlParaFEMDomain(obj,OptionalTol,OptionalItrTol,OptionalTimestep,OptionalSimMode)
    class(FEMDomain_),intent(inout)::obj
    real(8),optional,intent(in)::OptionalTol
    integer,optional,intent(in)::OptionalSimMode,OptionalItrTol,OptionalTimestep
    
    call SetControlPara(obj%ControlPara,OptionalTol,OptionalItrTol,OptionalTimestep,OptionalSimMode)
end subroutine
!##################################################


!##################################################
subroutine AddMaterialID(obj,xmin,xmax,ymin,ymax,zmin,zmax,&
    tmin,tmax,valx,valy,valz,MaterialID ,mode2D)
    class(FEMDomain_),intent(inout)::obj
    real(8),optional,intent(in)::xmin,xmax
    real(8),optional,intent(in)::ymin,ymax
    real(8),optional,intent(in)::zmin,zmax
    real(8),optional,intent(in)::tmin,tmax
    
    integer,optional,intent(in)::MaterialID
    real(8)::x_min,x_max
    real(8)::y_min,y_max
    real(8)::z_min,z_max
    real(8)::t_min,t_max
    
    real(8),optional,intent(in)::valx,valy,valz


    logical,optional,intent(in) :: Mode2D
    logical :: InOut
    real(8) :: minline,maxline,SetDBCound(3)
    integer,allocatable::TBoundNodITBuf(:,:),CopiedArrayInt(:,:)
    real(8),allocatable::TBoundValBuf(:,:),CopiedArrayReal(:,:),x(:),rmin(:),rmax(:)
    integer :: countnum,i,j,k,node_id,n,m,NumVN,newboundnum,ValID,md

    if(present(MaterialID) )then
        md=MaterialID
    else
        md=1
    endif


    n=size(obj%Mesh%NodCoord,2)
    
    if( present(Mode2D) )then
        if(Mode2D .eqv. .true.)then
            allocate(x(2) )
            allocate(rmin(3) )
            allocate(rmax(3) )
        else
            allocate(x(3) )
            allocate(rmin(3) )
            allocate(rmax(3) )
        endif
    elseif(n==2)then
        allocate(x(2) )
        allocate(rmin(3) )
        allocate(rmax(3) )
    else
        allocate(x(3) )
        allocate(rmin(3) )
        allocate(rmax(3) )
    endif

    if(.not.present(xmin) ) then
        x_min = -1.0e+14
        
    else
        x_min=xmin
    endif
    if(.not.present(xmax) ) then
        x_max = 1.0e+14
    else
        x_max=xmax
    endif

    if(.not.present(ymin) ) then
        y_min = -1.0e+14
    else
        y_min=ymin
    endif
    if(.not.present(ymax) ) then
        y_max = 1.0e+14
    else
        y_max=ymax
    endif

    if(.not.present(zmin) ) then
        z_min = -1.0e+14
    else
        z_min = zmin
    
    endif
    if(.not.present(zmax) ) then
        z_max = 1.0e+14
    else
        z_max=zmax
    endif
    
    if(.not.present(tmin) ) then
        t_min = -1.0e+14
    else
        t_min = tmin
    endif
    if(.not.present(tmax) ) then
        t_max = 1.0e+14
    else
        t_max = tmax
    endif
    
    

    ! get node ID and value
    
    !if the facet is not created, create facets (surface elements)
    
    
    rmin(1)=x_min
    rmin(2)=y_min
    rmin(3)=z_min
    
    rmax(1)=x_max
    rmax(2)=y_max
    rmax(3)=z_max
    n=size(obj%Mesh%ElemMat,1)

    do i=1,n
        x(:)=0.0d0
        do j=1,size(obj%Mesh%ElemNod,2)
            x(:)=x(:)+obj%Mesh%NodCoord( obj%Mesh%ElemNod(i,j),: )
        enddo

        x(:)=1.0d0/dble(size(obj%Mesh%ElemNod,2))*x(:)
        
        InOut = InOrOut(x,rmax,rmin)
        if(InOut .eqv. .true.)then
            
            obj%Mesh%ElemMat(i)=md
        endif
        
    enddo


end subroutine
!##################################################


!##################################################
subroutine MeltingSkeltonFEMDomain(obj)
    class(FEMDomain_),intent(inout)::obj

    call obj%Mesh%MeltingSkelton()
    
end subroutine
!##################################################



! #########################################################################################
subroutine GmshPlotMesh(obj,OptionalContorName,OptionalAbb,OptionalStep,Name,withNeumannBC,withDirichletBC&
	,onlyNeumannBC,onlyDirichletBC,asMsh)
	class(FEMDomain_),intent(inout)::obj
	real(8),allocatable::gp_value(:,:)
	integer,optional,intent(in)::OptionalStep
	character,optional,intent(in):: OptionalContorName*30,OptionalAbb*6
	character(*),optional,intent(in)::Name
	logical,optional,intent(in)::withNeumannBC,withDirichletBC,onlyNeumannBC,onlyDirichletBC,asMsh
	real(8),allocatable::x_double(:,:)
	real(8),allocatable::x(:,:)
	integer i,j,k,l,step,fh,nodeid1,nodeid2
	character filename0*11
	character filename*200
	character filetitle*6
	character command*200
	character:: mapname*30,abbmap*6
	


	if(present(OptionalContorName) )then
		mapname=OptionalContorName
	else
		mapname="Value"
	endif

	if(present(OptionalAbb) )then
		abbmap=OptionalAbb
	else
		abbmap="Values"
	endif

	if(present(OptionalStep) )then
		step=OptionalStep
	else
		step=1
	endif
	fh=123

	filetitle(1:6)=abbmap(1:6)
    
    if(.not.allocated(obj%Mesh%ElemMat) )then
        allocate(obj%Mesh%ElemMat(size(obj%Mesh%ElemNod,1) ) )
        obj%Mesh%ElemMat(:)=1
    endif

	!---------------------
	write (filename0, '("_", i6.6, ".pos")') step ! ここでファイル名を生成している
	if(present(Name) )then
		filename=filetitle//filename0
		
		call system(  "touch "//trim(Name)//trim(obj%FileName)//trim(filename) )
		print *, trim(Name)//trim(filename)
		open(fh,file=trim(Name)//trim(filename) )
		print *, "writing ",trim(Name)//trim(filename)," step>>",step
	else
		filename=filetitle//filename0
		call system(  "touch "//trim(obj%FileName)//trim(filename) )
		print *, trim(obj%FileName)//trim(filename)
		open(fh,file=trim(obj%FileName)//trim(filename) )
		print *, "writing ",trim(obj%FileName)//trim(filename)," step>>",step
	endif
	
	
	!---------------------
	if( size(obj%Mesh%ElemNod,2)==4 .and. size(obj%Mesh%NodCoord,2)==2 ) then
		allocate(x(4,3) )
		allocate(x_double(4,3) )
		x(:,:)=0.0d0
		x_double(:,:)=0.0d0
	elseif( size(obj%Mesh%ElemNod,2)==8 .and. size(obj%Mesh%NodCoord,2)==3 ) then
		allocate(x(8,3) )
		allocate(x_double(8,3) )
		x(:,:)=0.0d0
		x_double(:,:)=0.0d0
		
	endif

	allocate(gp_value( size(obj%Mesh%ElemNod,1),size(obj%Mesh%ElemNod,2) ))
	do i=1,size(obj%Mesh%ElemNod,1)
		gp_value(i,:)=dble(obj%Mesh%ElemMat(i))
	enddo

	if(present(withDirichletBC) )then
		if(withDirichletBC .eqv. .true. )then
			! search Dirichlet BC and change color
			if(.not. allocated(obj%Boundary%DBoundNodID) )then
				print *, "ERROR GmshPlotMesh >> withDirichletBC >> no NBC is found."
				return
			else
				print *, "[ok] GmshPlotMesh",trim(filename)," is exported withDirichletBC. The value is:",maxval(obj%Mesh%ElemMat(:))+40
				
			endif
			do i=1,size(obj%Boundary%DBoundNodID,1 )
				do j=1,size(obj%Boundary%DBoundNodID,2)
					
					if(obj%Boundary%DBoundNodID(i,j)>0 )then
						nodeid1=obj%Boundary%DBoundNodID(i,j)
					else
						cycle
					endif

					do k=1,size(obj%Mesh%ElemNod,1)
						do l=1,size(obj%Mesh%ElemNod,2)
							nodeid2=obj%Mesh%ElemNod( k,l  )
							if(nodeid1==nodeid2 )then
								gp_value(k,:)=dble(maxval(obj%Mesh%ElemMat(:)))+40.0d0 ! Dirichlet is +20
							endif
						enddo
					enddo
				enddo
			enddo

		endif
	endif

	if(present(withNeumannBC) )then
		if(withNeumannBC .eqv. .true. )then
			! search Neumann BC and change color
			if(.not. allocated(obj%Boundary%NBoundNodID) )then
				print *, "ERROR GmshPlotMesh >> withNeumannBC >> no NBC is found."
				return
			else
				print *, "[ok] GmshPlotMesh",trim(filename)," is exported withNeumannBC. The value is:",maxval(obj%Mesh%ElemMat(:))+20
				
			endif
			do i=1,size(obj%Boundary%NBoundNodID,1 )
				do j=1,size(obj%Boundary%NBoundNodID,2)
					
					if(obj%Boundary%NBoundNodID(i,j)>0 .and. obj%Boundary%NBoundVal(i,j)/=0.0d0)then
						nodeid1=obj%Boundary%NBoundNodID(i,j)
					else
						cycle
					endif

					do k=1,size(obj%Mesh%ElemNod,1)
						do l=1,size(obj%Mesh%ElemNod,2)
							nodeid2=obj%Mesh%ElemNod( k,l  )
							if(nodeid1==nodeid2 )then
								gp_value(k,:)=dble(maxval(obj%Mesh%ElemMat(:)))+20.0d0 ! neumann is +20
							endif
						enddo
					enddo
				enddo
			enddo

		endif
	endif


	gp_value(:,:)=-1.0d0
	if(present(onlyDirichletBC) )then
		if(onlyDirichletBC .eqv. .true. )then
			! search Dirichlet BC and change color
			if(.not. allocated(obj%Boundary%DBoundNodID) )then
				print *, "ERROR GmshPlotMesh >> onlyDirichletBC >> no NBC is found."
				return
			else
				print *, "[ok] GmshPlotMesh",trim(filename)," is exported onlyDirichletBC. The value is:",maxval(obj%Mesh%ElemMat(:))+40
				
			endif
			do i=1,size(obj%Boundary%DBoundNodID,1 )
				do j=1,size(obj%Boundary%DBoundNodID,2)
					
					if(obj%Boundary%DBoundNodID(i,j)>0 )then
						nodeid1=obj%Boundary%DBoundNodID(i,j)
					else
						cycle
					endif

					
					do k=1,size(obj%Mesh%ElemNod,1)
						do l=1,size(obj%Mesh%ElemNod,2)
							nodeid2=obj%Mesh%ElemNod( k,l  )
							if(nodeid1==nodeid2 )then
								if(l>size(gp_value,2) )then
									exit
								endif
								gp_value(k,l)=obj%Boundary%DBoundVal(i,j)
							endif
						enddo
					enddo
				enddo
			enddo

		endif
	endif

	x(:,:)=0.0d0
	write(fh,*) 'View "',mapname,'" {'
	do i=1,size(gp_value,1)
		if( size(obj%Mesh%ElemNod,2)==4 .and. size(obj%Mesh%NodCoord,2)==2 ) then
			
			! 2-D, 4 noded, isoparametric elements with four gauss points 
			x_double(1,1:2)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  )
			x_double(2,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  )
			x_double(3,1:2)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  )&
					+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  )
			x_double(4,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  )

			
			x(:,:)=x_double(:,:) 

			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),"){",gp_value(i,1),",",&
				gp_value(i,1),",",gp_value(i,1),",",gp_value(i,1),"};"
				

			x_double(1,1:2)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  )
			x_double(2,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  )
			x_double(3,1:2)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  )&
					+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  )
			x_double(4,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  )

			
			x(:,:)=x_double(:,:) 

			
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),"){",gp_value(i,2),",",&
				gp_value(i,2),",",gp_value(i,2),",",gp_value(i,2),"};"
				
			x_double(1,1:2)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  )
			x_double(2,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  )
			x_double(3,1:2)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  )&
					+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  )
			x_double(4,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  )
			
			x(:,:)=x_double(:,:) 

			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),"){",gp_value(i,3),",",&
				gp_value(i,3),",",gp_value(i,3),",",gp_value(i,3),"};"
				
			x_double(1,1:2)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  )
			x_double(2,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  )
			x_double(3,1:2)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  )&
					+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  )
			x_double(4,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  )
			
			x(:,:)=x_double(:,:) 
			
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),"){",gp_value(i,4),",",&
				gp_value(i,4),",",gp_value(i,4),",",gp_value(i,4),"};"
			
		elseif(size(obj%Mesh%ElemNod,2)==8 .and. size(obj%Mesh%NodCoord,2)==3  ) then
			
			
			! 3-D, 8 noded, isoparametric elements with 8 gauss points
			! 1/8

			x_double(1,1:3)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )
			x_double(2,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1), 1:3  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )
			x_double(3,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2), 1:3  )&
					+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4), 1:3  )
			x_double(4,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4), 1:3  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )

			x_double(5,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )

			x_double(6,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
					+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )
			
			x_double(7,1:3)=0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
					+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
					+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
					+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )

			x_double(8,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
					+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )
			
			x(:,:)=x_double(:,:) 



			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),"){",gp_value(i,1),",",&
				gp_value(i,1),",",gp_value(i,1),",",gp_value(i,1),"};"
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),"){",gp_value(i,1),",",&
				gp_value(i,1),",",gp_value(i,1),",",gp_value(i,1),"};"
			write(fh,*)" SQ(",x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),"){",gp_value(i,1),",",&
				gp_value(i,1),",",gp_value(i,1),",",gp_value(i,1),"};"
			write(fh,*)" SQ(",x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),"){",gp_value(i,1),",",&
				gp_value(i,1),",",gp_value(i,1),",",gp_value(i,1),"};"
			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,1),",",&
				gp_value(i,1),",",gp_value(i,1),",",gp_value(i,1),"};"
			write(fh,*)" SQ(",x(5,1),",",x(5,2),",",x(5,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,1),",",&
				gp_value(i,1),",",gp_value(i,1),",",gp_value(i,1),"};"
			
			! 2/8

			x_double(1,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1), 1:3  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )
			
			x_double(2,1:3)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )
			
			x_double(3,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )
			

			x_double(4,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )

			x_double(5,1:3)= 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )

			x_double(6,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )

			
			x_double(7,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )
			
			x_double(8,1:3)=0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )


			x(:,:)=x_double(:,:) 


			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),"){",gp_value(i,2),",",&
				gp_value(i,2),",",gp_value(i,2),",",gp_value(i,2),"};"
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),"){",gp_value(i,2),",",&
				gp_value(i,2),",",gp_value(i,2),",",gp_value(i,2),"};"
			write(fh,*)" SQ(",x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),"){",gp_value(i,2),",",&
				gp_value(i,2),",",gp_value(i,2),",",gp_value(i,2),"};"
			write(fh,*)" SQ(",x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),"){",gp_value(i,2),",",&
				gp_value(i,2),",",gp_value(i,2),",",gp_value(i,2),"};"
			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,2),",",&
				gp_value(i,2),",",gp_value(i,2),",",gp_value(i,2),"};"
			write(fh,*)" SQ(",x(5,1),",",x(5,2),",",x(5,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,2),",",&
				gp_value(i,2),",",gp_value(i,2),",",gp_value(i,2),"};"
			
			
			! 3/8

			x_double(8,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )

			x_double(3,1:3)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )
			
			x_double(2,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )
			

			x_double(1,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )

			x_double(6,1:3)= 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )

			x_double(7,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )

			
			x_double(4,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )

			x_double(5,1:3)=0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )


			x(:,:)=x_double(:,:) 


			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),"){",gp_value(i,3),",",&
				gp_value(i,3),",",gp_value(i,3),",",gp_value(i,3),"};"
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),"){",gp_value(i,3),",",&
				gp_value(i,3),",",gp_value(i,3),",",gp_value(i,3),"};"
			write(fh,*)" SQ(",x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),"){",gp_value(i,3),",",&
				gp_value(i,3),",",gp_value(i,3),",",gp_value(i,3),"};"
			write(fh,*)" SQ(",x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),"){",gp_value(i,3),",",&
				gp_value(i,3),",",gp_value(i,3),",",gp_value(i,3),"};"
			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,3),",",&
				gp_value(i,3),",",gp_value(i,3),",",gp_value(i,3),"};"
			write(fh,*)" SQ(",x(5,1),",",x(5,2),",",x(5,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,3),",",&
				gp_value(i,3),",",gp_value(i,3),",",gp_value(i,3),"};"
				

			! 4/8

			x_double(6,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )

			x_double(3,1:3)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )
			
			x_double(7,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )
			

			x_double(1,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )

			x_double(8,1:3)= 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )

			x_double(4,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )

			
			x_double(2,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )

			x_double(5,1:3)=0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )

			x(:,:)=x_double(:,:) 



			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),"){",gp_value(i,4),",",&
				gp_value(i,4),",",gp_value(i,4),",",gp_value(i,4),"};"
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),"){",gp_value(i,4),",",&
				gp_value(i,4),",",gp_value(i,4),",",gp_value(i,4),"};"
			write(fh,*)" SQ(",x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),"){",gp_value(i,4),",",&
				gp_value(i,4),",",gp_value(i,4),",",gp_value(i,4),"};"
			write(fh,*)" SQ(",x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),"){",gp_value(i,4),",",&
				gp_value(i,4),",",gp_value(i,4),",",gp_value(i,4),"};"
			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,4),",",&
				gp_value(i,4),",",gp_value(i,4),",",gp_value(i,4),"};"
			write(fh,*)" SQ(",x(5,1),",",x(5,2),",",x(5,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,4),",",&
				gp_value(i,4),",",gp_value(i,4),",",gp_value(i,4),"};"
			



			! 5/8

			x_double(7,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )

			x_double(5,1:3)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )
			
			x_double(6,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )
			

			x_double(2,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )

			x_double(4,1:3)= 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )

			x_double(1,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )

			
			x_double(8,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )

			x_double(3,1:3)=0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )

			x(:,:)=x_double(:,:) 



			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),"){",gp_value(i,5),",",&
				gp_value(i,5),",",gp_value(i,5),",",gp_value(i,5),"};"
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),"){",gp_value(i,5),",",&
				gp_value(i,5),",",gp_value(i,5),",",gp_value(i,5),"};"
			write(fh,*)" SQ(",x(2,1),",",x(2,2),",",x(2,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),"){",gp_value(i,5),",",&
				gp_value(i,5),",",gp_value(i,5),",",gp_value(i,5),"};"
			write(fh,*)" SQ(",x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),"){",gp_value(i,5),",",&
				gp_value(i,5),",",gp_value(i,5),",",gp_value(i,5),"};"
			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,5),",",&
				gp_value(i,5),",",gp_value(i,5),",",gp_value(i,5),"};"
			write(fh,*)" SQ(",x(5,1),",",x(5,2),",",x(5,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,5),",",&
				gp_value(i,5),",",gp_value(i,5),",",gp_value(i,5),"};"
			
			! 6/8

			x_double(8,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )

			x_double(6,1:3)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )
			
			x_double(5,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )
			

			x_double(1,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )

			x_double(3,1:3)= 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )

			x_double(2,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )

			
			x_double(7,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )

			x_double(4,1:3)=0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )

			x(:,:)=x_double(:,:) 



			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),"){",gp_value(i,6),",",&
				gp_value(i,6),",",gp_value(i,6),",",gp_value(i,6),"};"
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),"){",gp_value(i,6),",",&
				gp_value(i,6),",",gp_value(i,6),",",gp_value(i,6),"};"
			write(fh,*)" SQ(",x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),"){",gp_value(i,6),",",&
				gp_value(i,6),",",gp_value(i,6),",",gp_value(i,6),"};"
			write(fh,*)" SQ(",x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),"){",gp_value(i,6),",",&
				gp_value(i,6),",",gp_value(i,6),",",gp_value(i,6),"};"
			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,6),",",&
				gp_value(i,6),",",gp_value(i,6),",",gp_value(i,6),"};"
			write(fh,*)" SQ(",x(5,1),",",x(5,2),",",x(5,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,6),",",&
				gp_value(i,6),",",gp_value(i,6),",",gp_value(i,6),"};"
			

			
			! 7/8

			x_double(5,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )

			x_double(7,1:3)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )
			
			x_double(8,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )
			

			x_double(4,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )

			x_double(2,1:3)= 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )

			x_double(3,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )

			
			x_double(6,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )

			x_double(1,1:3)=0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )

			x(:,:)=x_double(:,:) 



			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),"){",gp_value(i,7),",",&
				gp_value(i,7),",",gp_value(i,7),",",gp_value(i,7),"};"
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),"){",gp_value(i,7),",",&
				gp_value(i,7),",",gp_value(i,7),",",gp_value(i,7),"};"
			write(fh,*)" SQ(",x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),"){",gp_value(i,7),",",&
				gp_value(i,7),",",gp_value(i,7),",",gp_value(i,7),"};"
			write(fh,*)" SQ(",x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),"){",gp_value(i,7),",",&
				gp_value(i,7),",",gp_value(i,7),",",gp_value(i,7),"};"
			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,7),",",&
				gp_value(i,7),",",gp_value(i,7),",",gp_value(i,7),"};"
			write(fh,*)" SQ(",x(5,1),",",x(5,2),",",x(5,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,7),",",&
				gp_value(i,7),",",gp_value(i,7),",",gp_value(i,7),"};"
			

			

			
			! 8/8

			x_double(5,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )

			x_double(7,1:3)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )
			
			x_double(6,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )
			

			x_double(2,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )

			x_double(4,1:3)= 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )

			x_double(3,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )

			
			x_double(8,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )

			x_double(1,1:3)=0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )


			x(:,:)=x_double(:,:) 


			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),"){",gp_value(i,8),",",&
				gp_value(i,8),",",gp_value(i,8),",",gp_value(i,8),"};"
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),"){",gp_value(i,8),",",&
				gp_value(i,8),",",gp_value(i,8),",",gp_value(i,8),"};"
			write(fh,*)" SQ(",x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),"){",gp_value(i,8),",",&
				gp_value(i,8),",",gp_value(i,8),",",gp_value(i,8),"};"
			write(fh,*)" SQ(",x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),"){",gp_value(i,8),",",&
				gp_value(i,8),",",gp_value(i,8),",",gp_value(i,8),"};"
			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,8),",",&
				gp_value(i,8),",",gp_value(i,8),",",gp_value(i,8),"};"
			write(fh,*)" SQ(",x(5,1),",",x(5,2),",",x(5,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,8),",",&
				gp_value(i,8),",",gp_value(i,8),",",gp_value(i,8),"};"
			




        else
            print *, " size(obj%Mesh%ElemNod,2)==",size(obj%Mesh%ElemNod,2)
            print *, ".and. size(obj%Mesh%NodCoord,2)==",size(obj%Mesh%NodCoord,2)
			stop "plot_contour >> now constructing"
		endif
	enddo
	write(fh,*) '};'
	close(fh)
 end subroutine
 !===========================================================================================



! ########################################################################################
subroutine GmshPlotContour(obj,gp_value,OptionalContorName,OptionalAbb,OptionalStep,Name)
	class(FEMDomain_),intent(in)::obj
	real(8),intent(in)::gp_value(:,:)
	integer,optional,intent(in)::OptionalStep
	character,optional,intent(in):: OptionalContorName*30,OptionalAbb*6
	character(*),optional,intent(in)::Name
	real(8),allocatable::x_double(:,:)
	real(8),allocatable::x(:,:)
	integer i,j,k,step,fh
	character filename0*11
	character filename*25
	character filetitle*6
	character command*31
	character:: mapname*30,abbmap*6
	
	if(present(OptionalContorName) )then
		mapname=OptionalContorName
	else
		mapname="Value"
	endif

	if(present(OptionalAbb) )then
		abbmap=OptionalAbb
	else
		abbmap="Values"
	endif

	if(present(OptionalStep) )then
		step=OptionalStep
	else
		step=1
	endif
	fh=40

	filetitle(1:6)=abbmap(1:6)
	!---------------------
	write (filename0, '("_", i6.6, ".pos")') step ! ここでファイル名を生成している
	filename=filetitle//filename0
	!command="touch "//trim(obj%FileName)//trim(filename)
	call system("touch "//trim(obj%FileName)//trim(filename))


	open(fh,file=trim(obj%FileName)//trim(filename))
	print *, "writing ",trim(obj%FileName)//trim(filename)," step>>",step
	
	!---------------------
	if( size(obj%Mesh%ElemNod,2)==4 .and. size(obj%Mesh%NodCoord,2)==2 ) then
		allocate(x(4,3) )
		allocate(x_double(4,3) )
		
	elseif( size(obj%Mesh%ElemNod,2)==8 .and. size(obj%Mesh%NodCoord,2)==3 ) then
		allocate(x(8,3) )
		allocate(x_double(8,3) )
		
	endif


	x(:,:)=0.0d0
	write(fh,*) 'View "',mapname,'" {'
	do i=1,size(gp_value,1)
		if( size(obj%Mesh%ElemNod,2)==4 .and. size(obj%Mesh%NodCoord,2)==2 ) then
			
			! 2-D, 4 noded, isoparametric elements with four gauss points 
			x_double(1,1:2)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  )
			x_double(2,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  )
			x_double(3,1:2)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  )&
					+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  )
			x_double(4,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  )

			x(:,:)=x_double(:,:) 

			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),"){",gp_value(i,1),",",&
				gp_value(i,1),",",gp_value(i,1),",",gp_value(i,1),"};"
				

			x_double(1,1:2)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  )
			x_double(2,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  )
			x_double(3,1:2)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  )&
					+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  )
			x_double(4,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  )

			
			x(:,:)=x_double(:,:) 

			
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),"){",gp_value(i,2),",",&
				gp_value(i,2),",",gp_value(i,2),",",gp_value(i,2),"};"
				
			x_double(1,1:2)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  )
			x_double(2,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  )
			x_double(3,1:2)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  )&
					+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  )
			x_double(4,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  )
			
			x(:,:)=x_double(:,:) 

			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),"){",gp_value(i,3),",",&
				gp_value(i,3),",",gp_value(i,3),",",gp_value(i,3),"};"
				
			x_double(1,1:2)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  )
			x_double(2,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  )
			x_double(3,1:2)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  )&
					+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  )
			x_double(4,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  )
			
			x(:,:)=x_double(:,:) 
			
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),"){",gp_value(i,4),",",&
				gp_value(i,4),",",gp_value(i,4),",",gp_value(i,4),"};"
		elseif(size(obj%Mesh%ElemNod,2)==8 .and. size(obj%Mesh%NodCoord,2)==3  ) then
			
			
			! 3-D, 8 noded, isoparametric elements with 8 gauss points
			! 1/8

			x_double(1,1:3)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )
			x_double(2,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1), 1:3  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )
			x_double(3,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2), 1:3  )&
					+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4), 1:3  )
			x_double(4,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4), 1:3  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )
			
			x_double(5,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )

			x_double(6,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
					+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )
			
			x_double(7,1:3)=0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
					+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
					+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
					+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )

			x_double(8,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
					+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )
			
			x(:,:)=x_double(:,:) 



			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),"){",gp_value(i,1),",",&
				gp_value(i,1),",",gp_value(i,1),",",gp_value(i,1),"};"
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),"){",gp_value(i,1),",",&
				gp_value(i,1),",",gp_value(i,1),",",gp_value(i,1),"};"
			write(fh,*)" SQ(",x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),"){",gp_value(i,1),",",&
				gp_value(i,1),",",gp_value(i,1),",",gp_value(i,1),"};"
			write(fh,*)" SQ(",x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),"){",gp_value(i,1),",",&
				gp_value(i,1),",",gp_value(i,1),",",gp_value(i,1),"};"
			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,1),",",&
				gp_value(i,1),",",gp_value(i,1),",",gp_value(i,1),"};"
			write(fh,*)" SQ(",x(5,1),",",x(5,2),",",x(5,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,1),",",&
				gp_value(i,1),",",gp_value(i,1),",",gp_value(i,1),"};"
			
			! 2/8

			x_double(1,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1), 1:3  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )
			
			x_double(2,1:3)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )
			
			x_double(3,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )
			

			x_double(4,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )

			x_double(5,1:3)= 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )

			x_double(6,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )

			
			x_double(7,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )
			
			x_double(8,1:3)=0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )


			x(:,:)=x_double(:,:) 


			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),"){",gp_value(i,2),",",&
				gp_value(i,2),",",gp_value(i,2),",",gp_value(i,2),"};"
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),"){",gp_value(i,2),",",&
				gp_value(i,2),",",gp_value(i,2),",",gp_value(i,2),"};"
			write(fh,*)" SQ(",x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),"){",gp_value(i,2),",",&
				gp_value(i,2),",",gp_value(i,2),",",gp_value(i,2),"};"
			write(fh,*)" SQ(",x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),"){",gp_value(i,2),",",&
				gp_value(i,2),",",gp_value(i,2),",",gp_value(i,2),"};"
			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,2),",",&
				gp_value(i,2),",",gp_value(i,2),",",gp_value(i,2),"};"
			write(fh,*)" SQ(",x(5,1),",",x(5,2),",",x(5,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,2),",",&
				gp_value(i,2),",",gp_value(i,2),",",gp_value(i,2),"};"
			
			
			! 3/8

			x_double(8,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )

			x_double(3,1:3)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )
			
			x_double(2,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )
			

			x_double(1,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )

			x_double(6,1:3)= 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )

			x_double(7,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )

			
			x_double(4,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )

			x_double(5,1:3)=0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )


			x(:,:)=x_double(:,:) 


			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),"){",gp_value(i,3),",",&
				gp_value(i,3),",",gp_value(i,3),",",gp_value(i,3),"};"
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),"){",gp_value(i,3),",",&
				gp_value(i,3),",",gp_value(i,3),",",gp_value(i,3),"};"
			write(fh,*)" SQ(",x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),"){",gp_value(i,3),",",&
				gp_value(i,3),",",gp_value(i,3),",",gp_value(i,3),"};"
			write(fh,*)" SQ(",x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),"){",gp_value(i,3),",",&
				gp_value(i,3),",",gp_value(i,3),",",gp_value(i,3),"};"
			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,3),",",&
				gp_value(i,3),",",gp_value(i,3),",",gp_value(i,3),"};"
			write(fh,*)" SQ(",x(5,1),",",x(5,2),",",x(5,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,3),",",&
				gp_value(i,3),",",gp_value(i,3),",",gp_value(i,3),"};"
				

			! 4/8

			x_double(6,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )

			x_double(3,1:3)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )
			
			x_double(7,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )
			

			x_double(1,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )

			x_double(8,1:3)= 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )

			x_double(4,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )

			
			x_double(2,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )

			x_double(5,1:3)=0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )

			x(:,:)=x_double(:,:) 



			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),"){",gp_value(i,4),",",&
				gp_value(i,4),",",gp_value(i,4),",",gp_value(i,4),"};"
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),"){",gp_value(i,4),",",&
				gp_value(i,4),",",gp_value(i,4),",",gp_value(i,4),"};"
			write(fh,*)" SQ(",x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),"){",gp_value(i,4),",",&
				gp_value(i,4),",",gp_value(i,4),",",gp_value(i,4),"};"
			write(fh,*)" SQ(",x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),"){",gp_value(i,4),",",&
				gp_value(i,4),",",gp_value(i,4),",",gp_value(i,4),"};"
			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,4),",",&
				gp_value(i,4),",",gp_value(i,4),",",gp_value(i,4),"};"
			write(fh,*)" SQ(",x(5,1),",",x(5,2),",",x(5,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,4),",",&
				gp_value(i,4),",",gp_value(i,4),",",gp_value(i,4),"};"
			



			! 5/8

			x_double(7,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )

			x_double(5,1:3)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )
			
			x_double(6,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )
			

			x_double(2,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )

			x_double(4,1:3)= 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )

			x_double(1,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )

			
			x_double(8,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )

			x_double(3,1:3)=0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )

			x(:,:)=x_double(:,:) 



			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),"){",gp_value(i,5),",",&
				gp_value(i,5),",",gp_value(i,5),",",gp_value(i,5),"};"
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),"){",gp_value(i,5),",",&
				gp_value(i,5),",",gp_value(i,5),",",gp_value(i,5),"};"
			write(fh,*)" SQ(",x(2,1),",",x(2,2),",",x(2,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),"){",gp_value(i,5),",",&
				gp_value(i,5),",",gp_value(i,5),",",gp_value(i,5),"};"
			write(fh,*)" SQ(",x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),"){",gp_value(i,5),",",&
				gp_value(i,5),",",gp_value(i,5),",",gp_value(i,5),"};"
			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,5),",",&
				gp_value(i,5),",",gp_value(i,5),",",gp_value(i,5),"};"
			write(fh,*)" SQ(",x(5,1),",",x(5,2),",",x(5,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,5),",",&
				gp_value(i,5),",",gp_value(i,5),",",gp_value(i,5),"};"
			
			! 6/8

			x_double(8,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )

			x_double(6,1:3)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )
			
			x_double(5,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )
			

			x_double(1,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )

			x_double(3,1:3)= 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )

			x_double(2,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )

			
			x_double(7,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )

			x_double(4,1:3)=0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )

			x(:,:)=x_double(:,:) 



			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),"){",gp_value(i,6),",",&
				gp_value(i,6),",",gp_value(i,6),",",gp_value(i,6),"};"
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),"){",gp_value(i,6),",",&
				gp_value(i,6),",",gp_value(i,6),",",gp_value(i,6),"};"
			write(fh,*)" SQ(",x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),"){",gp_value(i,6),",",&
				gp_value(i,6),",",gp_value(i,6),",",gp_value(i,6),"};"
			write(fh,*)" SQ(",x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),"){",gp_value(i,6),",",&
				gp_value(i,6),",",gp_value(i,6),",",gp_value(i,6),"};"
			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,6),",",&
				gp_value(i,6),",",gp_value(i,6),",",gp_value(i,6),"};"
			write(fh,*)" SQ(",x(5,1),",",x(5,2),",",x(5,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,6),",",&
				gp_value(i,6),",",gp_value(i,6),",",gp_value(i,6),"};"
			

			
			! 7/8

			x_double(5,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )

			x_double(7,1:3)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )
			
			x_double(8,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )
			

			x_double(4,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )

			x_double(2,1:3)= 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )

			x_double(3,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )

			
			x_double(6,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )

			x_double(1,1:3)=0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )

			x(:,:)=x_double(:,:) 



			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),"){",gp_value(i,7),",",&
				gp_value(i,7),",",gp_value(i,7),",",gp_value(i,7),"};"
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),"){",gp_value(i,7),",",&
				gp_value(i,7),",",gp_value(i,7),",",gp_value(i,7),"};"
			write(fh,*)" SQ(",x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),"){",gp_value(i,7),",",&
				gp_value(i,7),",",gp_value(i,7),",",gp_value(i,7),"};"
			write(fh,*)" SQ(",x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),"){",gp_value(i,7),",",&
				gp_value(i,7),",",gp_value(i,7),",",gp_value(i,7),"};"
			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,7),",",&
				gp_value(i,7),",",gp_value(i,7),",",gp_value(i,7),"};"
			write(fh,*)" SQ(",x(5,1),",",x(5,2),",",x(5,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,7),",",&
				gp_value(i,7),",",gp_value(i,7),",",gp_value(i,7),"};"
			

			

			
			! 8/8

			x_double(5,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )

			x_double(7,1:3)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )
			
			x_double(6,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )
			

			x_double(2,1:3)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )

			x_double(4,1:3)= 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  ) + 0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )

			x_double(3,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4), 1:3  )+0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )

			
			x_double(8,1:3)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )

			x_double(1,1:3)=0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,5),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,6),1:3  )&
				+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,7),1:3  )+0.1250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,8),1:3  )


			x(:,:)=x_double(:,:) 


			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),"){",gp_value(i,8),",",&
				gp_value(i,8),",",gp_value(i,8),",",gp_value(i,8),"};"
			write(fh,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
			,x(2,1),",",x(2,2),",",x(2,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),"){",gp_value(i,8),",",&
				gp_value(i,8),",",gp_value(i,8),",",gp_value(i,8),"};"
			write(fh,*)" SQ(",x(2,1),",",x(2,2),",",x(2,3),","&
			,x(3,1),",",x(3,2),",",x(3,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),"){",gp_value(i,8),",",&
				gp_value(i,8),",",gp_value(i,8),",",gp_value(i,8),"};"
			write(fh,*)" SQ(",x(3,1),",",x(3,2),",",x(3,3),","&
			,x(4,1),",",x(4,2),",",x(4,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),"){",gp_value(i,8),",",&
				gp_value(i,8),",",gp_value(i,8),",",gp_value(i,8),"};"
			write(fh,*)" SQ(",x(4,1),",",x(4,2),",",x(4,3),","&
			,x(1,1),",",x(1,2),",",x(1,3),","&
			,x(5,1),",",x(5,2),",",x(5,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,8),",",&
				gp_value(i,8),",",gp_value(i,8),",",gp_value(i,8),"};"
			write(fh,*)" SQ(",x(5,1),",",x(5,2),",",x(5,3),","&
			,x(6,1),",",x(6,2),",",x(6,3),","&
			,x(7,1),",",x(7,2),",",x(7,3),","&
			,x(8,1),",",x(8,2),",",x(8,3),"){",gp_value(i,8),",",&
				gp_value(i,8),",",gp_value(i,8),",",gp_value(i,8),"};"
			




		else
			stop "plot_contour >> now constructing"
		endif
	enddo
	write(fh,*) '};'
	close(fh)
 end subroutine
!===========================================================================================

!===========================================================================================
subroutine GmshPlotVector(obj,Vector,Name,FieldName,Step,fh,withMsh,ElementWize,NodeWize,onlyDirichlet)
	class(FEMDomain_),intent(in)::obj
	real(8),optional,intent(in)::Vector(:,:)
	character(*),intent(in)::FieldName
	character(*),optional,intent(in)::Name
	integer,intent(in)::Step
	real(8),allocatable ::DBCVector(:,:) 
	integer,optional,intent(in)::fh
	logical,optional,intent(in)::withMsh,ElementWize,NodeWize,onlyDirichlet

	character :: filename0*11, filename1*11,center*15
	integer :: FileHandle,i,j,k,n,m
	FileHandle=input(default=1000,option=fh)


	if(present(onlyDirichlet) )then
		if(onlyDirichlet .eqv. .true.)then
			
			call obj%getDBCVector(DBCVector)
			do i=1,size(DBCVector,1)
				write(10,*) DBCVector(i,:)
			enddo

			center="$NodeData"
		
			! only for 3D
		
			write (filename0, '("_", i6.6, "_vec")') step 
			if(present(Name) )then
				open(FileHandle,file=Name//filename0//".msh")
				print *, Name//filename0//".msh"//" is exported!"
			else
				open(FileHandle,file="DBCVector"//filename0//".msh")
				print *, "DBCVector"//filename0//".msh"//" is exported!"
			endif
			write(FileHandle,'(A)') "$MeshFormat"
		
			write(FileHandle,'(A)')  "2.2 0 8"
			write(FileHandle,'(A)')  "$EndMeshFormat"
			write(FileHandle,'(A)')  trim(center)
			write(FileHandle,'(A)')  "1"
			write(FileHandle,'(A)')  '"'//FieldName//'"'
			write(FileHandle,'(A)')  "1"
			write(FileHandle,'(A)')  "0.0"
			write(FileHandle,'(A)')  "3"
			write(FileHandle,'(A)')  "1"
			write(FileHandle,'(A)')  "3"
			write(FileHandle,*) 	size(obj%Mesh%NodCoord,1)  
			do i=1,size(obj%Mesh%NodCoord,1)
				write(FileHandle,*) i,DBCVector(i,:)
			enddo

			close(FileHandle)
		
			if(present(withMsh) )then
				if(withMsh .eqv. .true.)then

				
				
					write (filename1, '("_", i6.6, "_msh")') step
					if(present(Name) )then
						open(FileHandle,file=Name//filename1//".msh")
						print *, Name//filename1//".msh"//" is exported!"
					else
						open(FileHandle,file="DBCVector"//filename1//".msh")
						print *, "DBCVector"//filename1//".msh"//" is exported!"
					endif
					write(FileHandle,'(A)') "$MeshFormat"
				
					write(FileHandle,'(A)')  "2.2 0 8"
					write(FileHandle,'(A)')  "$EndMeshFormat"
					write(FileHandle,'(A)')  "$Nodes"
					write(FileHandle,*) 	size(obj%Mesh%NodCoord,1)  
					do i=1,size(obj%Mesh%NodCoord,1)
						write(FileHandle,*) i,obj%Mesh%NodCoord(i,:)
					enddo
					write(FileHandle,'(A)')  "$EndNodes"
					write(FileHandle,'(A)')  "$Elements"
					write(FileHandle,*) 	size(obj%Mesh%ElemNod,1)
					do i=1,size(obj%Mesh%ElemNod,1)
						write(FileHandle,*) i,"5 2 0 1 ",obj%Mesh%ElemNod(i,:)
					enddo  
					write(FileHandle,'(A)')  "$EndElements"

					close(FileHandle)
				endif
			endif
			return
		
			
		endif
	endif
	
	if(present(NodeWize) )then
		if(NodeWize .eqv. .true.)then
			center="$NodeData"
		else
			center="$ElementData"
		endif
	elseif(present(ElementWize))then
		if(ElementWize .eqv. .true.)then
			center="$ElementData"
		else
			center="$NodeData"
		endif
	else
		center="$NodeData"
	endif

	! only for 3D

	write (filename0, '("_", i6.6, "_vec")') step 
	if(present(Name) )then
		open(FileHandle,file=Name//filename0//".msh")
		print *, Name//filename0//".msh"//" is exported!"
	else
		open(FileHandle,file="Vector"//filename0//".msh")
		print *, "Vector"//filename0//".msh"//" is exported!"
	endif
	write(FileHandle,'(A)') "$MeshFormat"

	write(FileHandle,'(A)')  "2.2 0 8"
	write(FileHandle,'(A)')  "$EndMeshFormat"
	write(FileHandle,'(A)')  trim(center)
	write(FileHandle,'(A)')  "1"
	write(FileHandle,'(A)')  '"'//FieldName//'"'
	write(FileHandle,'(A)')  "1"
	write(FileHandle,'(A)')  "0.0"
	write(FileHandle,'(A)')  "3"
	write(FileHandle,'(A)')  "1"
	write(FileHandle,'(A)')  "3"
	write(FileHandle,*) 	size(obj%Mesh%NodCoord,1)  
	do i=1,size(obj%Mesh%NodCoord,1)
		write(FileHandle,*) i,Vector(i,:)
	enddo
	
	close(FileHandle)

	if(present(withMsh) )then
		if(withMsh .eqv. .true.)then
			
		
		
			write (filename1, '("_", i6.6, "_msh")') step
			if(present(Name) )then
				open(FileHandle,file=Name//filename1//".msh")
				print *, Name//filename1//".msh"//" is exported!"
			else
				open(FileHandle,file="Vector"//filename1//".msh")
				print *, "Vector"//filename1//".msh"//" is exported!"
			endif
			write(FileHandle,'(A)') "$MeshFormat"
		
			write(FileHandle,'(A)')  "2.2 0 8"
			write(FileHandle,'(A)')  "$EndMeshFormat"
			write(FileHandle,'(A)')  "$Nodes"
			write(FileHandle,*) 	size(obj%Mesh%NodCoord,1)  
			do i=1,size(obj%Mesh%NodCoord,1)
				write(FileHandle,*) i,obj%Mesh%NodCoord(i,:)
			enddo
			write(FileHandle,'(A)')  "$EndNodes"
			write(FileHandle,'(A)')  "$Elements"
			write(FileHandle,*) 	size(obj%Mesh%ElemNod,1)
			do i=1,size(obj%Mesh%ElemNod,1)
				write(FileHandle,*) i,"5 2 0 1 ",obj%Mesh%ElemNod(i,:)
			enddo  
			write(FileHandle,'(A)')  "$EndElements"
			
			close(FileHandle)
		endif
	endif

	
	
end subroutine
!===========================================================================================


subroutine GmshPlotContour2D(obj,gp_value,OptionalContorName,OptionalAbb,OptionalStep,Name)
	class(FEMDomain_),intent(in)::obj
	real(8),intent(in)::gp_value(:,:)
	integer,optional,intent(in)::OptionalStep
	character,optional,intent(in):: OptionalContorName*30,OptionalAbb*6
	character(*),optional,intent(in)::Name
	real(8),allocatable::x(:,:)
	integer i,j,k,step
	character filename0*11
	character filename*17
	character filetitle*6
	character:: mapname*30,abbmap*6
	
	if(present(OptionalContorName) )then
		mapname=OptionalContorName
	else
		mapname="Value"
	endif

	if(present(OptionalAbb) )then
		abbmap=OptionalAbb
	else
		abbmap="Values"
	endif

	if(present(OptionalStep) )then
		step=OptionalStep
	else
		step=1
	endif


	filetitle(1:6)=abbmap(1:6)
	!---------------------
	write (filename0, '("_", i6.6, ".pos")') step ! ここでファイル名を生成している
	filename=filetitle//filename0


	open(40,file=trim(obj%FileName)//filetitle//filename0)
	print *, "writing ",trim(obj%FileName)//filetitle//filename0," step>>",step	

	!---------------------
	allocate(x(4,3) )
	x(:,:)=0.0d0
	write(40,*) 'View "',mapname,'" {'
	do i=1,size(obj%Mesh%ElemNod,1)
		if( size(obj%Mesh%ElemNod,2)/=4)  stop  "GmshPlotContour >> now constructing"
		
		
		x(1,1:2)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  )
		x(2,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  )
		x(3,1:2)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  )
		x(4,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  )
		write(40,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
		,x(2,1),",",x(2,2),",",x(2,3),","&
		,x(3,1),",",x(3,2),",",x(3,3),","&
		,x(4,1),",",x(4,2),",",x(4,3),"){",gp_value(i,1),",",&
			gp_value(i,1),",",gp_value(i,1),",",gp_value(i,1),"};"
			
		x(1,1:2)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  )
		x(2,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  )
		x(3,1:2)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  )
		x(4,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  )
		write(40,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
		,x(2,1),",",x(2,2),",",x(2,3),","&
		,x(3,1),",",x(3,2),",",x(3,3),","&
		,x(4,1),",",x(4,2),",",x(4,3),"){",gp_value(i,2),",",&
			gp_value(i,2),",",gp_value(i,2),",",gp_value(i,2),"};"
			
		x(1,1:2)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  )
		x(2,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  )
		x(3,1:2)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  )
		x(4,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  )
		write(40,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
		,x(2,1),",",x(2,2),",",x(2,3),","&
		,x(3,1),",",x(3,2),",",x(3,3),","&
		,x(4,1),",",x(4,2),",",x(4,3),"){",gp_value(i,3),",",&
			gp_value(i,3),",",gp_value(i,3),",",gp_value(i,3),"};"
			
		x(1,1:2)=obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  )
		x(2,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  )
		x(3,1:2)=0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,1),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,2),1:2  )&
				+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  )+0.250d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  )
		x(4,1:2)=0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,3),1:2  ) + 0.50d0*obj%Mesh%NodCoord(obj%Mesh%ElemNod(i,4),1:2  )
		write(40,*)" SQ(",x(1,1),",",x(1,2),",",x(1,3),","&
		,x(2,1),",",x(2,2),",",x(2,3),","&
		,x(3,1),",",x(3,2),",",x(3,3),","&
		,x(4,1),",",x(4,2),",",x(4,3),"){",gp_value(i,4),",",&
			gp_value(i,4),",",gp_value(i,4),",",gp_value(i,4),"};"
	enddo
	write(40,*) '};'
 end subroutine GmshPlotContour2D
 !===========================================================================================
subroutine GmshExportStress(obj,uvec,sigma,strain_measure,step,Name )
	class(FEMDomain_),intent(in)::obj
	real(8),intent(in)::uvec(:),sigma(:,:,:),strain_measure(:,:,:)
	integer,intent(in)::step
	character p_stress_field*30
	real(8),allocatable::c_nod_coord(:,:),gp_value(:,:),F_iJ(:,:),b_ij(:,:)
	real(8) tr_sigma,tr_C,tr_b
	character q_stress_field*30
	character p_strain_field*30
	character q_strain_field*30
	character mapname*30,abbrivation*6
	character(*),optional,intent(in)::Name
	integer i,j,n,gp_number,dim_num
	
	gp_number=size(strain_measure,2)
	dim_num=size(obj%Mesh%NodCoord,2)
	
	p_stress_field="Hydrostatic stress (kPa)"
	q_stress_field="Deviatoric stress (kPa)"
	p_strain_field="Hydrostatic strain "
	q_strain_field="Deviatoric strain "
	
	allocate(F_iJ(3,3),b_ij(3,3) )
	
	allocate( c_nod_coord(size(obj%Mesh%NodCoord,1),size(obj%Mesh%NodCoord,2))) 
	allocate(gp_value(size(obj%Mesh%ElemNod,1),gp_number  ))
	
	do i=1,size(obj%Mesh%NodCoord,1)
		c_nod_coord(i,:)=obj%Mesh%NodCoord(i,:)+uvec(dim_num*(i-1)+1:dim_num*i )
	enddo
	
	!!"Hydrostatic stress (kPa)"
	do i=1,size(sigma,1)
		do j=1,size(sigma,2)
			if(dim_num==2)then
				gp_value(i,j)=sigma(i,j,1)+sigma(i,j,2)+sigma(i,j,4)
				gp_value(i,j)=gp_value(i,j)/3.0d0
			elseif(dim_num==3)then
				gp_value(i,j)=sigma(i,j,1)+sigma(i,j,2)+sigma(i,j,3)
				gp_value(i,j)=gp_value(i,j)/3.0d0
			endif
		enddo
	enddo
	mapname=p_stress_field
	abbrivation="Hysigm"
	call GmshPlotContour(obj,gp_value,mapname,abbrivation,step,Name=Name)
	
	!!""Deviatoric stress (kPa)"
	do i=1,size(sigma,1)
		do j=1,size(sigma,2)
			if(dim_num==2)then
				tr_sigma=sigma(i,j,1)+sigma(i,j,2)+sigma(i,j,4)

				gp_value(i,j)=( 1.50d0*( sigma(i,j,1)*sigma(i,j,1) +&
				sigma(i,j,2)*sigma(i,j,2) +sigma(i,j,4)*sigma(i,j,4) + sigma(i,j,3)*sigma(i,j,3)*2.0d0 -&
				tr_sigma*tr_sigma/3.0d0)   )**(0.50d0)
			elseif(dim_num==3)then
				
				gp_value(i,j)=( 1.50d0*( sigma(i,j,1)*sigma(i,j,1) +&
				sigma(i,j,2)*sigma(i,j,2) +sigma(i,j,3)*sigma(i,j,3) + sigma(i,j,4)*sigma(i,j,4)*3.0d0 &
				+ sigma(i,j,5)*sigma(i,j,5)*3.0d0 + sigma(i,j,6)*sigma(i,j,6)*3.0d0 &
				- sigma(i,j,1)*sigma(i,j,2)- sigma(i,j,2)*sigma(i,j,3)- sigma(i,j,3)*sigma(i,j,1)  )   )**(0.50d0)
			
			else
				stop "dim_num should be 2 or 3, GmshExportStress"
			endif
		enddo
	enddo
	mapname=q_stress_field
	abbrivation="Dvsigm"
	call GmshPlotContour(obj,gp_value,mapname,abbrivation,step,Name=Name)
	
	!!"Hydrostatic strain"
	do i=1,size(strain_measure,1)
		do j=1,size(strain_measure,2)
			F_iJ(:,:)=0.0d0
			if(dim_num==2)then
				F_iJ(1,1)=strain_measure(i,j,11)
				F_iJ(2,1)=strain_measure(i,j,14)
				F_iJ(1,2)=strain_measure(i,j,13)
				F_iJ(2,2)=strain_measure(i,j,12)
				F_iJ(3,3)=1.0d0
			elseif(dim_num==3)then
				F_iJ(1,1)=strain_measure(i,j,11)
				F_iJ(2,1)=strain_measure(i,j,14)
				F_iJ(1,2)=strain_measure(i,j,13)
				F_iJ(2,2)=strain_measure(i,j,12)
				F_iJ(3,3)=strain_measure(i,j,27)
				F_iJ(1,3)=strain_measure(i,j,28)
				F_iJ(2,3)=strain_measure(i,j,29)
				F_iJ(3,1)=strain_measure(i,j,30)
				F_iJ(3,2)=strain_measure(i,j,31)
			else
				stop "dim_num should be 2 or 3"
			endif
			b_ij(:,:)=matmul(F_iJ,transpose(F_iJ) )
			gp_value(i,j)=b_iJ(1,1)+b_iJ(2,2)+b_iJ(3,3)
			gp_value(i,j)=gp_value(i,j)/3.0d0
		enddo
	enddo
	mapname=p_strain_field
	abbrivation="Hyepsi"
	call GmshPlotContour(obj,gp_value,mapname,abbrivation,step,Name=Name)
	
	!!"Deviatoric strain"
	do i=1,size(strain_measure,1)
		do j=1,size(strain_measure,2)
			F_iJ(:,:)=0.0d0
			if(dim_num==2)then
				F_iJ(1,1)=strain_measure(i,j,11)
				F_iJ(2,1)=strain_measure(i,j,14)
				F_iJ(1,2)=strain_measure(i,j,13)
				F_iJ(2,2)=strain_measure(i,j,12)
				F_iJ(3,3)=1.0d0
			elseif(dim_num==3)then
				F_iJ(1,1)=strain_measure(i,j,11)
				F_iJ(2,1)=strain_measure(i,j,14)
				F_iJ(1,2)=strain_measure(i,j,13)
				F_iJ(2,2)=strain_measure(i,j,12)
				F_iJ(3,3)=strain_measure(i,j,27)
				F_iJ(1,3)=strain_measure(i,j,28)
				F_iJ(2,3)=strain_measure(i,j,29)
				F_iJ(3,1)=strain_measure(i,j,30)
				F_iJ(3,2)=strain_measure(i,j,31)
			else
				stop "dim_num should be 2 or 3"
			endif
			
			b_ij(:,:)=matmul(F_iJ,transpose(F_iJ) )
			gp_value(i,j)=( 1.50d0*( b_ij(1,1)*b_ij(1,1) +&
				b_ij(2,2)*b_ij(2,2) +b_ij(3,3)*b_ij(3,3) + b_ij(1,2)*b_ij(1,2)*3.0d0 &
				+ b_ij(2,3)*b_ij(2,3)*3.0d0 + b_ij(3,1)*b_ij(3,1)*3.0d0 &
				- b_ij(1,1)*b_ij(2,2)- b_ij(2,2)*b_ij(3,3)- b_ij(3,3)*b_ij(1,1)  )   )**(0.50d0)
			
		enddo
	enddo
	mapname=p_stress_field
	abbrivation="Dvepsi"
	call GmshPlotContour(obj,gp_value,mapname,abbrivation,step,Name=Name)
	
 end subroutine
 !=======================================================================================
subroutine GnuplotPlotContour(obj,gp_value,OptionalContorName,OptionalAbb,OptionalStep)
	class(FEMDomain_),intent(in)::obj
	real(8),intent(in)::gp_value(:,:)
	integer,optional,intent(in)::OptionalStep
	character,optional,intent(in):: OptionalContorName*30,OptionalAbb*6
	real(8),allocatable::x(:,:)
	integer i,j,k,step,n
	character filename0*11
	character filename*17
	character filetitle*6
	character:: mapname*30,abbmap*6
	
	if(present(OptionalContorName) )then
		mapname=OptionalContorName
	else
		mapname="Value"
	endif

	if(present(OptionalAbb) )then
		abbmap=OptionalAbb
	else
		abbmap="Values"
	endif

	if(present(OptionalStep) )then
		step=OptionalStep
	else
		step=1
	endif


	filetitle(1:6)=abbmap(1:6)
	!---------------------
	write (filename0, '("_", i6.6, ".txt")') step ! ここでファイル名を生成している
	filename=filetitle//filename0
	open(40,file="touch "//trim(obj%FileName)//filename)
	print *, "writing .gnuplot-txt file... step>>",step
	!---------------------

    do i=1,size(gp_value,1)
        do j=1,size(gp_value,2)
            n=obj%Mesh%ElemNod(i,j)
            write(40,*) obj%Mesh%NodCoord(n,:),&
                gp_value(i,j)
        enddo
    enddo
    close(40)
 end subroutine GnuplotPlotContour
 !===========================================================================================
 
 !===========================================================================================
subroutine GnuplotExportStress(obj,uvec,sigma,strain_measure,step )
	class(FEMDomain_),intent(in)::obj
	real(8),intent(in)::uvec(:),sigma(:,:,:),strain_measure(:,:,:)
	integer,intent(in)::step
	character p_stress_field*30
	
	real(8),allocatable::c_nod_coord(:,:),gp_value(:,:)
	real(8) tr_sigma,tr_C
	character q_stress_field*30
	character p_strain_field*30
	character q_strain_field*30
	character mapname*30,abbrivation*6
	integer i,j,n,gp_number,dim_num
	
	
	gp_number=size(strain_measure,2)
	dim_num=size(obj%Mesh%NodCoord,2)
	
	p_stress_field="Hydrostatic stress (kPa)"
	q_stress_field="Deviatoric stress (kPa)"
	p_strain_field="Hydrostatic strain "
	q_strain_field="Deviatoric strain "
	
	allocate( c_nod_coord(size(obj%Mesh%NodCoord,1),size(obj%Mesh%NodCoord,2))) 
	allocate(gp_value(size(obj%Mesh%ElemNod,1),gp_number  ))
	
	do i=1,size(obj%Mesh%NodCoord,1)
		c_nod_coord(i,:)=obj%Mesh%NodCoord(i,:)+uvec(dim_num*(i-1)+1:dim_num*i )
	enddo
	
	!!"Hydrostatic stress (kPa)"
	do i=1,size(sigma,1)
		do j=1,size(sigma,2)
			gp_value(i,j)=sigma(i,j,1)+sigma(i,j,2)+sigma(i,j,4)
			gp_value(i,j)=gp_value(i,j)/3.0d0
		enddo
	enddo
	mapname=p_stress_field
	abbrivation="Hysigm"
	call GnuplotPlotContour(obj,gp_value,mapname,abbrivation,step)
	
	!!""Deviatoric stress (kPa)"
	do i=1,size(sigma,1)
		do j=1,size(sigma,2)
			tr_sigma=sigma(i,j,1)+sigma(i,j,2)+sigma(i,j,4)
			gp_value(i,j)=( 1.50d0*( sigma(i,j,1)*sigma(i,j,1) +&
			sigma(i,j,2)*sigma(i,j,2) +sigma(i,j,4)*sigma(i,j,4) + sigma(i,j,3)*sigma(i,j,3)*2.0d0 -&
			tr_sigma*tr_sigma/3.0d0)   )**(0.50d0)
		enddo
	enddo
	mapname=q_stress_field
	abbrivation="Dvsigm"
	call GnuplotPlotContour(obj,gp_value,mapname,abbrivation,step)
	
	!!"Hydrostatic strain"
	do i=1,size(strain_measure,1)
		do j=1,size(strain_measure,2)
			gp_value(i,j)=strain_measure(i,j,4)+strain_measure(i,j,5)+1.0d0
			gp_value(i,j)=gp_value(i,j)/3.0d0
		enddo
	enddo
	mapname=p_strain_field
	abbrivation="Hyepsi"
	call GnuplotPlotContour(obj,gp_value,mapname,abbrivation,step)
	
	!!"Deviatoric strain"
	do i=1,size(strain_measure,1)
		do j=1,size(strain_measure,2)
			tr_C=strain_measure(i,j,4)+strain_measure(i,j,5)+1.0d0
			gp_value(i,j)=( 1.50d0*( strain_measure(i,j,4)*strain_measure(i,j,4) +&
			strain_measure(i,j,5)*strain_measure(i,j,5) +1.0d0&
			+ strain_measure(i,j,6)*strain_measure(i,j,6)*2.0d0 -&
			tr_C*tr_C/3.0d0)   )**(0.50d0)
		enddo
	enddo
	mapname=p_stress_field
	abbrivation="Dvepsi"
	call GnuplotPlotContour(obj,gp_value,mapname,abbrivation,step)
	
 end subroutine
 !=======================================================================================

! ################################################
subroutine moveFEMDomain(obj,x,y,z)
	class(FEMDomain_),intent(inout)::obj
	real(8),optional,intent(in)::x,y,z
	
	
	if(present(x) )then
		obj%Mesh%NodCoord(:,1)=obj%Mesh%NodCoord(:,1)+x
	endif


	if(present(y) )then
		obj%Mesh%NodCoord(:,2)=obj%Mesh%NodCoord(:,2)+y
	endif


	if(size(obj%Mesh%NodCoord,2) <3 .and. present(z))then
		print *, "ERROR :: moveFEMDomain >> z cannot be imported"
		return
	endif

	if(present(z) )then
		obj%Mesh%NodCoord(:,3)=obj%Mesh%NodCoord(:,3)+z
	endif
end subroutine
! ################################################
 

! ################################################
subroutine rotateFEMDomain(obj,x,y,z)
	class(FEMDomain_),intent(inout)::obj
	real(8),optional,intent(in)::x,y,z
	real(8),allocatable :: midpoint(:),rotmat(:,:),rotation(:),coord(:)
	integer :: i,j,n,m

	n=size(obj%Mesh%NodCoord,2)
	m=size(obj%Mesh%NodCoord,1)
	allocate(midpoint(n) )
	allocate(rotmat(n,n) )
	allocate(coord(n) )
	allocate(rotation(n) )
	
	midpoint(:)=0.0d0

	do i=1,m
		midpoint(:)=midpoint(:)+1.0d0/dble(m)*obj%Mesh%NodCoord(i,:)
	enddo

	if(present(x) )then
		do i=1,m
			coord(:)=obj%Mesh%NodCoord(i,:)-midpoint(:)

			rotmat(:,:)=0.0d0
			if(n==2)then
				rotmat(1,1)=cos(x)  ;rotmat(1,2) =-sin(x)    
				rotmat(2,1)=sin(x)   ;rotmat(2,2)= cos(x)  
			elseif(n==3)then
				rotmat(1,1)=1.0d0	;rotmat(1,2)=0.0d0		;rotmat(1,3)=0.0d0			;
				rotmat(2,1)=0.0d0	;rotmat(2,2)=cos(x)		;rotmat(2,3)=-sin(x)		;
				rotmat(3,1)=0.0d0	;rotmat(3,2)=sin(x)		;rotmat(3,3)= cos(x)		;
			else
				print *,"Error :: rotateFEMDomain:: size(obj%Mesh%NodCoord,2)=",n
				stop 
			endif

			rotation(:)=matmul(rotmat,coord)

			obj%Mesh%NodCoord(i,:)=midpoint(:)+rotation(:)
			
		enddo
		
	endif


	if(present(y) )then
		do i=1,m
			coord(:)=obj%Mesh%NodCoord(i,:)-midpoint(:)

			rotmat(:,:)=0.0d0
			if(n==2)then
				rotmat(1,1)=cos(y)  ;rotmat(1,2) =-sin(y)    
				rotmat(2,1)=sin(y)   ;rotmat(2,2)= cos(y)  
			elseif(n==3)then
				rotmat(1,1)=cos(y)	;rotmat(1,2)=0.0d0		;rotmat(1,3)=sin(y)			;
				rotmat(2,1)=0.0d0	;rotmat(2,2)=1.0d0		;rotmat(2,3)=0.0d0		;
				rotmat(3,1)=-sin(y)	;rotmat(3,2)=0.0d0		;rotmat(3,3)= cos(y)		;
			else
				print *,"Error :: rotateFEMDomain:: size(obj%Mesh%NodCoord,2)=",n
				stop 
			endif

			rotation(:)=matmul(rotmat,coord)

			obj%Mesh%NodCoord(i,:)=midpoint(:)+rotation(:)
			
		enddo
		
	endif


	if(size(obj%Mesh%NodCoord,2) <3 .and. present(z))then
		print *, "ERROR :: moveFEMDomain >> z cannot be imported"
		return
	endif

	if(present(z) )then
		do i=1,m
			coord(:)=obj%Mesh%NodCoord(i,:)-midpoint(:)

			rotmat(:,:)=0.0d0
			if(n==2)then
				rotmat(1,1)=cos(z)  ;rotmat(1,2) =-sin(z)    
				rotmat(2,1)=sin(z)   ;rotmat(2,2)= cos(z)  
			elseif(n==3)then
				rotmat(1,1)=cos(z)	;rotmat(1,2)=-sin(z)	;rotmat(1,3)=0.0d0		;
				rotmat(2,1)=sin(z)	;rotmat(2,2)=cos(z)		;rotmat(2,3)=0.0d0		;
				rotmat(3,1)=0.0d0	;rotmat(3,2)=0.0d0		;rotmat(3,3)=1.0d0 		;
			else
				print *,"Error :: rotateFEMDomain:: size(obj%Mesh%NodCoord,2)=",n
				stop 
			endif

			rotation(:)=matmul(rotmat,coord)

			obj%Mesh%NodCoord(i,:)=midpoint(:)+rotation(:)
			
		enddo
	endif

end subroutine
! ################################################



! ################################################
subroutine AddNBCFEMDomain(obj,NodID,DimID,Val,FastMode)
	class(FEMDomain_),intent(inout)::obj
	integer,intent(in)::NodID,DimID
	real(8),intent(in)::Val
	logical,optional,intent(in)::FastMode
	integer :: installed,i,j,n
	logical :: fmode

	if(present(FastMode) )then
		fmode=FastMode
	else
		fmode=.false.
	endif
	fmode = input(default=.false.,option=FastMode)

	if(.not.allocated(obj%Boundary%NBoundNodID))then
		print *, "ERROR  :: AddNBC >> obj%Boundary%NBoundNodID should be allocated."
		print *, "Initializing NBC..."
		call obj%InitNBC(NumOfValPerNod=3)
		return
	endif

	! check wheather NodID exisits or not
	! if obj%Boundary%NBoundNodID(NodID) is found, add the current Val to the last value and return.
	do i=1,size(obj%Boundary%NBoundNodID,1)
		if(obj%Boundary%NBoundNodID(i,DimID)==NodID)then
			obj%Boundary%NBoundVal(i,DimID)=obj%Boundary%NBoundVal(i,DimID)+Val
			return
		endif 
	enddo
	
	if(fmode .eqv. .false.)then
		installed=0
		do i=1,size(obj%Boundary%NBoundNodID,1)
			if(obj%Boundary%NBoundNodID(i,DimID)==-1  )then
				obj%Boundary%NBoundNodID(i,DimID)=NodID
				obj%Boundary%NBoundVal(i,DimID)=Val
				obj%Boundary%NBoundNum(DimID)=obj%Boundary%NBoundNum(DimID)+1
				installed=1
				exit
			else
				cycle
			endif
		enddo
	endif

	if(installed==1)then
		return
	else
		n=size(obj%Boundary%NBoundNodID,1)
		call insertArray(obj%Boundary%NBoundNodID ,insert1stColumn=.true.,DefaultValue=-1 ,NextOf=n)
		call insertArray(obj%Boundary%NBoundVal ,insert1stColumn=.true.,DefaultValue=0.0d0,NextOf=n)
		i=n+1
		obj%Boundary%NBoundNodID(i,DimID)=NodID
		obj%Boundary%NBoundVal(i,DimID)=Val
		obj%Boundary%NBoundNum(DimID)=obj%Boundary%NBoundNum(DimID)+1
		
	endif

end subroutine
! ################################################

subroutine ExportFEMDomainAsSTL(obj,OptionalProjectName,FileHandle,SolverType,MeshDimension)
	class(FEMDomain_),intent(inout)::obj
	integer,optional,intent(in)::FileHandle,MeshDimension
	character(*),optional,intent(in)::OptionalProjectName,SolverType	
	real(8) :: x1(3),x2(3),x3(3)
	character*11  :: filename0
	integer :: fh,i,dim_num
	
	dim_num=input(default=3,option=MeshDimension)

    if(present(FileHandle) )then
        fh=FileHandle
    else
        fh =104
    endif

	write (filename0, '("_", i6.6, ".stl")') obj%Timestep ! ここでファイル名を生成している
	call system(  "touch "//trim(obj%FileName)//trim(filename0) )
	print *, trim(obj%FileName)//trim(filename0)
	open(fh,file=trim(obj%FileName)//trim(filename0) )

	call obj%Mesh%GetSurface()
	
	if(dim_num/=3)then
		print *, "Sorry, Export stl is supported only for 3-D mesh"
		close(fh)
		return
	endif
	write(fh,'(A)') "solid "//trim(obj%FileName)
	print *, "Number of facet is",size(obj%Mesh%FacetElemNod,1)
	do i=1,size(obj%Mesh%FacetElemNod,1)
		if(size(obj%Mesh%FacetElemNod,2)==4  )then
			! rectangular
			! describe two triangular
			x1(:)=obj%Mesh%NodCoord(obj%Mesh%FacetElemNod(i,1),: ) 
			x2(:)=obj%Mesh%NodCoord(obj%Mesh%FacetElemNod(i,2),: )
			x3(:)=obj%Mesh%NodCoord(obj%Mesh%FacetElemNod(i,3),: )
			write(fh,'(A)') "facet normal 0.0 0.0 1.0"
			write(fh,'(A)') "outer loop"
			write(fh,*) "vertex ",real(x1(1) ),real(x1(2) ),real(x1(3) )
			write(fh,*) "vertex ",real(x2(1) ),real(x2(2) ),real(x2(3) )
			write(fh,*) "vertex ",real(x3(1) ),real(x3(2) ),real(x3(3) )
			write(fh,'(A)') "endloop"
			write(fh,'(A)') "endfacet"
			x1(:)=obj%Mesh%NodCoord(obj%Mesh%FacetElemNod(i,1),: ) 
			x2(:)=obj%Mesh%NodCoord(obj%Mesh%FacetElemNod(i,3),: )
			x3(:)=obj%Mesh%NodCoord(obj%Mesh%FacetElemNod(i,4),: )
			write(fh,'(A)') "facet normal 0.0 0.0 1.0"
			write(fh,'(A)') "outer loop"
			write(fh,*) "vertex ",real(x1(1) ),real(x1(2) ),real(x1(3) )
			write(fh,*) "vertex ",real(x2(1) ),real(x2(2) ),real(x2(3) )
			write(fh,*) "vertex ",real(x3(1) ),real(x3(2) ),real(x3(3) )
			write(fh,'(A)') "endloop"
			write(fh,'(A)') "endfacet"
		else
			! other
			print *, "Sorry, Export stl is supported only for rectangular mesh"
			return
			close(fh)
		endif
	enddo
	write(fh,'(A)') "endsolid "//trim(obj%FileName)

	print *, "writing ",trim(obj%FileName)//trim(filename0)," step>>",obj%Timestep
	flush(fh)
	close(fh)

end subroutine


!#######################################
subroutine meshingFEMDomain(obj)
	class(FEMDomain_),intent(inout)::obj

	call obj%Mesh%meshing()
end subroutine
!#######################################


!#######################################
subroutine removeDBoundCondition(obj)
	class(FEMDomain_),intent(inout)::obj

	call obj%Boundary%removeDBC()
end subroutine
!#######################################

!#######################################
subroutine removeNBoundCondition(obj)
	class(FEMDomain_),intent(inout)::obj
	call obj%Boundary%removeNBC()
end subroutine
!#######################################


!#######################################
subroutine removeTBoundCondition(obj)
	class(FEMDomain_),intent(inout)::obj
	call obj%Boundary%removeTBC()
end subroutine
!#######################################

!#######################################
subroutine CheckConnedctivityFEMDomain(obj,fix)
	class(FEMDomain_),intent(inout)::obj
	integer,allocatable:: checklist(:,:),new_node_id(:)
	logical,optional,intent(in)::fix
	integer :: i,n,m,j

	n=size(obj%Mesh%NodCoord,1)
	allocate(checklist(n,1),new_node_id(n) )
	checklist(:,1)=0

	do i=1,n
		new_node_id(i)=i
	enddo

	do i=1,size(obj%Mesh%ElemNod,1)
		do j=1,size(obj%Mesh%ElemNod,2)
			checklist( obj%Mesh%ElemNod(i,j),1 )=1
		enddo
	enddo

	do i=1,n
		if(checklist(i,1) ==0)then
			! update node id
			do j=i+1,n
				new_node_id(j)=new_node_id(j)-1	
			enddo
			new_node_id(i)=0
		else
			cycle
		endif
	enddo
	

	if(minval(checklist)==0 )then
		print *, "[ERROR] Non-connected nodes exist"
	else
		print *, "[OK] All nodes are connected."
	endif

	if(present(fix) )then
		if( fix .eqv. .true. )then
			! update connectivity
			do i=1,size(obj%Mesh%ElemNod,1)
				do j=1,size(obj%Mesh%ElemNod,2)
					if(new_node_id(obj%Mesh%ElemNod(i,j))==0)then
						print *, "ERROR :: CheckConnedctivityFEMDomain"
					endif
					obj%Mesh%ElemNod(i,j)=new_node_id(obj%Mesh%ElemNod(i,j))
				enddo
			enddo
			
			! remove astray node
			i=1
			do 
				if(checklist(i,1)==0 )then
					call removeArray(obj%Mesh%NodCoord,remove1stColumn=.true.,NextOf=i-1)
					call removeArray(checklist        ,remove1stColumn=.true.,NextOf=i-1)
				else
					i=i+1
					cycle
				endif
				if(minval(checklist)==1 )then
					exit
				else
					cycle
				endif
			enddo

			

		endif	
	endif

end subroutine
!#######################################

subroutine getDBCVectorFEMDomain(obj,DBCvec)
	class(FEMDomain_),intent(in)::obj
	real(8),allocatable,intent(inout)::DBCvec(:,:)
	integer :: i,j,n,m,k,l
	n=size(obj%Mesh%NodCoord,1)
	m=size(obj%Mesh%NodCoord,2)
	if(.not. allocated(DBCvec ) )then
		allocate(DBCvec(n,m) )
		DBCvec(:,:)=0.0d0
	endif

	! check number of DBC
	do i=1,size(obj%Boundary%DBoundNum)
		k=countif(Array=obj%Boundary%DBoundNodID(:,i),Value=-1,notEqual=.true.)
		l=obj%Boundary%DBoundNum(i)
		if(k /= l)then
			print *, "Caution :: FiniteDeformationClass::getDBCVector :: check number of DBC :: k /= l"
		endif
	enddo

	do i=1,size(obj%Boundary%DBoundNodID,1)
		do j=1,size(obj%Boundary%DBoundNodID,2)
			if(obj%Boundary%DBoundNodID(i,j) <=0)then
				cycle
			endif
			DBCvec(obj%Boundary%DBoundNodID(i,j),j )=obj%Boundary%DBoundVal(i,j)
		enddo
	enddo


end subroutine
! ##################################################
end module FEMDomainClass