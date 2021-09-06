character*4::FileFormat
character*70::ProjectName
character*74 ::FileName
integer,allocatable::IntMat(:,:)
real(8),allocatable::RealMat(:,:)
integer,optional,intent(in)::FileHandle
integer :: fh,i,j,k,NumOfDomain,n,m,DimNum,GpNum
character*70 Msg


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

open(fh,file=FileName,status="replace")


if(trim(FileFormat)==".scf" )then

    NumOfDomain=size(obj%Mesh%SubMeshNodFromTo,1)
    write(fh,'(A)') obj%Dtype
    write(fh,*) "  "
    write(fh,'(A)') obj%SolverType
    write(fh,*) "  "
    write(fh,*) NumOfDomain
    write(fh,*) "  "
    !allocate(IntMat(NumOfDomain,2))
    !allocate(obj%Mesh%SubMeshNodFromTo(NumOfDomain,3) )
    !allocate(obj%Mesh%SubMeshElemFromTo(NumOfDomain,3) )
    
    do i=1,NumOfDomain
        write(fh,*) obj%Mesh%SubMeshNodFromTo(i,2),obj%Mesh%SubMeshNodFromTo(i,3)
    enddo
    write(fh,*) "  "
    do i=1,NumOfDomain
        write(fh,*) obj%Mesh%SubMeshElemFromTo(i,3)
    enddo
    write(fh,*) "  "


    n=size(obj%Mesh%NodCoord,1)
    m=size(obj%Mesh%NodCoord,2)
    write(fh,*) n,m
    DimNum=m

    write(fh,*) "  "
    !allocate(obj%Mesh%NodCoord(n,m) )
    do i=1,n
        write(fh,*) obj%Mesh%NodCoord(i,:)
    enddo
    write(fh,*) "  "

    n=size(obj%Mesh%ElemNod,1)
    m=size(obj%Mesh%ElemNod,2)
    write(fh,*) n,m
    write(fh,*) "  "
    write(fh,'(A)') trim(obj%Mesh%ElemType)
    write(fh,*) "  "
    do i=1,n
        write(fh,*) obj%Mesh%ElemNod(i,:)
    enddo
    write(fh,*) "  "

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

    !DirichletBoundary
    
    if(.not.allocated(obj%Boundary%DBoundNum))then
        
        write(fh,*) "0" !DirichletBoundaryDimension
        write(fh,*) "  "
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

    write(fh,*) obj%ControlPara%SimMode ,obj%ControlPara%ItrTol,obj%ControlPara%Timestep
     
    close(fh)
else
    print *, "ERROR :: ExportFEMDomain >> only .scf file can be exported."
endif



