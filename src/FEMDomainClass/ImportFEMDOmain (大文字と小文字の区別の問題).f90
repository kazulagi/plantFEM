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
    allocate(obj%Mesh%ElemNod(n,m) )
    allocate(obj%Mesh%ElemMat(n  ) )
    call ImportArray(obj%Mesh%ElemNod,OptionalFileHandle=fh)
    do i=1,n
        read(fh,*) obj%Mesh%ElemMat(i)
    enddo

    read(fh,*) n,m
    allocate(obj%MaterialProp%MatPara(n,m) )
    call ImportArray(obj%MaterialProp%MatPara,OptionalFileHandle=fh)
    

    allocate(obj%Boundary%DBoundNum(DimNum ))
    read(fh,*) obj%Boundary%DBoundNum(:)
    allocate(obj%Boundary%DBoundNodID( maxval(obj%Boundary%DBoundNum), size(obj%Boundary%DBoundNum)  )  )
    allocate(obj%Boundary%DBoundVal( maxval(obj%Boundary%DBoundNum), size(obj%Boundary%DBoundNum)  )  )

    obj%Boundary%DBoundNodID(:,:)=-1
    obj%Boundary%DBoundVal(:,:)  =0.0d0

    do i=1,size(obj%Boundary%DBoundNum,1)
        do j=1,obj%Boundary%DBoundNum(i)
            read(fh,*) obj%Boundary%DBoundNodID(j,i)
            !!print *,obj%Boundary%DBoundNodID(j,i)
        enddo
        do j=1,obj%Boundary%DBoundNum(i)
            read(fh,*) obj%Boundary%DBoundVal(j,i)
            !!print *,obj%Boundary%DBoundVal(j,i)
        enddo
    enddo

    read(fh,*) n
    !!print *,n

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
    read(fh,*) n
    allocate(obj%Mesh%SurfaceLine2D(n) )

    allocate(obj%Mesh%SubMeshSurfFromTo(NumOfDomain,3) )
    do i=1,n
        read(fh,*) obj%Mesh%SurfaceLine2D(i)
    enddo

    do i=1,NumOfDomain
        obj%Mesh%SubMeshSurfFromTo(i,1)=i
        read(fh,*) obj%Mesh%SubMeshSurfFromTo(i,2:3)
    enddo

    
    read(fh,*) obj%ControlPara%Tol ,obj%ControlPara%Tol
    read(fh, *) n,m
    read(fh, *) n,m,m
    read(fh,*) Msg
    read(fh,*) obj%ControlPara%SimMode ,obj%ControlPara%ItrTol,obj%ControlPara%Timestep
    close(fh)
else
    !!print *, "ERROR :: ExportFEMDomain >> only .scf file can be exported."
endif



