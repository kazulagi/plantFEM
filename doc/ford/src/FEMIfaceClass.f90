module FEMIfaceClass
    use, intrinsic :: iso_fortran_env
    use MathClass
    !use MPIClass
    use ArrayClass
    use ShapeFunctionClass
    use MeshClass
    use MaterialPropClass
    use BoundaryConditionClass
    use ControlParameterClass
    use FEMDomainClass

    implicit none

    type,extends(FEMDomain_) :: FEMDomainPointer_
        type(FEMDomain_),pointer :: FEMDomainp

    end type


    type :: FEMIface_
        type(ShapeFunction_)    :: ShapeFunction1,ShapeFunction2
        type(Mesh_)             :: Mesh1,Mesh2 ! Mesh[12]%ElemNod is a LOCAL node pointer, not a DIRECT pointer for each domains
        type(MaterialProp_)     :: MaterialProp
        type(ControlParameter_) :: ControlPara
        type(FEMDomainPointer_),allocatable :: FEMDomains(:)
        real(real64),allocatable     :: NTN_NodCoord(:,:)
        real(real64),allocatable     :: NTS_NodCoord(:,:)
        real(real64),allocatable     :: STS_NodCoord(:,:)
        real(real64),allocatable     :: NTN_Val(:,:)
        real(real64),allocatable     :: NTS_Val(:,:)
        real(real64),allocatable     :: STS_Val(:,:)
        integer(int32),allocatable     :: NTN_ElemNod(:,:)
        integer(int32),allocatable     :: NTS_ElemNod(:,:)
        integer(int32),allocatable     :: STS_ElemNod(:,:)
        integer(int32),allocatable     :: NTN_Active(:)
        integer(int32),allocatable     :: NTS_Active(:)
        integer(int32),allocatable     :: STS_Active(:)
        real(real64),allocatable     :: NTN_Value(:,:)
        real(real64),allocatable     :: NTS_Value(:,:)
        real(real64),allocatable     :: STS_Value(:,:)
        integer(int32),allocatable     :: NTS_SegmentID(:,:)
        integer(int32),allocatable     :: GloNodPoint1(:,:)
        integer(int32),allocatable     :: GloNodPoint2(:,:)
        integer(int32)                 :: DomainID1
        integer(int32)                 :: DomainID2
        integer(int32)                 :: DomainID3
        integer(int32)                 :: TimeStep
        integer(int32)                 :: NumOfImportedDomain
        character*200           :: FilePathDomain1
        character*200           :: FilePathDomain2
        character*200           :: FilePath

        character*200 :: FileNameDomain1
        character*200 :: FileNameDomain2
        character*200 :: FileName
        character*9   :: Dtype
        character*20  :: SolverType

        
    contains 
        procedure :: Init => InitializeFEMIface
        procedure :: setFEMDomain => setFEMDomainFEMIface 
        procedure :: Delete => DeallocateFEMIface
        procedure :: Import => ImportFEMIface
        procedure :: GetFEMIface => GetFEMIfaceFromFEMDomains
        procedure :: Export => ExportFEMIface
        procedure :: GmshPlotMesh => GmshPlotMeshFEMIface
        procedure :: GmshPlotNTS  => GmshPlotNTSFEMIface
        procedure :: GetNTNelement => GetNTNelement
        procedure :: GetNTSelement => GetNTSelement
        procedure :: GetGlobalNodePointer => GetGlobalNodePointerNTS
        procedure :: updateTimestep => updateTimestepIface
    end type
contains


! #########################################################
subroutine InitializeFEMIface(obj,NumOfDomain)
    class(FEMIface_),intent(inout)::obj
    integer(int32),optional,intent(in)::NumOfDomain
    integer(int32) :: i

    call obj%Delete()

    if(allocated(obj%FEMDomains) ) deallocate(obj%FEMDomains)
    
    
    if(allocated(obj%FEMDomains) ) deallocate(obj%FEMDomains)
    if(present(NumOfDomain) )then
        allocate(obj%FEMDomains(NumOfDomain) )
    else
        allocate(obj%FEMDomains(2) )
    endif
    obj%NumOfImportedDomain = 0
    obj%TimeStep = 0


end subroutine
! #########################################################


! #########################################################
subroutine setFEMDomainFEMIface(obj,dobj,Name)
    class(FEMIface_),intent(inout)::obj
    class(FEMDomain_),target,intent(in)::dobj
    character(*),optional,intent(in) :: Name


    obj%NumOfImportedDomain=obj%NumOfImportedDomain+1
    if(size(obj%FEMDomains,1) < obj%NumOfImportedDomain )then
        print *, "ERROR :: FEMIFace >> setFEMDomainFEMIface :: size(obj%FEMDomains,1) < obj%NumOfImportedDomain "
    else

        obj%FEMDomains(obj%NumOfImportedDomain)%FEMDomainp => dobj
        
        if(present(Name))then
            obj%FEMDomains(obj%NumOfImportedDomain)%FEMDomainp%FileName = Name
        endif
    endif

end subroutine
! #########################################################



! #########################################################
subroutine GmshPlotMeshFEMIface(obj,Name,withNeumannBC,withDirichletBC)
    class(FEMIface_),intent(inout)::obj
    character(*),optional,intent(in) :: Name
    logical,optional,intent(in)::withNeumannBC,withDirichletBC
    integer(int32) :: i

    do i=1,size(obj%FEMDomains,1)
        if(present(Name) )then
            call obj%FEMDomains(i)%FEMDomainp%GmshPlotMesh(Name=trim(Name)//trim( adjustl(fstring(i)) ) ,&
                withNeumannBC=withNeumannBC,withDirichletBC=withDirichletBC)
        else
            call obj%FEMDomains(i)%FEMDomainp%GmshPlotMesh(Name=trim( adjustl(fstring(i)) ) ,&
                withNeumannBC=withNeumannBC,withDirichletBC=withDirichletBC)
        endif
    enddo
    if(present(Name) )then
        call obj%GmshPlotNTS(Name=trim(Name)//"interface")
    else
        call obj%GmshPlotNTS(Name="interface")
    endif


end subroutine
! #########################################################


! #########################################################
subroutine DeallocateFEMIface(obj)
    class(FEMIface_),intent(inout)::obj

    
    if(allocated(obj%NTN_NodCoord) ) then
        deallocate(obj%NTN_NodCoord)
    else
    endif

    if(allocated(obj%NTS_NodCoord) )then
        deallocate(obj%NTS_NodCoord)
    endif
    if(allocated(obj%STS_NodCoord) )then
        deallocate(obj%STS_NodCoord)
    endif
    if(allocated(obj%NTN_ElemNod) )then
        deallocate(obj%NTN_ElemNod)
    endif
    if(allocated(obj%NTS_ElemNod) )then
        deallocate(obj%NTS_ElemNod)
    endif
    if(allocated(obj%STS_ElemNod) )then
        deallocate(obj%STS_ElemNod)
    endif


    if(allocated(obj%FEMDomains) )then
        deallocate(obj%FEMDomains)
    endif


end subroutine 
! #########################################################


! #########################################################
subroutine ImportFEMIface(obj,OptionalFileFormat,OptionalProjectName,FileHandle)
    class(FEMIface_),intent(inout)::obj
    character*4,optional,intent(in)::OptionalFileFormat
    character*70,optional,intent(in)::OptionalProjectName

    character*4::FileFormat
    character*70::ProjectName
    character*74 ::FileName
    character*9  :: DataType
    integer(int32),allocatable::IntMat(:,:)
    real(real64),allocatable::RealMat(:,:)
    integer(int32),optional,intent(in)::FileHandle
    integer(int32) :: fh,i,j,k,NumOfDomain,n,m,DimNum,GpNum,ierr
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

    obj%FileName=ProjectName
    FileName = trim(ProjectName)//trim(FileFormat)


    open(fh,file=FileName,status="old")

    if(trim(FileFormat)==".scf" )then


        read(fh,*) DataType 

        if(trim(DataType)/="interface" .and. trim(DataType)/=" interface" )then
            return
        endif

        obj%Dtype=trim(DataType)
        read(fh,*) obj%FileNameDomain1
        read(fh,*) obj%FileNameDomain2
        read(fh,*) obj%SolverType
        
    endif
    close(fh)


end subroutine
! #########################################################




!###################### Get FEM Interfaces ##########################
subroutine GetFEMIfaceFromFEMDomains(obj,obj1,obj2,MasterID,SlaveID)
    class(FEMDomain_),optional,intent(inout)::obj1,obj2
    class(FEMIface_),intent(inout)::obj
    integer(int32),optional,intent(in)  ::MasterID,SlaveID

    integer(int32) :: i,j,n1,ierr,err

    if(.not. present(obj1) .and. .not. present(obj2) )then
        call GetFEMIfaceFromPointer(obj,MasterID,SlaveID)
        return
    endif

    print *, "object names #1 : ",trim(obj1%FileName)
    print *, "object names #2 : ",trim(obj2%FileName)
    


    
    call GetInterface(obj1%Mesh,obj2%Mesh,obj%Mesh1,obj%Mesh2,err) ! AABB algorithm
    

    if(err==1)then
        print *, "no contact"
        return
    endif
    n1 = index(trim(obj1%FileName),".scf", back=.true. )
    obj%FilePathDomain1=trim(obj1%FilePath)
    obj%FilePathDomain2=trim(obj2%FilePath)
    obj%FilePath       =trim(obj1%FilePath)
    obj%FileNameDomain1=trim(obj1%FileName)
    obj%FileNameDomain2=trim(obj2%FileName)
    obj%FileName       ="Iface_"//obj1%FileName(1:n1-1)//"_"//trim(obj2%FileName)

    call obj%GetNTNelement()
    call obj%GetNTSelement()
    
    call ShowArray(obj%NTN_NodCoord,FileHandle=10)
    call ShowArray(obj%NTS_NodCoord,FileHandle=20)

end subroutine 
!###################### Get FEM Interfaces ##########################






!###################### Get FEM Interfaces ##########################
subroutine GetFEMIfaceFromPointer(obj,MasterID,SlaveID)
    class(FEMIface_),intent(inout)::obj
    class(FEMDomain_),pointer::obj1,obj2
    integer(int32),optional,intent(in) :: MasterID,SlaveID

    integer(int32) :: i,j,n1,ierr,err

    if(present(MasterID) )then
        i=MasterID
    else
        i=1
    endif

    if(present(SlaveID) )then
        j=SlaveID
    else
        j=2
    endif

    obj1 => obj%FEMDomains(i)%FEMDomainp
    obj2 => obj%FEMDomains(j)%FEMDomainp
    
    print *, "object names #1 : ",trim(obj1%FileName)
    print *, "object names #2 : ",trim(obj2%FileName)
    


    
    call GetInterface(obj1%Mesh,obj2%Mesh,obj%Mesh1,obj%Mesh2,err) ! AABB algorithm
    call obj%GetGlobalNodePointer()

    !if(err==1)then
    !    print *, "no contact"
    !    return
    !endif
    !n1 = index(trim(obj1%FileName),".scf", back=.true. )
    !obj%FilePathDomain1=trim(obj1%FilePath)
    !obj%FilePathDomain2=trim(obj2%FilePath)
    !obj%FilePath       =trim(obj1%FilePath)
    !obj%FileNameDomain1=trim(obj1%FileName)
    !obj%FileNameDomain2=trim(obj2%FileName)
    !obj%FileName       ="Iface_"//obj1%FileName(1:n1-1)//"_"//trim(obj2%FileName)

    call obj%GetNTNelement()
    call obj%GetNTSelement()
    
    call ShowArray(obj%NTN_NodCoord,FileHandle=10)
    call ShowArray(obj%NTS_NodCoord,FileHandle=20)

end subroutine 
!###################### Get FEM Interfaces ##########################



!###################### Get Node-To-Node Elements ##########################
subroutine GetNTNelement(obj)
    class(FEMIface_),intent(inout)::obj

    real(real64),allocatable::x(:),xn(:)
    real(real64) :: dist
    integer(int32) :: node_num1,dim_num1,node_num2,dim_num2,i,j,n,node_num
    integer(int32) :: master,dim_num,id

    node_num1=size(obj%Mesh1%NodCoord,1)
    dim_num1 =size(obj%Mesh1%NodCoord,2)

    node_num2=size(obj%Mesh2%NodCoord,1)
    dim_num2 =size(obj%Mesh2%NodCoord,2)

    if(dim_num1/=dim_num2)then
        stop "ERROR :: GetNTNelement dimension of domain1 and domain2 is not consistent"
    endif
    dim_num=dim_num1

    allocate(x(dim_num1) )
    allocate(xn(dim_num1) )
    if(node_num1>=node_num2)then
        node_num=node_num2
        master = 2
    else
        node_num=node_num1
        master = 1
    endif

    if(allocated(obj%NTN_NodCoord) ) deallocate(obj%NTN_NodCoord)
    if(allocated(obj%NTN_ElemNod) ) deallocate(obj%NTN_ElemNod)
    if(allocated(obj%NTN_Val) ) deallocate(obj%NTN_Val)
    
    allocate(obj%NTN_NodCoord(node_num,dim_num*2 ) ) !In terms of local IDs
    allocate(obj%NTN_ElemNod(node_num,2 ) ) !In terms of local IDs
    allocate(obj%NTN_Val(node_num,2 ) ) !In terms of local IDs

    do i=1,node_num
        if(master==1)then ! domain 1 is master >> search the pair from domain 2
            x(:)=obj%Mesh1%NodCoord(i,:)
            id = SearchNearestCoord(obj%Mesh2%NodCoord,x)
            obj%NTN_ElemNod(i,1)=i  
            obj%NTN_ElemNod(i,2)=id    
            xn(:)=obj%Mesh2%NodCoord(id,:)
            obj%NTN_Val(i,1)=dsqrt(dot_product(x-xn,x-xn))
            obj%NTN_NodCoord(i,1:dim_num)=x(:)
            obj%NTN_NodCoord(i,dim_num+1:2*dim_num)=xn(:)
        else ! domain 1 is master >> search the pair from domain 2
            x(:)=obj%Mesh2%NodCoord(i,:)
            id = SearchNearestCoord(obj%Mesh1%NodCoord,x)
            obj%NTN_ElemNod(i,2)=i  
            obj%NTN_ElemNod(i,1)=id    
            xn(:)=obj%Mesh1%NodCoord(id,:)
            obj%NTN_Val(i,1)=dsqrt(dot_product(x-xn,x-xn))
            obj%NTN_NodCoord(i,1:dim_num)=xn(:)
            obj%NTN_NodCoord(i,dim_num+1:2*dim_num)=x(:)
        endif
    enddo


end subroutine
!###################### Get Node-To-Node Elements ##########################


!###################### Get Node-To-Segment Elements ##########################
subroutine GetNTSelement(obj)
    class(FEMIface_),intent(inout)::obj

    real(real64),allocatable::x(:),xn(:),ElemMidPointCoord(:,:)
    real(real64) :: dist
    integer(int32) :: node_num1,dim_num1,node_num2,dim_num2,i,j,n,node_num,elem_num2,elem_num
    integer(int32) :: slave,dim_num,id,elemnod_num2,elemnod_num


    node_num1=size(obj%Mesh1%NodCoord,1)
    dim_num1 =size(obj%Mesh1%NodCoord,2)

    dim_num2=size(obj%Mesh2%NodCoord,2)
    elem_num2=size(obj%Mesh2%ElemNod,1)
    elemnod_num2 =size(obj%Mesh2%ElemNod,2)

    if(dim_num1/=dim_num2)then
        stop "ERROR :: GetNTSelement dimension of domain1 and domain2 is not consistent"
    endif
    dim_num=dim_num1 
    node_num=node_num1 ! slave node
    elem_num=elem_num2 ! master segment
    elemnod_num=elemnod_num2 ! master segment
    allocate(x(dim_num1) )
    allocate(xn(dim_num1) )
    allocate(ElemMidPointCoord(size(obj%Mesh2%ElemNod,1),dim_num ))
    do i=1,elem_num
        xn(:)=0.0d0
        do j=1,elemnod_num
            xn(:)=xn(:)+1.0d0/dble(elemnod_num)*obj%Mesh2%NodCoord(obj%Mesh2%ElemNod(i,j),:  )
        enddo
        ElemMidPointCoord(i,:)=xn(:)
    enddo

    if(allocated(obj%NTS_NodCoord) ) deallocate(obj%NTS_NodCoord)
    if(allocated(obj%NTS_ElemNod) ) deallocate(obj%NTS_ElemNod)
    if(allocated(obj%NTS_Val) ) deallocate(obj%NTS_Val)
    if(allocated(obj%NTS_SegmentID) ) deallocate(obj%NTS_SegmentID)
    

    allocate(obj%NTS_NodCoord(node_num,dim_num*(1+elemnod_num)   ) ) !In terms of local IDs
    allocate(obj%NTS_ElemNod(node_num,1+elemnod_num ) ) !In terms of local IDs
    allocate(obj%NTS_Val(node_num,2 ) ) !In terms of local IDs
    allocate(obj%NTS_SegmentID(node_num,1 ) )


    obj%NTS_Val(:,:)=0.0d0
    do i=1,node_num
        x(:)=obj%Mesh1%NodCoord(i,:)
        id = SearchNearestCoord(ElemMidPointCoord,x)
        obj%NTS_ElemNod(i,1)=i
        obj%NTS_SegmentID(i,1)=id
        obj%NTS_NodCoord(i,1:dim_num)=x(:)
        
        do j=1,elemnod_num
            obj%NTS_ElemNod(i,1+j)=obj%Mesh2%ElemNod(id,j)
            obj%NTS_NodCoord(i, dim_num*(j)+1 :dim_num*(j+1) ) = &
            obj%Mesh2%NodCoord( obj%Mesh2%ElemNod(id,j),:   )
        enddo
        obj%NTS_NodCoord(i,1:dim_num)=x(:)
        
    enddo



end subroutine
!###################### Get Node-To-Segment Elements ##########################


!###################### Get Segment-To-Segment Elements ##########################
subroutine GetSTSelement(obj)
    class(FEMIface_),intent(inout)::obj

    print *, "now, developping"
end subroutine
!###################### Get Segment-To-Segment Elements ##########################


! #########################################################
subroutine ExportFEMIface(obj,OptionalFileFormat,OptionalProjectName,FileHandle)
    class(FEMIface_),intent(inout)::obj
    character*4,optional,intent(in)::OptionalFileFormat
    character*70,optional,intent(in)::OptionalProjectName

    character*4::FileFormat
    character*70::ProjectName
    character*74 ::FileName
    character*9  :: DataType
    integer(int32),allocatable::IntMat(:,:)
    real(real64),allocatable::RealMat(:,:)
    integer(int32),optional,intent(in)::FileHandle
    integer(int32) :: fh,i,j,k,NumOfDomain,n,m,DimNum,GpNum
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

    open(fh,file=FileName,status="replace")


    if(trim(FileFormat)==".scf" )then


        if(trim(obj%Dtype)/="interface")then
            return
        endif
        write(fh,'(A)') trim(obj%Dtype)  
        write(fh,'(A)') trim(obj%FileNameDomain1)
        write(fh,'(A)') trim(obj%FileNameDomain2)
        write(fh,'(A)') trim(obj%SolverType)
        
        
    endif
    close(fh)

end subroutine
! #########################################################




! #########################################################
subroutine GmshPlotNTSFEMIface(obj,Name)
    class(FEMIFace_),intent(in)::obj
    type(FEMDomain_) :: Fobj
    integer(int32) :: i,j,n,m,dim_num,k,elemnodnum,step
    character(*),optional,intent(in)::Name
    

    ! Visualize NTS elements

    step=obj%TimeStep
    
    dim_num=size(obj%FEMDomains(1)%FEMDomainp%Mesh%NodCoord,2)
    print *, dim_num

    n=size(obj%NTS_ElemNod,1)
    m=size(obj%NTS_ElemNod,2)
    

    if(dim_num==2)then
        ! 4 
        elemnodnum=4
    elseif(dim_num==3)then
        ! 8
        elemnodnum=8
    else
        print *, "ERROR dim_num of NTS_NodCoord is ",dim_num
        return
    endif


    allocate(Fobj%Mesh%NodCoord(n*m,dim_num) )
    allocate(Fobj%Mesh%ElemNod(n,elemnodnum) )
    allocate(Fobj%Mesh%ElemMat(n) )
    Fobj%FileName=obj%FileName
    Fobj%Mesh%ElemMat(:)=1
    Fobj%Mesh%NodCoord(:,:)=0.0d0
    Fobj%Mesh%ElemNod(:,:)=0

    ! create Fobj%Mesh%NodCoord
    k=0
    do i=1,n ! number of NTS elements
        do j=1,m ! over nodes in a NTS elements
            
            k=k+1 ! ID
            if(elemnodnum==4)then
                Fobj%Mesh%NodCoord(k,1:2)=obj%NTS_NodCoord( i , dim_num*(j-1)+1:dim_num*(j)  ) ! need revision
            elseif(elemnodnum==8)then
                if(j==1)then
                    Fobj%Mesh%ElemNod(i,j+elemnodnum/2)=k
                else
                    Fobj%Mesh%ElemNod(i,j-1)=k
                    if(j-1 /= 1)then
                        Fobj%Mesh%ElemNod(i,j-1+elemnodnum/2)=k
                    endif
                endif
                Fobj%Mesh%NodCoord(k,1:3)=obj%NTS_NodCoord(i, dim_num*(j-1)+1:dim_num*(j) )
            else
                print *, "ERROR2 dim_num of NTS_NodCoord is ",dim_num
                return
            endif
        enddo
    enddo

    call Fobj%GmshPlotMesh(Name=Name,OptionalStep=step)


end subroutine
! #########################################################



! #########################################################
subroutine GetGlobalNodePointerNTS(obj)
    class(FEMIface_)::obj
    !type(MPI_)::mpidata
    integer(int32) :: i,j,k,n,m,NumElemIface1,NumElemIface2
    real(real64),allocatable :: x(:),x_tr(:)

    ! get GlobalNodePointer of NTS element
    NumElemIface1=size(obj%Mesh1%ElemNod,1)
    NumElemIface2=size(obj%Mesh2%ElemNod,1)
    n=size(obj%Mesh1%ElemNod,2)
    m=size(obj%Mesh2%ElemNod,2)
    if(allocated(obj%GloNodPoint1) )then
        deallocate(obj%GloNodPoint1)
    endif
    if(allocated(obj%GloNodPoint2) )then
        deallocate(obj%GloNodPoint2)
    endif
    allocate(obj%GloNodPoint1(NumElemIface1,n),obj%GloNodPoint2(NumElemIface2,m))

    ! for Domain1
    do i=1,NumElemIface1
        do j=1,n
            !print *, obj%Mesh1%GlobalNodID(obj%Mesh1%ElemNod(i,j) ),"/",size(obj%FEMDomains(1)%FEMDomainp%Mesh%NodCoord,1)
            obj%GloNodPoint1(i,j)=obj%Mesh1%GlobalNodID(obj%Mesh1%ElemNod(i,j) )
        enddo
    enddo
!
!
    !! for Domain2
    do i=1,NumElemIface2
        do j=1,m
            !print *, obj%Mesh2%GlobalNodID(obj%Mesh2%ElemNod(i,j) ),"/",size(obj%FEMDomains(2)%FEMDomainp%Mesh%NodCoord,1)
            obj%GloNodPoint2(i,j)=obj%Mesh2%GlobalNodID(obj%Mesh2%ElemNod(i,j) )
        enddo
    enddo



end subroutine
! #########################################################


! #########################################################
subroutine updateTimestepIface(obj,timestep)
    class(FEMIFace_),intent(inout)::obj
    integer(int32),optional,intent(in)::timestep
    integer(int32) :: dt

    dt=input(default=1,option=timestep)
    obj%TimeStep=obj%TimeStep+dt

end subroutine
! #########################################################




end module