module FieldClass
    use, intrinsic :: iso_fortran_env
    use FEMDomainClass
    use FEMIfaceClass
    use DictionaryClass
    use DiffusionEquationClass
    use FiniteDeformationClass
    
    implicit none

    type :: FieldObjName_
        character*200  :: FieldObjName
    end type

    type :: Field_
        type(FEMDomain_),allocatable::FEMDomainArray(:)
        type(FEMIface_) ,allocatable::FEMIfaceArray(:)
        type(FieldObjName_),allocatable::FieldList(:)
        integer(int32),allocatable::Timestep(:)
        real(real64),allocatable::RealTime(:)
        integer(int32) :: NumberOfObject,NumberOfIface

        character*200 :: FolderName
        character*200 :: DomainListName
        character*200 :: IfaceListName
    contains
        procedure :: Import => ImportField
        procedure :: show   => showField
        procedure :: Export => ExportField
        procedure :: Shift  => ShiftField

        procedure :: linkDomainToIface => linkDomainToIfaceField
    end type

contains
!###################################################
subroutine ImportField(obj,OptionalDomainListName,OptionalIfaceListName,OptionalProjectName,OptionalFileHandle)
    class(Field_),target,intent(inout)::obj
    character(*),optional,intent(in)::OptionalDomainListName
    character(*),optional,intent(in)::OptionalIfaceListName
    character(*),optional,intent(in)::OptionalProjectName
    integer(int32),optional,intent(in)::OptionalFileHandle

    character*200::pathd,pathi,line
    integer(int32) :: fh,fh2,i,fh_i,ierr
    if(present(OptionalFileHandle) )then
        fh=OptionalFileHandle
        fh2=fh+1
        fh_i=fh+200
    else
        fh=100
        fh2=200
        fh_i=400
    endif

    if(present(OptionalProjectName) )then
        obj%FolderName=trim(OptionalProjectName)//"/"
    else
        obj%FolderName=""
        print *, "Current Directory is used as Project Directory."
    endif

    if(present(OptionalDomainListName) )then
        obj%DomainListName=trim(OptionalDomainListName)
    else
        obj%DomainListName="DomainList.txt"
        print *, "Current Directory is used as Project Directory."
    endif

    if(present(OptionalIfaceListName) )then
        obj%IfaceListName=trim(OptionalIfaceListName)
    else
        obj%IfaceListName="IfaceList.txt"
        print *, "Current Directory is used as Project Directory."
    endif

    pathd=trim(obj%FolderName)//trim(obj%DomainListName)
    pathi=trim(obj%FolderName)//trim(obj%IfaceListName)

    open(fh,file=trim(pathd),status="old")
    read(fh,*) obj%NumberOfObject
    print *, "Domainlist : ",trim(pathd)," is imported."
    print *, "Number of Domains    : ",obj%NumberOfObject," is imported."
    
    open(fh_i,file=trim(pathi),status="old")
    read(fh_i,*) obj%NumberOfIface
    print *, "Ifacelist : ",trim(pathd)," is imported."
    print *, "Number of Iface     : ",obj%NumberOfIface," is imported."
    
    



    allocate(obj%FieldList(obj%NumberOfObject + obj%NumberOfIface) )
    allocate(obj%FEMDomainArray( obj%NumberOfObject ))
    allocate(obj%FEMIfaceArray( obj%NumberOfIface ))

    ! ###### Domain ############
    do i=1,obj%NumberOfObject
        read(fh, '(A)' ) pathd

        obj%FieldList(i)%FieldObjName=trim(pathd)
        !read(fh,*) obj%FieldList(i)%FieldObjName
        !pathd=trim(obj%FolderName)//trim(obj%FieldList(i)%FieldObjName )

        !call removeWord(str=pathd,keyword=".scf",itr=1)
        print *, "Now Importing Domain id : ",i,"File path : ",trim(pathd),".scf"
        call ImportFEMDomain(obj%FEMDomainArray(i),OptionalProjectName=pathd,FileHandle=fh2)
        obj%FEMDomainArray(i)%FileName=trim(obj%FieldList(i)%FieldObjName)
        
    enddo
    ! ###### Domain ############

    ! ###### Interface ############
    do i=obj%NumberOfObject+1,obj%NumberOfObject+obj%NumberOfIface
        read(fh_i, '(A)' ) obj%FieldList(i)%FieldObjName
        pathi=trim(obj%FolderName)//trim(obj%FieldList(i)%FieldObjName )
        
        !call removeWord(str=pathi,keyword=".scf",itr=1)
        call ImportFEMIface(obj%FEMIfaceArray(i-obj%NumberOfObject),OptionalProjectName=pathi,FileHandle=fh2)
        print *, "Imported ",trim(pathi),".scf  >> IFace ID :",i-obj%NumberOfObject,"/",obj%NumberOfIface
        
    enddo
    ! ###### Interface ############
    

    ! ###### Linkage between FEMIface and FEMDomain ######
    call obj%linkDomainToIface()
    ! ###### Linkage between FEMIface and FEMDomain ######
    
    close(fh_i)
    close(fh)

end subroutine
!###################################################



!###################################################
subroutine showField(obj)
    class(Field_),intent(inout)::obj
    integer(int32) :: i,n,m

    n=size(obj%FEMDomainArray)
    m=size(obj%FEMIfaceArray)
    print *, "==========-----------=========="
    print *, "FieldClass >> Field Information"
    print *, "==========-----------=========="
    print *, "Number of domain >> ",n
    print *, "==========-----------=========="
    do i=1,n
        print *, "Domain ID",i,":",obj%FEMDomainArray(i)%FileName
    enddo 
    print *, "==========-----------=========="
    print *, "Number of domain >> ",m
    print *, "==========-----------=========="
    do i=1,m
        print *, "Domain ID",i,":",obj%FEMIfaceArray(i)%FileName
    enddo 
    print *, "==========-----------=========="

end subroutine
!###################################################



!###################################################
subroutine linkDomainToIfaceField(obj)
    class(Field_),target,intent(inout)::obj
    type(Dictionary_)::DomainNameList
    integer(int32) :: i,j,n,m,page1,page2

    n=size(obj%FEMIfaceArray,1)
    m=size(obj%FEMDomainArray,1)
    call DomainNameList%init(m)

    do i=1,m
        call DomainNameList%input(page=i,content=obj%FEMDomainArray(i)%FileName)
    enddo

    do i=1,n
        page1=DomainNameList%GetPageNum(obj%FEMIfaceArray(i)%FileNameDomain1)
        page2=DomainNameList%GetPageNum(obj%FEMIfaceArray(i)%FileNameDomain2)
        
        call obj%FEMIfaceArray(i)%init(NumOfDomain=2)

    

        obj%FEMIfaceArray(i)%FEMDomains(1)%FEMDomainp => obj%FEMDomainArray(page1)
        obj%FEMIfaceArray(i)%FEMDomains(2)%FEMDomainp => obj%FEMDomainArray(page2)

        !print *, "Domain 1 =>",obj%FEMIfaceArray(i)%FEMDomains(1)%FEMDomainp%FileName
        !print *, "Domain 2 =>",obj%FEMIfaceArray(i)%FEMDomains(2)%FEMDomainp%FileName

    enddo

end subroutine
!###################################################


!###################################################
subroutine ShiftField(obj,distance,Optionaldirection)
    class(field_),intent(inout)::obj
    integer(int32),optional,intent(in)::Optionaldirection
    real(real64),intent(in) :: distance
    integer(int32) :: i,n,dir

    if(present(Optionaldirection) )then
        dir=Optionaldirection
    else
        dir=1
    endif

    n=size(obj%FEMDomainArray,1)
    do i=1,n
        obj%FEMDomainArray(i)%Mesh%NodCoord(:,dir)=&
        obj%FEMDomainArray(i)%Mesh%NodCoord(:,dir)+distance
        obj%FEMDomainArray(i)%Mesh%NodCoordInit(:,dir)=&
        obj%FEMDomainArray(i)%Mesh%NodCoordInit(:,dir)+distance
    enddo
end subroutine
!###################################################



!###################################################
subroutine ExportField(obj,OptionalDomainListName,OptionalIfaceListName,&
    OptionalProjectName,OptionalFileHandle)
    class(Field_),target,intent(inout)::obj
    character*200,optional,intent(in)::OptionalDomainListName
    character*200,optional,intent(in)::OptionalIfaceListName
    character*200,optional,intent(in)::OptionalProjectName
    integer(int32),optional,intent(in)::OptionalFileHandle
    
    character*200::pathd,pathi
    integer(int32) :: fh,fh2,i,fh_i


    if(present(OptionalFileHandle) )then
        fh=OptionalFileHandle
        fh2=fh+1
        fh_i=fh+200
    else
        fh=100
        fh2=200
        fh_i=400
    endif

    if(present(OptionalProjectName) )then
        obj%FolderName=trim(OptionalProjectName)//"/"
    else
        obj%FolderName=""
        print *, "Current Directory is used as Project Directory."
    endif

    if(present(OptionalDomainListName) )then
        obj%DomainListName=trim(OptionalDomainListName)
    else
        obj%DomainListName="DomainList.txt"
        print *, "Current Directory is used as Project Directory."
    endif

    if(present(OptionalIfaceListName) )then
        obj%IfaceListName=trim(OptionalIfaceListName)
    else
        obj%IfaceListName="IfaceList.txt"
        print *, "Current Directory is used as Project Directory."
    endif


    pathd=trim(obj%FolderName)//trim(obj%DomainListName)
    pathi=trim(obj%FolderName)//trim(obj%IfaceListName)

    open(fh,file=trim(pathd),status="replace")
    open(fh_i,file=trim(pathi),status="replace")
    write(fh,*) size(obj%FEMDomainArray,1)
    write(fh_i,*) size(obj%FEMIfaceArray,1)
    
    do i=1,obj%NumberOfObject
        write(fh,'(A)') trim(obj%FieldList(i)%FieldObjName)
        pathd=trim(obj%FolderName)//trim(obj%FieldList(i)%FieldObjName )
        print *, "Now Exporting ",trim(pathd),".scf"
        call ExportFEMDomain(obj%FEMDomainArray(i),OptionalProjectName=pathd,FileHandle=fh2)
    enddo
    

    
    do i=obj%NumberOfObject+1,obj%NumberOfObject+obj%NumberOfIface
        write(fh_i,'(A)') trim(obj%FieldList(i)%FieldObjName)
        
        pathi=trim(obj%FolderName)//trim(obj%FieldList(i)%FieldObjName )
        
        print *, "Now Exporting ",trim(pathi),".scf"
        call ExportFEMIface(obj%FEMIfaceArray(i-obj%NumberOfObject),OptionalProjectName=pathi,FileHandle=fh2)
    enddo
    close(fh)

end subroutine
!###################################################


end module