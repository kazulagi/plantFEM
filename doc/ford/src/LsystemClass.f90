module LsystemClass
    use, intrinsic :: iso_fortran_env
    use KinematicClass
    !use PreprocessingClass
    use FEMDomainClass
    use StemClass
    use LeafClass
    use PetiClass
    use PodClass
    use FlowerClass
    use PlantRootClass
    use PlantNodeClass

    implicit none
    
    type :: NodeSystem_
        type(PlantNode_),allocatable :: NodeSystem(:)
        integer(int32) :: num_of_node
    contains
        !procedure :: export => exportNodeSystem
    end type

    type :: RootSystem_
        type(PlantRoot_),allocatable :: RootSystem(:)
        integer(int32) :: num_of_root
    contains
        !procedure :: export => exportRootSystem
    end type


    

    type :: Lsystem_
        type(Leaf_),allocatable::LeafList(:)
        type(Peti_),allocatable::PetiList(:)
        type(Flower_),allocatable::FlowerList(:)
        type(Stem_),allocatable::StemList(:)
        type(PlantRoot_),allocatable::RootList(:)
    contains
        procedure,public :: Init => InitLsystem 
    end type

contains






! ########################################
subroutine InitLsystem(obj,InObj,MaxSize)
    class(Lsystem_),intent(inout)::obj
    type(Lsystem_),optional,intent(in) ::InObj
    integer(int32),optional,intent(in)::MaxSize
    integer(int32) :: n

    ! Regacy version 


    n=input(default=100,option=MaxSize)
    allocate( obj%LeafList(n) )
    allocate( obj%PetiList(n) )
    allocate( obj%FlowerList(n) )
    allocate( obj%StemList(n) )
    allocate( obj%RootList(n) )

end subroutine
! ########################################

! ########################################
!subroutine exportNodeSystem(obj,FilePath,FileName,ObjID)
!    class(NodeSystem_),intent(in) :: obj
!    integer(int32),intent(inout) :: objID
!
!    ! export stem
!    character(*),optional,intent(in) :: FilePath,FileName
!    integer(int32) :: i
!    do i=1,size(obj%NodeSystem)
!
!        if(present(FileName) )then
!            call obj%NodeSystem(i)%export(FileName="PlantNode_"//trim(FileName),objID=ObjID)
!        elseif(present(FilePath) )then
!            call obj%NodeSystem(i)%export(FileName=trim(FilePath)//"/Node.geo",objID=objID)
!        else
!            call obj%NodeSystem(i)%export(FileName="/Node.geo",objID=objID)
!        endif
!        if(i==obj%num_of_node  )then
!            return
!        endif
!    enddo
!    
!
!
!end subroutine
! ########################################

! ########################################
!subroutine exportRootSystem(obj,FilePath,FileName,objID)
!    class(RootSystem_),intent(in) :: obj
!    character(*),optional,intent(in) :: FilePath,FileName
!    integer(int32),intent(inout) :: objID
!    integer(int32) :: i
!    do i=1,size(obj%RootSystem)
!
!        if(present(FileName) )then
!            call obj%RootSystem(i)%export(FileName="PlantRoot_"//trim(FileName),RootID=objID)
!        elseif(present(FilePath) )then
!            call obj%RootSystem(i)%export(FileName=trim(FilePath)//"/Root.geo",RootID=objID)
!        else
!            call obj%RootSystem(i)%export(FileName="/Root.geo",RootID=objID)
!        endif
!        if(i==obj%num_of_root  )then
!            return
!        endif
!    enddo
!end subroutine
! ########################################

end module LsystemClass