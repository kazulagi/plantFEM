module BoundaryConditionClass
    use, intrinsic :: iso_fortran_env
    use std
    use ArrayClass
    use MeshClass

    implicit none

    type :: ContactName_
        character(200) :: name
    end type

    type::Boundary_
        ! new data structure
        type(Mesh_) :: DBound
        type(Mesh_) :: NBound
        type(Mesh_) :: TBound
        real(real64),allocatable:: DBoundPara(:,:)
        real(real64),allocatable:: NBoundPara(:,:)
        real(real64),allocatable:: TBoundPara(:,:)
        
        ! classic data structure
        real(real64),allocatable::DBoundVal(:,:)
        real(real64),allocatable::NBoundVal(:,:)  
        real(real64),allocatable::TBoundVal(:,:)  
        real(real64),allocatable::TBoundElemGpVal(:,:,:)      
        
        real(real64),allocatable::DBoundValInc(:,:)
        real(real64),allocatable::NBoundValInc(:,:)  
        real(real64),allocatable::TBoundValInc(:,:)

        integer(int32),allocatable::DBoundNodID(:,:)
        integer(int32),allocatable::NBoundNodID(:,:)
        integer(int32),allocatable::TBoundNodID(:,:)
        integer(int32),allocatable::TBoundElemID(:)

        integer(int32),allocatable::DBoundNum(:)
        integer(int32),allocatable::NBoundNum(:)
        integer(int32),allocatable::TBoundNum(:)
        integer(int32),allocatable::TBoundElemNum(:)

        ! contact boundary
        type(ContactName_),allocatable :: ContactNameList(:)
        integer(int32),allocatable:: MasterNodeID(:,:)
        integer(int32),allocatable:: SlaveNodeID(:,:)
        integer(int32),allocatable:: MasterSegment(:,:)
        integer(int32),allocatable:: SlaveSegment(:,:)

        real(real64) ::x_max,x_min,y_max,y_min,z_max,z_min,t_max,t_min
        integer(int32) :: Dcount,Ncount,Tcount
        integer(int32) :: layer
        character*200 :: Name= " "
        character*70 :: ErrorMsg= " "
    contains
        procedure :: AddMasterNode => AddMasterNodeBoundary
        procedure :: Init => InitializeBoundary
        procedure :: Delete => DeallocateBoundary
        procedure :: CheckDataType => CheckDatatypeBoundary
        procedure :: RemoveOverlap => DeleteOverlapBoundary
        procedure :: ImportDBound => ImportDBound
        procedure :: ImportNBound => ImportNBound
        procedure :: MergeDBound => MergeDBound
        procedure :: MergeNBound => MergeNBound
        procedure :: remove => removeBoudary
        procedure :: removeDBC => removeDBCBoundary        
        procedure :: removeNBC => removeNBCBoundary
        procedure :: removeTBC => removeTBCBoundary        
        procedure :: save => saveBoundary
        procedure :: setDB     => setDBoundary
        procedure :: setNB     => setNBoundary
        procedure :: setTB     => setTBoundary
        procedure :: gmsh    => gmshBoundary
        procedure :: create => createBoundary
        procedure :: show => showBoundary
        procedure :: export => exportBoundary
        procedure :: open => openBoundary
    end type Boundary_

contains

! ####################################################################

subroutine openBoundary(obj,path,name)
    class(Boundary_),intent(inout)::obj
    character(*),intent(in) :: path
    character(*),optional,intent(in) :: name
    type(IO_) :: f

    if(present(name) )then
        call execute_command_line("mkdir -p "//trim(path)//"/"//trim(adjustl(name)))
        call obj%DBound%open(path=trim(path)//"/"//trim(adjustl(name)), name="DBound")
        call obj%NBound%open(path=trim(path)//"/"//trim(adjustl(name)), name="NBound")
        call obj%TBound%open(path=trim(path)//"/"//trim(adjustl(name)), name="TBound")
        
        call f%open(trim(path)//"/"//trim(adjustl(name))//"/","Boundary","prop" )
        call openArray(f%fh, obj%DBoundPara)
        call openArray(f%fh, obj%NBoundPara)
        call openArray(f%fh, obj%TBoundPara)
        call openArray(f%fh, obj%DBoundVal)
        call openArray(f%fh, obj%NBoundVal)  
        call openArray(f%fh, obj%TBoundVal)  
        call openArray(f%fh, obj%TBoundElemGpVal)      
        call openArray(f%fh, obj%DBoundValInc)
        call openArray(f%fh, obj%NBoundValInc)  
        call openArray(f%fh, obj%TBoundValInc)
        call openArray(f%fh, obj%DBoundNodID)
        call openArray(f%fh, obj%NBoundNodID)
        call openArray(f%fh, obj%TBoundNodID)
        call openArray(f%fh, obj%TBoundElemID)
        call openArray(f%fh, obj%DBoundNum)
        call openArray(f%fh, obj%NBoundNum)
        call openArray(f%fh, obj%TBoundNum)
        call openArray(f%fh, obj%TBoundElemNum)
        read(f%fh,*) obj%x_max
        read(f%fh,*) obj%x_min
        read(f%fh,*) obj%y_max
        read(f%fh,*) obj%y_min
        read(f%fh,*) obj%z_max
        read(f%fh,*) obj%z_min
        read(f%fh,*) obj%t_max
        read(f%fh,*) obj%t_min
        read(f%fh,*) obj%Dcount
        read(f%fh,*) obj%Ncount
        read(f%fh,*) obj%Tcount
        read(f%fh,*) obj%layer
        read(f%fh, '(A)' ) obj%Name
        read(f%fh, '(A)' ) obj%ErrorMsg
        call f%close()
    else

        call execute_command_line("mkdir -p "//trim(path)//"/Boundary")
        call obj%DBound%open(path=trim(path), name="Boundary")
        call obj%NBound%open(path=trim(path), name="Boundary")
        call obj%TBound%open(path=trim(path), name="Boundary")
        
        call f%open(trim(path)//"/","Boundary/Boundary","prop" )
        call openArray(f%fh, obj%DBoundPara)
        call openArray(f%fh, obj%NBoundPara)
        call openArray(f%fh, obj%TBoundPara)
        call openArray(f%fh, obj%DBoundVal)
        call openArray(f%fh, obj%NBoundVal)  
        call openArray(f%fh, obj%TBoundVal)  
        call openArray(f%fh, obj%TBoundElemGpVal)      
        call openArray(f%fh, obj%DBoundValInc)
        call openArray(f%fh, obj%NBoundValInc)  
        call openArray(f%fh, obj%TBoundValInc)
        call openArray(f%fh, obj%DBoundNodID)
        call openArray(f%fh, obj%NBoundNodID)
        call openArray(f%fh, obj%TBoundNodID)
        call openArray(f%fh, obj%TBoundElemID)
        call openArray(f%fh, obj%DBoundNum)
        call openArray(f%fh, obj%NBoundNum)
        call openArray(f%fh, obj%TBoundNum)
        call openArray(f%fh, obj%TBoundElemNum)
        read(f%fh,*) obj%x_max
        read(f%fh,*) obj%x_min
        read(f%fh,*) obj%y_max
        read(f%fh,*) obj%y_min
        read(f%fh,*) obj%z_max
        read(f%fh,*) obj%z_min
        read(f%fh,*) obj%t_max
        read(f%fh,*) obj%t_min
        read(f%fh,*) obj%Dcount
        read(f%fh,*) obj%Ncount
        read(f%fh,*) obj%Tcount
        read(f%fh,*) obj%layer
        read(f%fh, '(A)' ) obj%Name
        read(f%fh, '(A)' ) obj%ErrorMsg
        call f%close()
    endif

end subroutine






! ####################################################################
subroutine removeBoudary(obj)
    class(Boundary_),intent(inout) :: obj

    call obj%DBound%remove()
    call obj%NBound%remove()
    call obj%TBound%remove()

    if(allocated(obj%DBoundPara) )then
        deallocate(obj%DBoundPara)
    endif
    if(allocated(obj%NBoundPara) )then
        deallocate(obj%NBoundPara)
    endif
    if(allocated(obj%TBoundPara) )then
        deallocate(obj%TBoundPara)
    endif
    if(allocated(obj%DBoundVal) )then
        deallocate(obj%DBoundVal)
    endif
    if(allocated(obj%NBoundVal) )then
        deallocate(obj%NBoundVal)
    endif
    if(allocated(obj%TBoundVal) )then
        deallocate(obj%TBoundVal)
    endif
    if(allocated(obj%TBoundElemGpVal) )then
        deallocate(obj%TBoundElemGpVal)
    endif
    if(allocated(obj%DBoundValInc) )then
        deallocate(obj%DBoundValInc)
    endif
    if(allocated(obj%NBoundValInc) )then
        deallocate(obj%NBoundValInc)
    endif
    if(allocated(obj%TBoundValInc) )then
        deallocate(obj%TBoundValInc)
    endif
    if(allocated(obj%DBoundNodID) )then
        deallocate(obj%DBoundNodID)
    endif
    if(allocated(obj%NBoundNodID) )then
        deallocate(obj%NBoundNodID)
    endif
    if(allocated(obj%TBoundNodID) )then
        deallocate(obj%TBoundNodID)
    endif
    if(allocated(obj%TBoundElemID) )then
        deallocate(obj%TBoundElemID)
    endif
    if(allocated(obj%DBoundNum) )then
        deallocate(obj%DBoundNum)
    endif
    if(allocated(obj%NBoundNum) )then
        deallocate(obj%NBoundNum)
    endif
    if(allocated(obj%TBoundNum) )then
        deallocate(obj%TBoundNum)
    endif
    if(allocated(obj%TBoundElemNum) )then
        deallocate(obj%TBoundElemNum)
    endif

    
    obj%x_max=0.0d0
    obj%x_min=0.0d0
    obj%y_max=0.0d0
    obj%y_min=0.0d0
    obj%z_max=0.0d0
    obj%z_min=0.0d0
    obj%t_max=0.0d0
    obj%t_min=0.0d0

    obj%Dcount=0
    obj%Ncount=0
    obj%Tcount=0
    obj%layer=0
    obj%Name=" "
    obj%ErrorMsg=" "
end subroutine

! ###########################################################################
subroutine showBoundary(obj,onlyRange)
    class(Boundary_),intent(inout) :: obj
    integer(int32) ::i,j,n
    logical ,optional, intent(in) :: onlyRange

    if(present(onlyRange) )then
        if(allocated(obj%DBound%NodCoord) )then
            print *, "Dirichlet Boundary edge:: "
            do i=1,size(obj%DBound%NodCoord,1)
                print *, obj%DBound%NodCoord(i,:)
            enddo
        else
            print *,  "Dirichlet Boundary edge:: None"
        endif
        if(allocated(obj%DBound%ElemNod) )then
            do i=1,size(obj%DBound%ElemNod,1)
                print *, "Dirichlet Boundary Connectivity:: "
                print *, obj%DBound%ElemNod(i,:)
            enddo
        else
            print *, "Dirichlet Boundary Connectivity:: None"
        endif

        if(allocated(obj%NBound%NodCoord) )then
            print *, "Neumann Boundary edge:: "
            do i=1,size(obj%NBound%NodCoord,1)
                print *, obj%NBound%NodCoord(i,:)
            enddo
        else
            print *,  "Neumann Boundary edge:: None"
        endif
        if(allocated(obj%NBound%ElemNod) )then
            do i=1,size(obj%NBound%ElemNod,1)
                print *, "Neumann Boundary Connectivity:: "
                print *, obj%NBound%ElemNod(i,:)
            enddo
        else
            print *, "Neumann Boundary Connectivity:: None"
        endif
        return
    else

        if(allocated(obj%Dbound%ElemNod) )then
            n=size(obj%Dbound%ElemNod,1)
            print *, trim(obj%Name), " as Dirichlet Boundary"
            do i=1,n
                if(i==n)then
                    print *, "  L _ Zone #",i,"Parameters : ",obj%DboundPara(i,:)
                else
                    print *, "  | - Zone #",i,"Parameters : ",obj%DboundPara(i,:)
                endif
            enddo
        endif

        if(allocated(obj%Nbound%ElemNod) )then
            n=size(obj%Nbound%ElemNod,1)
            print *, trim(obj%Name), " as Neumann Boundary"
            do i=1,n
                if(i==n)then
                    print *, "  L _ Zone #",i,"Parameters : ",obj%NboundPara(i,:)
                else
                    print *, "  | - Zone #",i,"Parameters : ",obj%NboundPara(i,:)
                endif
            enddo
        endif

        if(allocated(obj%Tbound%ElemNod))then
            n=size(obj%Tbound%ElemNod,1)
            print *, trim(obj%Name), " as Time Boundary"
            do i=1,n
                if(i==n)then
                    print *, "  L _ Zone #",i,"Parameters : ",obj%TboundPara(i,:)
                else
                    print *, "  | - Zone #",i,"Parameters : ",obj%TboundPara(i,:)
                endif
            enddo
        endif
    endif
end subroutine
! ###########################################################################

! ###########################################################################
subroutine createBoundary(obj,Name,Category,x_max,x_min,y_max,y_min,z_max,z_min,t_max,t_min,&
    BoundValue,Layer)
    class(Boundary_),intent(inout) :: obj
    character(*),optional,intent(in) :: Category,Name
    real(real64),optional,intent(in) :: x_max,x_min,y_max,y_min,z_max,z_min,t_max,t_min
    real(real64),optional,intent(in) :: BoundValue
    integer(int32),optional,intent(in) :: Layer
    obj%Name=""
    if(present(Name) )then
        obj%Name=Name
    else
        obj%Name="NoName"
    endif
    if(present(Category) )then
        if(Category == "Dirichlet")then
            call obj%setDB(x_max=x_max,x_min=x_min,y_max=y_max,y_min=y_min,z_max=z_max,&
                z_min=z_min,t_max=t_max,t_min=t_min,BoundValue=BoundValue,Layer=Layer)
        endif
    endif
    if(present(Category) )then
        if(Category == "Neumann")then
            call obj%setNB(x_max=x_max,x_min=x_min,y_max=y_max,y_min=y_min,z_max=z_max,&
                z_min=z_min,t_max=t_max,t_min=t_min,BoundValue=BoundValue,Layer=Layer)
        endif
    endif
    if(present(Category) )then
        if(Category == "Time")then
            call obj%setTB(x_max=x_max,x_min=x_min,y_max=y_max,y_min=y_min,z_max=z_max,&
                z_min=z_min,t_max=t_max,t_min=t_min,BoundValue=BoundValue,Layer=Layer)
        endif
    endif
end subroutine
! ###########################################################################

! ###########################################################################
subroutine setDBoundary(obj,x_max,x_min,y_max,y_min,z_max,z_min,t_max,t_min,BoundValue,Layer)
    class(Boundary_),intent(inout) :: obj
    real(real64),optional,intent(in) :: x_max,x_min,y_max,y_min,z_max,z_min,t_max,t_min
    real(real64),optional,intent(in) :: BoundValue
    integer(int32) :: i,j,n,m,valsize
    integer(int32),optional,intent(in) :: Layer
    
    obj%Layer=input(default=1, option=Layer)
    valsize=1
    if(.not. allocated(obj%DBound%NodCoord) )then
        allocate(obj%DBound%NodCoord(8,3))
        allocate(obj%DBound%ElemNod(1,8))
        allocate(obj%DBoundPara(1,valsize))
        obj%Dcount=1
    endif
    obj%x_max=input(default=10000000.0d0,option=x_max)
    obj%x_min=input(default=-10000000.0d0 ,option=x_min)
    obj%y_max=input(default=10000000.0d0,option=y_max)
    obj%y_min=input(default=-10000000.0d0 ,option=y_min)
    obj%z_max=input(default=10000000.0d0,option=z_max)
    obj%z_min=input(default=-10000000.0d0 ,option=z_min)
    obj%t_max=input(default=10000000.0d0,option=t_max)
    obj%t_min=input(default=0.0d0 ,option=t_min)

    if(obj%Dcount==1 )then
        obj%DBound%NodCoord(1,1) = obj%x_min
        obj%DBound%NodCoord(1,2) = obj%y_min
        obj%DBound%NodCoord(1,3) = obj%z_min

        obj%DBound%NodCoord(2,1) = obj%x_max
        obj%DBound%NodCoord(2,2) = obj%y_min
        obj%DBound%NodCoord(2,3) = obj%z_min

        obj%DBound%NodCoord(3,1) = obj%x_max
        obj%DBound%NodCoord(3,2) = obj%y_max
        obj%DBound%NodCoord(3,3) = obj%z_min

        obj%DBound%NodCoord(4,1) = obj%x_min
        obj%DBound%NodCoord(4,2) = obj%y_max
        obj%DBound%NodCoord(4,3) = obj%z_min

        obj%DBound%NodCoord(5,1) = obj%x_min
        obj%DBound%NodCoord(5,2) = obj%y_min
        obj%DBound%NodCoord(5,3) = obj%z_max

        obj%DBound%NodCoord(6,1) = obj%x_max
        obj%DBound%NodCoord(6,2) = obj%y_min
        obj%DBound%NodCoord(6,3) = obj%z_max

        obj%DBound%NodCoord(7,1) = obj%x_max
        obj%DBound%NodCoord(7,2) = obj%y_max
        obj%DBound%NodCoord(7,3) = obj%z_max

        obj%DBound%NodCoord(8,1) = obj%x_min
        obj%DBound%NodCoord(8,2) = obj%y_max
        obj%DBound%NodCoord(8,3) = obj%z_max


        do i=1,8
            obj%DBound%ElemNod(1,i)=i
        enddo
        obj%DBoundPara(1,1)=input(default=0.0d0,option=BoundValue)
        !if(present(Values) )then
        !    obj%DBoundPara(1,:)=values
        !endif
        obj%Dcount=obj%Dcount+1
    else
        n=size(obj%DBound%ElemNod,1)
        do i=1,8
            call extendArray(mat=obj%DBound%NodCoord,extend1stColumn=.true.)
        enddo
        call extendArray(mat=obj%DBoundPara,extend1stColumn=.true.)
        call extendArray(mat=obj%DBound%ElemNod,extend1stColumn=.true.)
        obj%Dcount=obj%Dcount+1
        obj%DBound%NodCoord( n*8 + 1,1) = obj%x_min
        obj%DBound%NodCoord( n*8 + 1,2) = obj%y_min
        obj%DBound%NodCoord( n*8 + 1,3) = obj%z_min
        
        obj%DBound%NodCoord( n*8 + 2,1) = obj%x_max
        obj%DBound%NodCoord( n*8 + 2,2) = obj%y_min
        obj%DBound%NodCoord( n*8 + 2,3) = obj%z_min

        obj%DBound%NodCoord( n*8 + 3,1) = obj%x_max
        obj%DBound%NodCoord( n*8 + 3,2) = obj%y_max
        obj%DBound%NodCoord( n*8 + 3,3) = obj%z_min

        obj%DBound%NodCoord( n*8 + 4,1) = obj%x_min
        obj%DBound%NodCoord( n*8 + 4,2) = obj%y_max
        obj%DBound%NodCoord( n*8 + 4,3) = obj%z_min

        obj%DBound%NodCoord( n*8 + 5,1) = obj%x_min
        obj%DBound%NodCoord( n*8 + 5,2) = obj%y_min
        obj%DBound%NodCoord( n*8 + 5,3) = obj%z_max

        obj%DBound%NodCoord( n*8 + 6,1) = obj%x_max
        obj%DBound%NodCoord( n*8 + 6,2) = obj%y_min
        obj%DBound%NodCoord( n*8 + 6,3) = obj%z_max

        obj%DBound%NodCoord( n*8 + 7,1) = obj%x_max
        obj%DBound%NodCoord( n*8 + 7,2) = obj%y_max
        obj%DBound%NodCoord( n*8 + 7,3) = obj%z_max

        obj%DBound%NodCoord( n*8 + 8,1) = obj%x_min
        obj%DBound%NodCoord( n*8 + 8,2) = obj%y_max
        obj%DBound%NodCoord( n*8 + 8,3) = obj%z_max
        
        obj%DBoundPara(1+n,1)=input(default=0.0d0,option=BoundValue)
        do i=1,8
            obj%DBound%ElemNod(1+n,i)=i+n*8
        enddo
        !if(present(Values) )then
        !    obj%DBoundPara(1+n,:)=values
        !endif


    endif
    
end subroutine setDBoundary
! ###########################################################################


! ###########################################################################
subroutine setNBoundary(obj,x_max,x_min,y_max,y_min,z_max,z_min,t_max,t_min,BoundValue,Layer)
    class(Boundary_),intent(inout) :: obj
    real(real64),optional,intent(in) :: x_max,x_min,y_max,y_min,z_max,z_min,t_max,t_min
    real(real64),optional,intent(in) :: BoundValue
    integer(int32) :: i,j,n,m,valsize
    integer(int32),optional,intent(in) :: Layer
    
    obj%Layer=input(default=1, option=Layer)
    
    valsize=1
    if(.not. allocated(obj%NBound%NodCoord) )then
        allocate(obj%NBound%NodCoord(8,3))
        allocate(obj%NBound%ElemNod(1,8))
        allocate(obj%NBoundPara(1,valsize))
        obj%Ncount=1
    endif
    obj%x_max=input(default=10000000.0d0,option=x_max)
    obj%x_min=input(default=-10000000.0d0 ,option=x_min)
    obj%y_max=input(default=10000000.0d0,option=y_max)
    obj%y_min=input(default=-10000000.0d0 ,option=y_min)
    obj%z_max=input(default=10000000.0d0,option=z_max)
    obj%z_min=input(default=-10000000.0d0 ,option=z_min)
    obj%t_max=input(default=10000000.0d0,option=t_max)
    obj%t_min=input(default=0.0d0 ,option=t_min)

    if(obj%Ncount==1 )then
        obj%NBound%NodCoord(1,1) = obj%x_min
        obj%NBound%NodCoord(1,2) = obj%y_min
        obj%NBound%NodCoord(1,3) = obj%z_min

        obj%NBound%NodCoord(2,1) = obj%x_max
        obj%NBound%NodCoord(2,2) = obj%y_min
        obj%NBound%NodCoord(2,3) = obj%z_min

        obj%NBound%NodCoord(3,1) = obj%x_max
        obj%NBound%NodCoord(3,2) = obj%y_max
        obj%NBound%NodCoord(3,3) = obj%z_min

        obj%NBound%NodCoord(4,1) = obj%x_min
        obj%NBound%NodCoord(4,2) = obj%y_max
        obj%NBound%NodCoord(4,3) = obj%z_min

        obj%NBound%NodCoord(5,1) = obj%x_min
        obj%NBound%NodCoord(5,2) = obj%y_min
        obj%NBound%NodCoord(5,3) = obj%z_max

        obj%NBound%NodCoord(6,1) = obj%x_max
        obj%NBound%NodCoord(6,2) = obj%y_min
        obj%NBound%NodCoord(6,3) = obj%z_max

        obj%NBound%NodCoord(7,1) = obj%x_max
        obj%NBound%NodCoord(7,2) = obj%y_max
        obj%NBound%NodCoord(7,3) = obj%z_max

        obj%NBound%NodCoord(8,1) = obj%x_min
        obj%NBound%NodCoord(8,2) = obj%y_max
        obj%NBound%NodCoord(8,3) = obj%z_max


        do i=1,8
            obj%NBound%ElemNod(1,i)=i
        enddo
        obj%NBoundPara(1,1)=input(default=0.0d0,option=BoundValue)
        !if(present(Values) )then
        !    obj%NBoundPara(1,:)=values
        !endif
        obj%Ncount=obj%Ncount+1
    else
        n=size(obj%NBound%ElemNod,1)
        do i=1,8
            call extendArray(mat=obj%NBound%NodCoord,extend1stColumn=.true.)
        enddo
        call extendArray(mat=obj%NBoundPara,extend1stColumn=.true.)
        call extendArray(mat=obj%NBound%ElemNod,extend1stColumn=.true.)
        obj%Ncount=obj%Ncount+1
        obj%NBound%NodCoord( n*8 + 1,1) = obj%x_min
        obj%NBound%NodCoord( n*8 + 1,2) = obj%y_min
        obj%NBound%NodCoord( n*8 + 1,3) = obj%z_min
        
        obj%NBound%NodCoord( n*8 + 2,1) = obj%x_max
        obj%NBound%NodCoord( n*8 + 2,2) = obj%y_min
        obj%NBound%NodCoord( n*8 + 2,3) = obj%z_min

        obj%NBound%NodCoord( n*8 + 3,1) = obj%x_max
        obj%NBound%NodCoord( n*8 + 3,2) = obj%y_max
        obj%NBound%NodCoord( n*8 + 3,3) = obj%z_min

        obj%NBound%NodCoord( n*8 + 4,1) = obj%x_min
        obj%NBound%NodCoord( n*8 + 4,2) = obj%y_max
        obj%NBound%NodCoord( n*8 + 4,3) = obj%z_min

        obj%NBound%NodCoord( n*8 + 5,1) = obj%x_min
        obj%NBound%NodCoord( n*8 + 5,2) = obj%y_min
        obj%NBound%NodCoord( n*8 + 5,3) = obj%z_max

        obj%NBound%NodCoord( n*8 + 6,1) = obj%x_max
        obj%NBound%NodCoord( n*8 + 6,2) = obj%y_min
        obj%NBound%NodCoord( n*8 + 6,3) = obj%z_max

        obj%NBound%NodCoord( n*8 + 7,1) = obj%x_max
        obj%NBound%NodCoord( n*8 + 7,2) = obj%y_max
        obj%NBound%NodCoord( n*8 + 7,3) = obj%z_max

        obj%NBound%NodCoord( n*8 + 8,1) = obj%x_min
        obj%NBound%NodCoord( n*8 + 8,2) = obj%y_max
        obj%NBound%NodCoord( n*8 + 8,3) = obj%z_max
        
        obj%NBoundPara(1+n,1)=input(default=0.0d0,option=BoundValue)
        do i=1,8
            obj%NBound%ElemNod(1+n,i)=i+n*8
        enddo
        !if(present(Values) )then
        !    obj%DBoundPara(1+n,:)=values
        !endif


    endif
    
end subroutine setNBoundary
! ###########################################################################


! ###########################################################################
subroutine setTBoundary(obj,x_max,x_min,y_max,y_min,z_max,z_min,t_max,t_min,BoundValue,Layer)
    class(Boundary_),intent(inout) :: obj
    real(real64),optional,intent(in) :: x_max,x_min,y_max,y_min,z_max,z_min,t_max,t_min
    real(real64),optional,intent(in) :: BoundValue
    integer(int32) :: i,j,n,m,valsize
    integer(int32),optional,intent(in) :: Layer
    
    obj%Layer=input(default=1, option=Layer)
    valsize=1
    if(.not. allocated(obj%TBound%NodCoord) )then
        allocate(obj%TBound%NodCoord(8,3))
        allocate(obj%TBound%ElemNod(1,8))
        allocate(obj%TBoundPara(1,valsize))
        obj%Tcount=1
    endif
    obj%x_max=input(default=10000000.0d0,option=x_max)
    obj%x_min=input(default=-10000000.0d0 ,option=x_min)
    obj%y_max=input(default=10000000.0d0,option=y_max)
    obj%y_min=input(default=-10000000.0d0 ,option=y_min)
    obj%z_max=input(default=10000000.0d0,option=z_max)
    obj%z_min=input(default=-10000000.0d0 ,option=z_min)
    obj%t_max=input(default=10000000.0d0,option=t_max)
    obj%t_min=input(default=0.0d0 ,option=t_min)

    if(obj%Tcount==1 )then
        obj%TBound%NodCoord(1,1) = obj%x_min
        obj%TBound%NodCoord(1,2) = obj%y_min
        obj%TBound%NodCoord(1,3) = obj%z_min

        obj%TBound%NodCoord(2,1) = obj%x_max
        obj%TBound%NodCoord(2,2) = obj%y_min
        obj%TBound%NodCoord(2,3) = obj%z_min

        obj%TBound%NodCoord(3,1) = obj%x_max
        obj%TBound%NodCoord(3,2) = obj%y_max
        obj%TBound%NodCoord(3,3) = obj%z_min

        obj%TBound%NodCoord(4,1) = obj%x_min
        obj%TBound%NodCoord(4,2) = obj%y_max
        obj%TBound%NodCoord(4,3) = obj%z_min

        obj%TBound%NodCoord(5,1) = obj%x_min
        obj%TBound%NodCoord(5,2) = obj%y_min
        obj%TBound%NodCoord(5,3) = obj%z_max

        obj%TBound%NodCoord(6,1) = obj%x_max
        obj%TBound%NodCoord(6,2) = obj%y_min
        obj%TBound%NodCoord(6,3) = obj%z_max

        obj%TBound%NodCoord(7,1) = obj%x_max
        obj%TBound%NodCoord(7,2) = obj%y_max
        obj%TBound%NodCoord(7,3) = obj%z_max

        obj%TBound%NodCoord(8,1) = obj%x_min
        obj%TBound%NodCoord(8,2) = obj%y_max
        obj%TBound%NodCoord(8,3) = obj%z_max


        do i=1,8
            obj%TBound%ElemNod(1,i)=i
        enddo
        obj%TBoundPara(1,1)=input(default=0.0d0,option=BoundValue)
        !if(present(Values) )then
        !    obj%TBoundPara(1,:)=values
        !endif
        obj%Tcount=obj%Tcount+1
    else
        n=size(obj%TBound%ElemNod,1)
        do i=1,8
            call extendArray(mat=obj%TBound%NodCoord,extend1stColumn=.true.)
        enddo
        call extendArray(mat=obj%TBoundPara,extend1stColumn=.true.)
        call extendArray(mat=obj%TBound%ElemNod,extend1stColumn=.true.)
        obj%Tcount=obj%Tcount+1
        obj%TBound%NodCoord( n*8 + 1,1) = obj%x_min
        obj%TBound%NodCoord( n*8 + 1,2) = obj%y_min
        obj%TBound%NodCoord( n*8 + 1,3) = obj%z_min
        
        obj%TBound%NodCoord( n*8 + 2,1) = obj%x_max
        obj%TBound%NodCoord( n*8 + 2,2) = obj%y_min
        obj%TBound%NodCoord( n*8 + 2,3) = obj%z_min

        obj%TBound%NodCoord( n*8 + 3,1) = obj%x_max
        obj%TBound%NodCoord( n*8 + 3,2) = obj%y_max
        obj%TBound%NodCoord( n*8 + 3,3) = obj%z_min

        obj%TBound%NodCoord( n*8 + 4,1) = obj%x_min
        obj%TBound%NodCoord( n*8 + 4,2) = obj%y_max
        obj%TBound%NodCoord( n*8 + 4,3) = obj%z_min

        obj%TBound%NodCoord( n*8 + 5,1) = obj%x_min
        obj%TBound%NodCoord( n*8 + 5,2) = obj%y_min
        obj%TBound%NodCoord( n*8 + 5,3) = obj%z_max

        obj%TBound%NodCoord( n*8 + 6,1) = obj%x_max
        obj%TBound%NodCoord( n*8 + 6,2) = obj%y_min
        obj%TBound%NodCoord( n*8 + 6,3) = obj%z_max

        obj%TBound%NodCoord( n*8 + 7,1) = obj%x_max
        obj%TBound%NodCoord( n*8 + 7,2) = obj%y_max
        obj%TBound%NodCoord( n*8 + 7,3) = obj%z_max

        obj%TBound%NodCoord( n*8 + 8,1) = obj%x_min
        obj%TBound%NodCoord( n*8 + 8,2) = obj%y_max
        obj%TBound%NodCoord( n*8 + 8,3) = obj%z_max
        
        obj%TBoundPara(1+n,1)=input(default=0.0d0,option=BoundValue)
        do i=1,8
            obj%TBound%ElemNod(1+n,i)=i+n*8
        enddo
        !if(present(Values) )then
        !    obj%DBoundPara(1+n,:)=values
        !endif


    endif
    
end subroutine setTBoundary
! ###########################################################################




!############### Check Consistency of Objects ###############
subroutine CheckDatatypeBoundary(obj)
    class(Boundary_),intent(inout)::obj

    integer(int32) i,j,n,m,o

    if(allocated(obj%DBoundNodID))then
        obj%ErrorMsg=""
    else
        obj%ErrorMsg="Check Point 1/10 DBoundNodID is not allocated"
        return
    endif

    obj%ErrorMsg=""
    do i=1,size(obj%DBoundNodID,1)
        do j=1,size(obj%DBoundNodID,2)
            if(obj%DBoundNodID(i,j)==0 )then
                obj%ErrorMsg="Check Point 1/10 : Node-pointer should not be == 0 in DBoundNodID"
                return
            endif
            if(obj%DBoundNodID(i,j)<-1 )then
                obj%ErrorMsg="Check Point 1/10 : Node-pointer should not be < -1 in DBoundNodID"
                return
            endif
            if(obj%DBoundNodID(i,j)/=obj%DBoundNodID(i,j) )then
                obj%ErrorMsg="Check Point 1/10 : Invalid integer(int32) is in DBoundNodID"
                return
            endif         
        enddo
    enddo
    
    
    
    if(allocated(obj%DBoundVal))then
        obj%ErrorMsg=""
    else
        obj%ErrorMsg="Check Point 2/10 DBoundVal is not allocated"
        return
    endif
    obj%ErrorMsg=""
    do i=1,size(obj%DBoundVal,1)
        do j=1,size(obj%DBoundVal,2)
            if(obj%DBoundVal(i,j)/=obj%DBoundVal(i,j) )then
                obj%ErrorMsg="Check Point 1/10 : Invalid RealNumber is in DBoundVal"
                return
            endif         
        enddo
    enddo
    
    if(allocated(obj%NBoundNodID))then
        obj%ErrorMsg=""
    else
        obj%ErrorMsg="Check Point 3/10 NBoundNodID is not allocated"
        return
    endif
    obj%ErrorMsg=""
    do i=1,size(obj%NBoundNodID,1)
        do j=1,size(obj%NBoundNodID,2)
            if(obj%NBoundNodID(i,j)==0 )then
                obj%ErrorMsg="Check Point 1/10 : Node-pointer should not be == 0 in NBoundNodID"
                return
            endif
            if(obj%NBoundNodID(i,j)<-1 )then
                obj%ErrorMsg="Check Point 1/10 : Node-pointer should not be < -1 in NBoundNodID"
                return
            endif
            if(obj%NBoundNodID(i,j)/=obj%NBoundNodID(i,j) )then
                obj%ErrorMsg="Check Point 1/10 : Invalid integer(int32) is in NBoundNodID"
                return
            endif         
        enddo
    enddo

    if(allocated(obj%NBoundVal))then
        obj%ErrorMsg=""
    else
        obj%ErrorMsg="Check Point 4/10 NBoundVal is not allocated"
        return
    endif
    obj%ErrorMsg=""
    do i=1,size(obj%DBoundVal,1)
        do j=1,size(obj%DBoundVal,2)
            if(obj%DBoundVal(i,j)/=obj%DBoundVal(i,j) )then
                obj%ErrorMsg="Check Point 1/10 : Invalid RealNumber is in DBoundVal"
                return
            endif         
        enddo
    enddo
    
    if(allocated(obj%DBoundNum))then
        obj%ErrorMsg=""
    else
        obj%ErrorMsg="Check Point 5/10 DBoundNum is not allocated"
        return
    endif

    if(allocated(obj%NBoundNum))then
        obj%ErrorMsg=""
    else
        obj%ErrorMsg="Check Point 6/10 NBoundNum is not allocated"
        return
    endif

    obj%ErrorMsg = "No problem was found!"

end subroutine CheckDatatypeBoundary
!############### Check Consistency of Objects ###############


!############### Deallocate All Objects ###############
subroutine DeallocateBoundary(obj)
    class(Boundary_),intent(inout)::obj

    if(allocated(obj%DBoundVal) )then
        deallocate(obj%DBoundVal)
    endif
    if(allocated(obj%DBoundNodID) )then
        deallocate(obj%DBoundNodID)
    endif
    if(allocated(obj%NBoundVal) )then
        deallocate(obj%NBoundVal)
    endif
    if(allocated(obj%NBoundNodID) )then
        deallocate(obj%NBoundNodID)
    endif
    
    if(allocated(obj%DBoundNum) )then
        deallocate(obj%DBoundNum)
    endif
    if(allocated(obj%NBoundNum) )then
        deallocate(obj%NBoundNum)
    endif
    obj%ErrorMsg="All Array are deallocated."
    

end subroutine DeallocateBoundary
!############### Deallocate All Objects ###############



!############### Initialize Boundary Conditions ###############
subroutine InitializeBoundary(obj,Default,NumberOfBC)
    class(Boundary_),intent(inout)::obj
    logical,optional,intent(in):: Default
    integer(int32),optional,intent(in)::NumberOfBC

    integer(int32) i,j,n,m,num

    if(present(Default) )then
        if(Default .eqv. .true.)then
            call obj%Delete()

            allocate(obj%DBoundVal(1,1) )
            allocate(obj%NBoundVal(1,1) )  
            allocate(obj%TBoundVal(1,1) )  
            allocate(obj%TBoundElemGpVal(1,1,1) )      
            allocate(obj%DBoundValInc(1,1) )
            allocate(obj%NBoundValInc(1,1) )  
            allocate(obj%TBoundValInc(1,1) )
            allocate(obj%DBoundNodID(1,1) )
            allocate(obj%NBoundNodID(1,1) )
            allocate(obj%TBoundNodID(1,1) )
            allocate(obj%TBoundElemID(1) )
            allocate(obj%DBoundNum(1) )
            allocate(obj%NBoundNum(1) )
            allocate(obj%TBoundNum(1) )
            allocate(obj%TBoundElemNum(1) )
            allocate(obj%DBound%NodCoord(8,3))
            allocate(obj%NBound%NodCoord(8,3))
            allocate(obj%TBound%NodCoord(8,3))
            allocate(obj%DBound%ElemNod(1,8))
            allocate(obj%NBound%ElemNod(1,8))
            allocate(obj%TBound%ElemNod(1,8))
    
            obj%DBoundVal(1,1) =0.0d0
            obj%NBoundVal(1,1)   =0.0d0
            obj%TBoundVal(1,1)   =0.0d0
            obj%TBoundElemGpVal(1,1,1)     =0.0d0  
            obj%DBoundValInc(1,1) = 0.0d0
            obj%NBoundValInc(1,1) = 0.0d0
            obj%TBoundValInc(1,1) = 0.0d0
            obj%DBoundNodID(1,1) = 0
            obj%NBoundNodID(1,1) = 0
            obj%TBoundNodID(1,1) = 0
            obj%TBoundElemID(1) =0
            obj%DBoundNum(1) =0
            obj%NBoundNum(1) =0
            obj%TBoundNum(1) =0
            obj%TBoundElemNum(1) =0        

            return
        endif
    endif

    if(allocated(obj%DBoundNum))then
        deallocate(obj%DBoundNum)
    endif
    if(.not.allocated(obj%DBoundNodID) )then
        m=1
    else
        m=size(obj%DBoundNodID,2)
    endif
    allocate(obj%DBoundNum( m ) )
    obj%DBoundNum(:)=0
    do i=1,size(obj%DBoundNodID,1)
        do j=1,size(obj%DBoundNodID,2)
            if(obj%DBoundNodID(i,j)==-1)then
                cycle
            else
                obj%DBoundNum(j)=obj%DBoundNum(j)+1
            endif
        enddo
    enddo

    if(allocated(obj%NBoundNum))then
        deallocate(obj%NBoundNum)
    endif
    if(.not.allocated(obj%NBoundNodID) )then
        m=1
    else
        m=size(obj%NBoundNodID,2)
    endif
    allocate(obj%NBoundNum( m ) )
    obj%NBoundNum(:)=0
    do i=1,size(obj%NBoundNodID,1)
        do j=1,size(obj%NBoundNodID,2)
            if(obj%NBoundNodID(i,j)==-1)then
                cycle
            else
                obj%NBoundNum(j)=obj%NBoundNum(j)+1
            endif
        enddo
    enddo

end subroutine InitializeBoundary
!############### Initialize Boundary Conditions ###############

!############### Delete Overlapped Boundary Conditions ###############
subroutine DeleteOverlapBoundary(obj)
    class(Boundary_),intent(inout)::obj

    !only Dirichlet Boundary Condition is checked.
    !overwrite mode
    integer(int32),allocatable::Intbuf(:,:)
    real(real64),allocatable::Realbuf(:,:)
    integer(int32) :: i,j,k,n,m,countnum

    n = size(obj%DBoundNodID,1)
    m = size(obj%DBoundNodID,2)
    allocate(IntBuf(n,m),RealBuf(n,m)  )
    IntBuf(:,:) =   -2
    RealBuf(:,:)=   0.0d0


    ! if overlapped -> overwrite by canceling the condition (Fill by -1)
    do i=1,size(obj%DBoundNodID,2)
        do j=1,size(obj%DBoundNodID,1)-1
            do k=j+1, size(obj%DBoundNodID,1)
                if(obj%DBoundNodID(j,i)==obj%DBoundNodID(k,i) )then
                    obj%DBoundNodID(j,i)=-1
                    exit
                else
                    cycle
                endif
            enddo
        enddo
    enddo

    ! for each line, if all components are equal to -1, cancel
    countnum = 0
    do i=1,n ! over lines
        k=0
        do j=1,m
            if(obj%DBoundNodID(i,j)==-1 )then
                k=k+1
            else
                cycle
            endif
        enddo
        if(k/=m)then
            countnum = countnum + 1
            IntBuf(countnum,: ) =obj%DBoundNodID(i,:)
            RealBuf(countnum,: )=obj%DBoundVal(i,:)
        else
            cycle
        endif
    enddo

    call TrimArray(IntBuf, countnum)
    call TrimArray(RealBuf,countnum)
    call CopyArray(IntBuf, obj%DBoundNodID)
    call CopyArray(RealBuf,obj%DBoundVal  )


end subroutine DeleteOverlapBoundary
!############### Delete Overlapped Boundary Conditions ###############


!###############  Import D-Boundary Condition ###############
subroutine ImportDBound(obj,Node_ID,DValue)
    class(Boundary_),intent(inout)::obj
    real(real64),intent(in)::DValue(:,:)
    integer(int32),intent(in)::Node_ID(:,:)

    
    integer :: i,DBoundDimension,DBoundNum

    DBoundNum      =size(DValue,1)
    DBoundDimension=size(DValue,2)

    allocate(obj%DBoundVal(DBoundNum,DBoundDimension) )
    allocate(obj%DBoundNodID(DBoundNum,DBoundDimension))
    obj%DBoundVal(:,:)=DValue(:,:)
    obj%DBoundNodID(:,:)=Node_ID(:,:)

end subroutine ImportDBound
!###############  Import D-Boundary Condition ###############



!###############  Import D-Boundary Condition ###############
subroutine MergeDBound(BCObj1,MeshObj1,BCObj2,MeshObj2,OutBCObj)
    class(Boundary_),intent(in)::BCObj1,BCObj2
    class(Mesh_),intent(in)::MeshObj1,MeshObj2
    class(Boundary_),intent(inout)::OutBCObj

    
    integer :: i,j,n1,n2,n3
    integer :: NumOfNode1,NumOfNode2,NumOfDim

    NumOfNode1=size(MeshObj1%NodCoord,1)
    NumOfNode2=size(MeshObj2%NodCoord,1)
    NumOfDim=size(MeshObj2%NodCoord,2)

    n1 = size(BCObj1%DBoundVal,1)
    n2 = size(BCObj2%DBoundVal,1)
    n3 = size(BCObj2%DBoundVal,2)
    allocate(OutBCObj%DBoundVal(n1+n2,n3) )
    OutBCObj%DBoundVal(1:n1,:)=BCObj1%DBoundVal(1:n1,:)
    OutBCObj%DBoundVal(n1+1:n1+n2,:)=BCObj1%DBoundVal(1:n2,:)
    
    n1 = size(BCObj1%DBoundNodID,1)
    n2 = size(BCObj2%DBoundNodID,1)
    n3 = size(BCObj2%DBoundNodID,2)
    allocate(OutBCObj%DBoundNodID(n1+n2,n3) )
    OutBCObj%DBoundNodID(1:n1,:)        =BCObj1%DBoundNodID(1:n1,:)
    OutBCObj%DBoundNodID(n1+1:n1+n2,:)  =BCObj1%DBoundNodID(1:n2,:)+NumOfNode1

    if(size(BCObj1%DBoundNum)/=size(BCObj2%DBoundNum) )then
        OutBCObj%ErrorMsg="ERROR :: MergeNBound >> size(BCObj1%DBoundNum)/=size(BCObj2%DBoundNum) )" 
        print *, "ERROR :: MergeNBound >> size(BCObj1%DBoundNum)/=size(BCObj2%DBoundNum) )"
        return
    endif
    allocate(OutBCObj%DBoundNum(size(BCObj1%DBoundNum ) ))
    OutBCObj%DBoundNum(:)=0
    OutBCObj%DBoundNum(:)=BCObj1%DBoundNum(:)+BCObj2%DBoundNum(:)


end subroutine MergeDBound
!###############  Import D-Boundary Condition ###############

!###############  Import N-Boundary Condition ###############
subroutine MergeNBound(BCObj1,MeshObj1,BCObj2,MeshObj2,OutBCObj)
    class(Boundary_),intent(in)::BCObj1,BCObj2
    class(Mesh_),intent(in)::MeshObj1,MeshObj2
    class(Boundary_),intent(inout)::OutBCObj

    integer :: i,j,n1,n2,n3
    integer :: NumOfNode1,NumOfNode2,NumOfDim

    NumOfNode1=size(MeshObj1%NodCoord,1)
    NumOfNode2=size(MeshObj2%NodCoord,1)
    NumOfDim=size(MeshObj2%NodCoord,2)

    n1 = size(BCObj1%NBoundVal,1)
    n2 = size(BCObj2%NBoundVal,1)
    n3 = size(BCObj2%NBoundVal,2)
    allocate(OutBCObj%NBoundVal(n1+n2,n3) )
    OutBCObj%NBoundVal(1:n1,:)=BCObj1%NBoundVal(1:n1,:)
    OutBCObj%NBoundVal(n1+1:n1+n2,:)=BCObj1%NBoundVal(1:n2,:)
    
    n1 = size(BCObj1%NBoundNodID,1)
    n2 = size(BCObj2%NBoundNodID,1)
    n3 = size(BCObj2%NBoundNodID,2)
    allocate(OutBCObj%NBoundNodID(n1+n2,n3) )
    OutBCObj%NBoundNodID(1:n1,:)        =BCObj1%NBoundNodID(1:n1,:)
    OutBCObj%NBoundNodID(n1+1:n1+n2,:)  =BCObj1%NBoundNodID(1:n2,:)+NumOfNode1


    if(size(BCObj1%NBoundNum)/=size(BCObj2%NBoundNum) )then
        OutBCObj%ErrorMsg="ERROR :: MergeNBound >> size(BCObj1%NBoundNum)/=size(BCObj2%NBoundNum) )" 
        print *, "ERROR :: MergeNBound >> size(BCObj1%NBoundNum)/=size(BCObj2%NBoundNum) )"
        return
    endif
    allocate(OutBCObj%NBoundNum(size(BCObj1%NBoundNum ) ))
    OutBCObj%NBoundNum(:)=BCObj1%NBoundNum(:)+BCObj2%NBoundNum(:)




end subroutine MergeNBound
!###############  Import N-Boundary Condition ###############



!###############  Import N-Boundary Condition ###############
subroutine ImportNBound(obj,Node_ID,NValue)
    class(Boundary_),intent(inout)::obj
    real(real64),intent(in)::NValue(:,:)
    integer(int32),intent(in)::Node_ID(:,:)

    integer :: i,NBoundDimension,NBoundNum

    NBoundNum      =size(NValue,1)
    NBoundDimension=size(NValue,2)

    allocate(obj%NBoundVal(NBoundNum,NBoundDimension) )
    allocate(obj%NBoundNodID(NBoundNum,NBoundDimension))
    obj%NBoundVal(:,:)=NValue(:,:)
    obj%NBoundNodID(:,:)=Node_ID(:,:)

end subroutine ImportNBound
!###############  Import N-Boundary Condition ###############


!###############################################
subroutine removeDBCBoundary(obj)
    class(Boundary_),intent(inout)::obj

    if(allocated(obj%DBoundNodID) )then
        deallocate(obj%DBoundNodID)
    endif

    if(allocated(obj%DBoundVal) )then
        deallocate(obj%DBoundVal)
    endif
    
    if(allocated(obj%DBoundValInc) )then
        deallocate(obj%DBoundValInc)
    endif
    if(allocated(obj%DBoundNum) )then
        deallocate(obj%DBoundNum)
    endif
    
end subroutine
!###############################################


!###############################################
subroutine removeNBCBoundary(obj)
    class(Boundary_),intent(inout)::obj

    if(allocated(obj%NBoundNodID) )then
        deallocate(obj%NBoundNodID)
    endif

    if(allocated(obj%NBoundVal) )then
        deallocate(obj%NBoundVal)
    endif
    
    if(allocated(obj%NBoundValInc) )then
        deallocate(obj%NBoundValInc)
    endif
    if(allocated(obj%NBoundNum) )then
        deallocate(obj%NBoundNum)
    endif

end subroutine
!###############################################


!###############################################
subroutine removeTBCBoundary(obj)
    class(Boundary_),intent(inout)::obj

    if(allocated(obj%TBoundNodID) )then
        deallocate(obj%TBoundNodID)
    endif

    if(allocated(obj%TBoundVal) )then
        deallocate(obj%TBoundVal)
    endif
    
    if(allocated(obj%TBoundValInc) )then
        deallocate(obj%TBoundValInc)
    endif
    if(allocated(obj%TBoundNum) )then
        deallocate(obj%TBoundNum)
    endif
    if(allocated(obj%TBoundElemNum) )then
        deallocate(obj%TBoundElemNum)
    endif
end subroutine
!###############################################

!###############################################
subroutine gmshBoundary(obj,Dirichlet, Neumann, Time, Name, Tag)
    class(Boundary_),intent(inout) :: obj
    logical,optional,intent(in) :: Dirichlet, Neumann, Time
    character(*),optional,intent(in) :: Name,Tag
    real(real64),allocatable :: ElemValue(:,:)
    integer(int32) :: i


    if(present(Dirichlet) )then
        if(Dirichlet .eqv. .true.)then
            if( obj%DBound%empty() .eqv. .true. )then
                print *, "ERROR gmshBoundary :: No Dirichlet B.C. is installed. "
                return
            endif
            allocate(ElemValue(size(obj%DBoundPara,1),1 ))
            do i=1,size(obj%DBoundPara,2)
                ElemValue(:,1)=obj%DBoundPara(:,i)
                if(present(tag) )then
                    call obj%DBound%gmsh(Name=Name//"Dirichlet",ElemValue=ElemValue,&
                        OptionalContorName=trim(Tag))
                else
                    call obj%DBound%gmsh(Name=Name//"Dirichlet",ElemValue=ElemValue,&
                        OptionalContorName="Dirichlet Boundary Value :: ID = "//trim(adjustl(fstring(i))))
                endif
            enddo
            deallocate(ElemValue)
        endif
        return
    endif

    if(present(Neumann) )then
        if(Neumann .eqv. .true.)then
            if( obj%NBound%empty() .eqv. .true. )then
                print *, "ERROR gmshBoundary :: No Neumann B.C. is installed. "
                return
            endif
            allocate(ElemValue(size(obj%NBoundPara,1),1 ))
            do i=1,size(obj%NBoundPara,2)
                ElemValue(:,1)=obj%NBoundPara(:,i)
                if(present(tag) )then
                    call obj%NBound%gmsh(Name=Name//"Neumann",ElemValue=ElemValue,&
                        OptionalContorName=trim(Tag))
                else
                    call obj%NBound%gmsh(Name=Name//"Neumann",ElemValue=ElemValue,&
                        OptionalContorName="Neumann Boundary Value :: ID = "//trim(adjustl(fstring(i))))
                endif
            enddo
            deallocate(ElemValue)
        endif
        return
    endif

    if(present(Time) )then
        if(Time .eqv. .true.)then
            if( obj%TBound%empty() .eqv. .true. )then
                print *, "ERROR gmshBoundary :: No Time B.C. is installed. "
                return
            endif
            allocate(ElemValue(size(obj%TBoundPara,1),1 ))
            do i=1,size(obj%TBoundPara,2)
                ElemValue(:,1)=obj%TBoundPara(:,i)
                if(present(tag) )then
                    call obj%TBound%gmsh(Name=Name//"Time",ElemValue=ElemValue,&
                        OptionalContorName=trim(Tag))
                else
                    call obj%TBound%gmsh(Name=Name//"Time",ElemValue=ElemValue,&
                        OptionalContorName="Time Boundary Value :: ID = "//trim(adjustl(fstring(i))))
                endif
            enddo
            deallocate(ElemValue)
        endif
        return
    endif
    



end subroutine gmshBoundary
!###############################################

subroutine exportBoundary(obj,restart,path)
    class(Boundary_),intent(inout)::obj
    logical,optional,intent(in) :: restart
    character(*),intent(in) :: path
    type(IO_) :: f

    if(present(restart) )then
        call execute_command_line("mkdir -p "//trim(path)//"/Boundary")
        call obj%DBound%export(path=trim(path)//"/Boundary", restart=.true.)
        call obj%NBound%export(path=trim(path)//"/Boundary", restart=.true.)
        call obj%TBound%export(path=trim(path)//"/Boundary", restart=.true.)
        
        call f%open(trim(path)//"/","Boundary/Boundary","prop" )
        write(f%fh,*) obj%DBoundPara(:,:)
        write(f%fh,*) obj%NBoundPara(:,:)
        write(f%fh,*) obj%TBoundPara(:,:)
        write(f%fh,*) obj%DBoundVal(:,:)
        write(f%fh,*) obj%NBoundVal(:,:)  
        write(f%fh,*) obj%TBoundVal(:,:)  
        write(f%fh,*) obj%TBoundElemGpVal(:,:,:)      
        write(f%fh,*) obj%DBoundValInc(:,:)
        write(f%fh,*) obj%NBoundValInc(:,:)  
        write(f%fh,*) obj%TBoundValInc(:,:)
        write(f%fh,*) obj%DBoundNodID(:,:)
        write(f%fh,*) obj%NBoundNodID(:,:)
        write(f%fh,*) obj%TBoundNodID(:,:)
        write(f%fh,*) obj%TBoundElemID(:)
        write(f%fh,*) obj%DBoundNum(:)
        write(f%fh,*) obj%NBoundNum(:)
        write(f%fh,*) obj%TBoundNum(:)
        write(f%fh,*) obj%TBoundElemNum(:)
        write(f%fh,*) obj%x_max
        write(f%fh,*) obj%x_min
        write(f%fh,*) obj%y_max
        write(f%fh,*) obj%y_min
        write(f%fh,*) obj%z_max
        write(f%fh,*) obj%z_min
        write(f%fh,*) obj%t_max
        write(f%fh,*) obj%t_min
        write(f%fh,*) obj%Dcount
        write(f%fh,*) obj%Ncount
        write(f%fh,*) obj%Tcount
        write(f%fh,*) obj%layer
        write(f%fh, '(A)' ) trim(obj%Name)
        write(f%fh, '(A)' ) trim(obj%ErrorMsg)
        call f%close()
    endif

end subroutine
! ############################################################


subroutine saveBoundary(obj,path,name)
    class(Boundary_),intent(inout)::obj
    character(*),intent(in) :: path
    character(*),optional,intent(in) :: name
    type(IO_) :: f

    if(present(name) )then
        call execute_command_line("mkdir -p "//trim(path)//"/"//trim(adjustl(name)))
        call obj%DBound%save(path=trim(path)//"/"//trim(adjustl(name)), name="DBound")
        call obj%NBound%save(path=trim(path)//"/"//trim(adjustl(name)), name="NBound")
        call obj%TBound%save(path=trim(path)//"/"//trim(adjustl(name)), name="TBound")
        
        call f%open(trim(path)//"/"//trim(adjustl(name))//"/","Boundary","prop" )
        call writeArray(f%fh, obj%DBoundPara)
        call writeArray(f%fh, obj%NBoundPara)
        call writeArray(f%fh, obj%TBoundPara)
        call writeArray(f%fh, obj%DBoundVal)
        call writeArray(f%fh, obj%NBoundVal)  
        call writeArray(f%fh, obj%TBoundVal)  
        call writeArray(f%fh, obj%TBoundElemGpVal)      
        call writeArray(f%fh, obj%DBoundValInc)
        call writeArray(f%fh, obj%NBoundValInc)  
        call writeArray(f%fh, obj%TBoundValInc)
        call writeArray(f%fh, obj%DBoundNodID)
        call writeArray(f%fh, obj%NBoundNodID)
        call writeArray(f%fh, obj%TBoundNodID)
        call writeArray(f%fh, obj%TBoundElemID)
        call writeArray(f%fh, obj%DBoundNum)
        call writeArray(f%fh, obj%NBoundNum)
        call writeArray(f%fh, obj%TBoundNum)
        call writeArray(f%fh, obj%TBoundElemNum)
        write(f%fh,*) obj%x_max
        write(f%fh,*) obj%x_min
        write(f%fh,*) obj%y_max
        write(f%fh,*) obj%y_min
        write(f%fh,*) obj%z_max
        write(f%fh,*) obj%z_min
        write(f%fh,*) obj%t_max
        write(f%fh,*) obj%t_min
        write(f%fh,*) obj%Dcount
        write(f%fh,*) obj%Ncount
        write(f%fh,*) obj%Tcount
        write(f%fh,*) obj%layer
        write(f%fh, '(A)' ) trim(obj%Name)
        write(f%fh, '(A)' ) trim(obj%ErrorMsg)
        call f%close()
    else

        call execute_command_line("mkdir -p "//trim(path)//"/Boundary")
        call obj%DBound%save(path=trim(path), name="Boundary")
        call obj%NBound%save(path=trim(path), name="Boundary")
        call obj%TBound%save(path=trim(path), name="Boundary")
        
        call f%open(trim(path)//"/","Boundary/Boundary","prop" )
        call writeArray(f%fh, obj%DBoundPara)
        call writeArray(f%fh, obj%NBoundPara)
        call writeArray(f%fh, obj%TBoundPara)
        call writeArray(f%fh, obj%DBoundVal)
        call writeArray(f%fh, obj%NBoundVal)  
        call writeArray(f%fh, obj%TBoundVal)  
        call writeArray(f%fh, obj%TBoundElemGpVal)      
        call writeArray(f%fh, obj%DBoundValInc)
        call writeArray(f%fh, obj%NBoundValInc)  
        call writeArray(f%fh, obj%TBoundValInc)
        call writeArray(f%fh, obj%DBoundNodID)
        call writeArray(f%fh, obj%NBoundNodID)
        call writeArray(f%fh, obj%TBoundNodID)
        call writeArray(f%fh, obj%TBoundElemID)
        call writeArray(f%fh, obj%DBoundNum)
        call writeArray(f%fh, obj%NBoundNum)
        call writeArray(f%fh, obj%TBoundNum)
        call writeArray(f%fh, obj%TBoundElemNum)
        write(f%fh,*) obj%x_max
        write(f%fh,*) obj%x_min
        write(f%fh,*) obj%y_max
        write(f%fh,*) obj%y_min
        write(f%fh,*) obj%z_max
        write(f%fh,*) obj%z_min
        write(f%fh,*) obj%t_max
        write(f%fh,*) obj%t_min
        write(f%fh,*) obj%Dcount
        write(f%fh,*) obj%Ncount
        write(f%fh,*) obj%Tcount
        write(f%fh,*) obj%layer
        write(f%fh, '(A)' ) trim(obj%Name)
        write(f%fh, '(A)' ) trim(obj%ErrorMsg)
        call f%close()
    endif

end subroutine


! ################################################################
subroutine AddMasterNodeBoundary(obj)
    class(Boundary_),intent(inout) :: obj

end subroutine 


end module BoundaryConditionClass