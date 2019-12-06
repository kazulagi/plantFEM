module BoundaryConditionClass
    use, intrinsic :: iso_fortran_env
    use ArrayClass
    use MeshClass

    implicit none

    type::Boundary_
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


        character*70 :: ErrorMsg
    contains
        procedure :: Init => InitializeBoundary
        procedure :: Delete => DeallocateBoundary
        procedure :: CheckDataType => CheckDatatypeBoundary
        procedure :: RemoveOverlap => DeleteOverlapBoundary
        procedure :: ImportDBound => ImportDBound
        procedure :: ImportNBound => ImportNBound
        procedure :: MergeDBound => MergeDBound
        procedure :: MergeNBound => MergeNBound

        procedure :: removeDBC => removeDBCBoundary        
        procedure :: removeNBC => removeNBCBoundary
        procedure :: removeTBC => removeTBCBoundary        
    end type Boundary_

contains
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
subroutine InitializeBoundary(obj,Default)
    class(Boundary_),intent(inout)::obj
    logical,optional,intent(in):: Default

    integer(int32) i,j,n,m

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

    include "./ImportDBound.f90"

end subroutine ImportDBound
!###############  Import D-Boundary Condition ###############



!###############  Import D-Boundary Condition ###############
subroutine MergeDBound(BCObj1,MeshObj1,BCObj2,MeshObj2,OutBCObj)
    class(Boundary_),intent(in)::BCObj1,BCObj2
    class(Mesh_),intent(in)::MeshObj1,MeshObj2
    class(Boundary_),intent(inout)::OutBCObj

    
    include "./MergeDBound.f90"

end subroutine MergeDBound
!###############  Import D-Boundary Condition ###############

!###############  Import N-Boundary Condition ###############
subroutine MergeNBound(BCObj1,MeshObj1,BCObj2,MeshObj2,OutBCObj)
    class(Boundary_),intent(in)::BCObj1,BCObj2
    class(Mesh_),intent(in)::MeshObj1,MeshObj2
    class(Boundary_),intent(inout)::OutBCObj

    include "./MergeNBound.f90"

end subroutine MergeNBound
!###############  Import N-Boundary Condition ###############



!###############  Import N-Boundary Condition ###############
subroutine ImportNBound(obj,Node_ID,NValue)
    class(Boundary_),intent(inout)::obj
    real(real64),intent(in)::NValue(:,:)
    integer(int32),intent(in)::Node_ID(:,:)

    include "./ImportNBound.f90"

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

end module BoundaryConditionClass