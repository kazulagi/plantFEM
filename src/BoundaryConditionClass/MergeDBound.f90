
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
