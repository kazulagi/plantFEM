
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


