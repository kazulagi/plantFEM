
    integer :: i,DBoundDimension,DBoundNum

    DBoundNum      =size(DValue,1)
    DBoundDimension=size(DValue,2)

    allocate(obj%DBoundVal(DBoundNum,DBoundDimension) )
    allocate(obj%DBoundNodID(DBoundNum,DBoundDimension))
    obj%DBoundVal(:,:)=DValue(:,:)
    obj%DBoundNodID(:,:)=Node_ID(:,:)