
    integer :: i,NBoundDimension,NBoundNum

    NBoundNum      =size(NValue,1)
    NBoundDimension=size(NValue,2)

    allocate(obj%NBoundVal(NBoundNum,NBoundDimension) )
    allocate(obj%NBoundNodID(NBoundNum,NBoundDimension))
    obj%NBoundVal(:,:)=NValue(:,:)
    obj%NBoundNodID(:,:)=Node_ID(:,:)
