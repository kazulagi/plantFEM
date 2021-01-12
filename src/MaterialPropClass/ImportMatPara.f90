
    if(allocated(obj%MatPara) )then
        deallocate(obj%MatPara)
    endif
    allocate(obj%MatPara(size(mat_para,1),size(mat_para,2) ) )
    obj%MatPara(:,:)=mat_para(:,:)

    obj%NumOfMatPara=size(mat_para,1)
    obj%NumOfMaterial=size(mat_para,2)