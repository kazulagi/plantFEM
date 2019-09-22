integer num1,num2,num3,i


! ========= Merge nodes  ============
num1=size(inobj1%MatPara,1)
num2=size(inobj2%MatPara,1)
num3=size(inobj2%MatPara,2)
if(num3 /= size(inobj1%MatPara,1) )then
    outobj%ErrorMsg="MergeMaterialProp >> num3 /= inobj1%MatPara,1"
endif
allocate(outobj%MatPara(num1+num2, num3))
do i=1,num1
    outobj%MatPara(i,:)=inobj1%MatPara(i,:)
enddo
do i=1,num2
    outobj%MatPara(i+num1,:)=inobj2%MatPara(i,:)
enddo
outobj%NumOfMatPara=inobj1%NumOfMatPara
outobj%NumOfMaterial=inobj1%NumOfMaterial+inobj1%NumOfMaterial
! ========= Merge nodes  ============

