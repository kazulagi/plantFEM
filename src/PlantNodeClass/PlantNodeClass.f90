module PlantNodeClass
    use, intrinsic :: iso_fortran_env
    use KinematicClass
    use FEMDomainClass
    use StemClass
    use LeafClass
    use PetiClass
    use PodClass
    use FlowerClass
    use PlantRootClass
    implicit none
    
    type :: PlantNode_
        character(200)  :: crop_name
        logical         :: Reproductive
        type(Leaf_)     ,allocatable :: Leaf(:) 
        type(Peti_)     ,allocatable :: Peti(:)
        type(Flower_)   ,allocatable :: Flower(:)
        type(Pod_)      ,allocatable :: Pod(:)
        type(Stem_)     ,allocatable :: Stem(:)
        type(PlantRoot_)     ,allocatable :: Root(:)
    contains
        procedure :: init => initNode
        procedure :: export => exportNode
    end Type
contains


! ########################################
subroutine initNode(obj,regacy,PlantName,Stage,&
    LeafThickness,Leaflength,Leafwidth,LeafShapeFactor,&
    MaxLeafThickness,MaxLeaflength,MaxLeafwidth,PetiThickness,Petilength,Petiwidth,PetiShapeFactor,&
    MaxPetiThickness,MaxPetilength,MaxPetiwidth,StemThickness,Stemlength,Stemwidth,StemShapeFactor,&
    MaxStemThickness,MaxStemlength,MaxStemwidth,location)
    class(PlantNode_),intent(inout),target::obj
    character(*),intent(in) :: PlantName
    character(2),intent(in) :: Stage
    logical,optional,intent(in) :: regacy
    
    real(real64),optional,intent(in) :: LeafThickness,Leaflength,Leafwidth,LeafShapeFactor
    real(real64),optional,intent(in) :: MaxLeafThickness,MaxLeaflength,MaxLeafwidth
    real(real64),optional,intent(in) :: PetiThickness,Petilength,Petiwidth,PetiShapeFactor
    real(real64),optional,intent(in) :: MaxPetiThickness,MaxPetilength,MaxPetiwidth
    real(real64),optional,intent(in) :: StemThickness,Stemlength,Stemwidth,StemShapeFactor
    real(real64),optional,intent(in) :: MaxStemThickness,MaxStemlength,MaxStemwidth,location(3)
    real(real64) :: loc(3)



    if(present(regacy) )then
        if(regacy .eqv. .true.)then
            loc(:)=0.0d0
            if(present(location) )then
                loc(:)=location(:)
            endif
        
            if(trim(PlantName) == "soybean" .or. trim(PlantName) == "Soybean")then
                if(Stage == "VE")then
                    allocate(obj%Leaf(2) )
                    allocate(obj%Peti(2) )
                    allocate(obj%Flower(0) )
                    allocate(obj%Pod(0) )
                    allocate(obj%Stem(1) )
                    allocate(obj%Root(0) )

                
                    ! initialize leaf
                    call obj%Leaf(1)%init(ShapeFactor=LeafShapeFactor,&
                    Thickness=LeafThickness,length=Leaflength,&
                    width=Leafwidth,MaxThickness=MaxLeafThickness,&
                    Maxlength=MaxLeaflength,MaxWidth=MaxLeafWidth,&
                    location=location)
                    call obj%Leaf(2)%init(ShapeFactor=LeafShapeFactor,&
                    Thickness=LeafThickness,length=Leaflength,&
                    width=Leafwidth,MaxThickness=MaxLeafThickness,&
                    Maxlength=MaxLeaflength,MaxWidth=MaxLeafWidth,&
                    location=location)
                    ! initialize peti
                    call obj%Peti(1)%init(Thickness=PetiThickness,&
                    length=Petilength,&
                    width=Petiwidth,MaxThickness=MaxPetiThickness,&
                    Maxlength=MaxPetilength,MaxWidth=MaxPetiWidth,&
                    location=location)
                    call obj%Peti(2)%init(Thickness=PetiThickness,&
                    length=Petilength,&
                    width=Petiwidth,MaxThickness=MaxPetiThickness,&
                    Maxlength=MaxPetilength,MaxWidth=MaxPetiWidth,&
                    location=location)
                    ! initialize stem
                    call obj%Stem(1)%init(Thickness=StemThickness,&
                    length=Stemlength,&
                    width=Stemwidth,MaxThickness=MaxStemThickness,&
                    Maxlength=MaxStemlength,MaxWidth=MaxStemWidth,&
                    location=location)

                    ! joint leaves
                    obj%Leaf(1)%pPeti => obj%Peti(1)
                    obj%Leaf(2)%pPeti => obj%Peti(2)

                    ! joint peti
                    obj%Peti(1)%pStem => obj%Stem(1)
                    obj%Peti(2)%pStem => obj%Stem(1)
                
                    return
                else
                    return
                endif
            endif
        endif
    endif
end subroutine
! ########################################





! ########################################
subroutine exportNode(obj,FileName,ObjID)
    class(PlantNode_),intent(in) :: obj
    character(*),optional,intent(in) :: FileName
    integer(int32) :: i,max_num_of_peti_per_node,n
    integer(int32),intent(inout) :: objID

    max_num_of_peti_per_node=5

    do i=1,size(obj%Stem,1)
        call obj%Stem(i)%export(FileName=FileName//"_Stem"//trim(adjustl(fstring(i)))//".geo",StemID=objID)
    enddo

    do i=1,size(obj%Peti,1)
        call obj%Peti(i)%export(FileName=FileName//"_Peti"//trim(adjustl(fstring(i)))//".geo",PetiID=ObjID)
    enddo


end subroutine
! ########################################



end module