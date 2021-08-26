module PlantClass
    use fem

    implicit none

    ! Leaf phyllotaxis
    integer(int32) :: PF_LEAF_PHYLLOTAXIS_ALTERNATE=1    !対生
    integer(int32) :: PF_LEAF_PHYLLOTAXIS_DISTICHOUS=2   !互生
    integer(int32) :: PF_LEAF_PHYLLOTAXIS_VERTICILLATE=3 !輪生

    ! leaf type
    integer(int32) :: PF_LEAF_TYPE_SINGLE=1 ! 単葉
    integer(int32) :: PF_LEAF_TYPE_MULTIPLE=2 ! 複葉
    integer(int32) :: PF_LEAF_TYPE_MULTIPLE__MULTIPLE=2 ! 2回羽状複葉
    integer(int32) :: PF_LEAF_TYPE_NUM=1 !複葉の場合，何枚あるか．

    ! Branching Rule
    integer(int32) :: PF_BRANCH_PHYLLOTAXIS_ALTERNATE=1    !対生
    integer(int32) :: PF_BRANCH_PHYLLOTAXIS_DISTICHOUS=2   !互生
    integer(int32) :: PF_BRANCH_PHYLLOTAXIS_VERTICILLATE=3 !輪生

    type :: Plant_
        integer(int32) :: Species
    contains
        procedure :: init => initPlant
    end type

    ! plant species
    ! Based on USDA-ARS https://plants.sc.egov.usda.gov/home/basicSearchResults?resultId=54a4f9ba-606c-421c-8c45-6062aed4bd77
    ! For Example, Glycine Max is GLMA
    character(4) :: Species
    

contains

subroutine initPlant(obj,species)
    class(Plant_),intent(inout) :: obj
    integer(int32),intent(in) :: species

    obj%species = species
    


end subroutine

end module PlantClass