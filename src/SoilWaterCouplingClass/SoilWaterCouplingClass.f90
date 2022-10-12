module SoilWaterCouplingClass
    use fem

    ! This class is for numerical simulation for saturated soils.
    ! The formulation is based on JGS texts.
    type :: SoilWaterCoupling_
        type(FEMDomain_),pointer :: femdomain=>null()
    contains
        procedure,public :: init => initSoilWaterCoupling
    end type 
contains

subroutine initSoilWaterCoupling(obj,femdomain)
    class(SoilWaterCoupling_),intent(inout) :: obj
    type(FEMDomain_),target,intent(in) :: femdomain

    if(associated(obj%femdomain) )then
        nullify(obj%femdomain) 
    endif
    obj%femdomain => femdomain
end subroutine

end module SoilWaterCouplingClass