module PanicleClass
    use, intrinsic :: iso_fortran_env
    use KinematicClass
    use FEMDomainClass



    type :: Panicle
        type(FEMDomain_)    ::  FEMDomain
        real(real64)             ::  Thickness,length,width
        real(real64)             ::  center_bottom(3),center_top(3)
        real(real64)             ::  radius_bottom(3),radius_top(3)
        real(real64)             ::  outer_normal_bottom(3),outer_normal_top(3)
        integer(int32)             ::  Division
        type(Panicle),pointer ::  pPanicle
    contains
        !procedure, public :: Init => initPanicle
    end type
contains

end module