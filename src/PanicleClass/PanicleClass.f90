module PanicleClass
    use KinematicClass
    use FEMDomainClass



    type :: Panicle
        type(FEMDomain_)    ::  FEMDomain
        real(8)             ::  Thickness,length,width
        real(8)             ::  center_bottom(3),center_top(3)
        real(8)             ::  radius_bottom(3),radius_top(3)
        real(8)             ::  outer_normal_bottom(3),outer_normal_top(3)
        integer             ::  Division
        type(Panicle),pointer ::  pPanicle
    contains
        !procedure, public :: Init => initPanicle
    end type
contains

end module