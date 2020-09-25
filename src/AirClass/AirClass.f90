module AirClass
    use fem
    implicit none

    type :: Air_
        type(FEMDomain_) :: femdomain
        real(real64) :: temp = 303.0d0 ! K
        real(real64) :: CO2 = 380.0d0 ! ppm
        real(real64) :: O2 = 202000.0d0 ! ppm
    contains
        procedure, public :: init => initAir
    end type
contains

subroutine initAir(obj,temp,CO2,O2)
    class(Air_),intent(inout) :: obj
    real(real64),optional,intent(in) :: temp,CO2,O2

    obj%temp = input(default=304.0d0, option=temp)
    obj%CO2 = input(default=380.0d0, option=CO2)
    obj%O2 = input(default=202000.0d0, option=O2)

end subroutine

end module 