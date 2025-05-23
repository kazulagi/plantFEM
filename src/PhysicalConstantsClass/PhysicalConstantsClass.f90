module PhysicalConstantsClass
   use iso_fortran_env
   implicit none

   !> Physical constants in terms of SI unit.
   type :: PhysicalConstants_
      real(real64) :: c = 299792458.0d0
      !! speed of light in vacuum (m/s)
      real(real64) :: speed_of_light = 299792458.0d0
      !! speed of light in vacuum (m/s)
      real(real64) :: Plank = 6.62607015*10.0d0**(-34.0) 
      !! the Plank constant (J s)
      real(real64) :: g = 9.80665d0  
      !! standard acceleration of gravity (m/s)
      real(real64) :: R = 8.31446261815324d0 
      !! Molar gas constant (J/K/mol)
      real(real64) :: gas_constant = 8.31446261815324d0 
      !! Molar gas constant (J/K/mol)
      real(real64) :: Avogadro = 6.02214076*10.0d0**23 
      !! Avogadro constant (/mol)
      real(real64) :: k = 1.380649*10.0d0**(-23) 
      !! Boltzmann constant (J/K)
      real(real64) :: Boltzmann = 1.380649*10.0d0**(-23) 
      !! Boltzmann constant(J/K)
   contains
      !> It shows units for a physical quantities defined in attributes.
      procedure, public :: unit => unitPhysicalConstants
   end type
contains

   function unitPhysicalConstants(obj, tgt) result(ret)
      class(PhysicalConstants_), intent(in) :: obj
      character(*), intent(in) :: tgt
      character(:), allocatable :: ret

      if (tgt == "c") then
         ret = "m/s"
         return
      end if
      if (tgt == "speed_of_light") then
         ret = "m/s"
         return
      end if
      if (tgt == "Plank") then
         ret = "J s"
         return
      end if
      if (tgt == "g") then
         ret = "m/s"
         return
      end if
      if (tgt == "R") then
         ret = "J/K/mol"
         return
      end if
      if (tgt == "gas_constant") then
         ret = "J/K/mol"
         return
      end if
      if (tgt == "Avogadro") then
         ret = "/mol"
         return
      end if
      if (tgt == "k") then
         ret = "J/K"
         return
      end if
      if (tgt == "Boltzmann") then
         ret = "J/K"
         return
      end if

   end function

end module
