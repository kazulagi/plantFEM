module EnvironmentClass
   use LightClass
   use AirClass
   implicit none

   type :: Environment_
      type(Light_) :: light
      type(Air_)   :: air
      real(real64), allocatable :: temperature(:) ! K
      real(real64), allocatable :: light_ppfd(:) ! ppfd
      real(real64), allocatable :: light_position_x(:) ! ppfd
      real(real64), allocatable :: light_position_y(:) ! ppfd
      real(real64), allocatable :: light_position_z(:) ! ppfd
      character(:), allocatable :: msg
   contains
      procedure, public :: init => initEnvironment
   end type
contains

   subroutine initEnvironment(this, json)
      class(Environment_), intent(inout) :: this
      character(*), intent(in) :: json
      real(real64) :: temp, CO2, O2
      type(IO_) :: f

      if (.not. f%exists(json)) then
         this%msg = "[ERROR] :: initEnvironment >> file not found "+json
         return
      else
         this%msg = "found "+json
      end if

      temp = freal(f%parse(json, key1="air", key2="temperature"))
      CO2 = freal(f%parse(json, key1="air", key2="CO2"))
      O2 = freal(f%parse(json, key1="air", key2="O2"))

      call this%air%init(temp=temp, CO2=CO2, O2=O2)

      this%light%lighttype = trim(f%parse(json, key1="light", key2="source"))
      this%light%position(1) = freal(f%parse(json, key1="light", key2="position_x"))
      this%light%position(2) = freal(f%parse(json, key1="light", key2="position_y"))
      this%light%position(3) = freal(f%parse(json, key1="light", key2="position_z"))
      this%light%maxPPFD = freal(f%parse(json, key1="light", key2="maxPPFD"))

      this%light%angles(1) = degrees(atan2(this%light%position(2), this%light%position(1)))
      this%light%angles(2) = degrees(atan2(this%light%position(3), norm(this%light%position(1:2))))
      this%light%angles(1) = dble(int(this%light%angles(1)))
      this%light%angles(2) = dble(int(this%light%angles(2)))
   end subroutine

end module
