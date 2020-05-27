module addon_example
    use SiCroF
    type::addon_example_
        ! Member variables
        real(real64),private :: realVal
        real(int32 ),private :: intVal
    contains
        ! methods (public_name => private_name)
        procedure :: set => setaddon_sample
        procedure :: show => showaddon_sample
    end type
contains

! Definitions of methods

! ################################################
subroutine setaddon_sample(obj,realVal, intVal)
    class(addon_example_),intent(inout) :: obj
    real(real64),optional,intent(in) :: realVal
    integer(int32),optional,intent(in) :: intVal

    obj%realVal = input(default=0.0d0, option=realVal)
    obj%intVal  = input(default=0, option=intVal)

end subroutine
! ################################################



! ################################################
subroutine showaddon_sample(obj)
    class(addon_example_),intent(in) :: obj
    print *, "Real-64bit value is :: ", obj%realVal
    print *, "int-32bit value is  :: ", obj%intVal
end subroutine
! ################################################

end module addon_example