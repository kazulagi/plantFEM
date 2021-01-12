module MultiDiffDeformClass
    use, intrinsic :: iso_fortran_env
    use DiffusionEquationClass
    use FiniteDeformationClass
    implicit none

contains

subroutine EnforceMassConserv(difobj,defobj)
    class(DiffusionEq_) ,intent(inout)::difobj
    class(FiniteDeform_),intent(in   )::defobj

    integer(int32) :: i,elem_num

    elem_num=size(difobj%Divergence,1)
    ! only for linear elements
    do i=1,elem_num
        difobj%Divergence(i,:)=defobj%VolInitCurrEBE(i,3)*difobj%UnknownValue(i,:)
    enddo


end subroutine

end module 