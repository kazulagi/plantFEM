module PodClass
    use KinematicClass
    use StemClass
    use FEMDomainClass
    implicit none

    type :: Pod_
        type(FEMDomain_)    ::  FEMDomain
        real(8),allocatable ::  LeafSurfaceNode2D(:,:)
        real(8)             ::  Thickness,length,width,center(3)
        integer             ::  Division
        type(Stem_),pointer ::  pStem
    contains
        procedure, public :: Init => initPod
    end type
contains 


! ########################################
subroutine initPod(obj,Thickness,length,width)
    class(Pod_),intent(inout) :: obj
    real(8),optional :: Thickness,length,width

    if(present(length) .and. present(width) )then
        obj%length  = length
        obj%width   = width
        if(present(Thickness) )then
            obj%Thickness=Thickness
        endif
        return
    endif

    print *, "Caution :: no input is in initleaf"

end subroutine 
! ########################################

end module