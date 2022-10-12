module SDEClass
    use RandomClass
    use ArrayClass
    use MathClass
    implicit none

    integer(int32) :: PF_SDE_GeometricBrown = 1
    type :: SDE_
        real(real64),allocatable :: process(:)
        real(real64) :: c = 0.0d0
        real(real64) :: sigma= 0.0d0
        integer(int32) :: SDEType=0
    contains
        procedure,public :: init => initSDE
        procedure,public :: solve => solveSDE
    end type
contains

subroutine initSDE(obj,SDEType,c,sigma) 
    class(SDE_) ,intent(inout) :: obj
    integer(int32),intent(in) :: SDEType
    real(real64),optional,intent(in) :: c,sigma

    if(SDEType==PF_SDE_GeometricBrown)then
        ! Geometric Brown Motion
        if(.not. present(c) )then
            print *, "ERROR :: initSDE >> arg c is necessary."
            stop
        endif
        if(.not. present(sigma) )then
            print *, "ERROR :: initSDE >> arg sigma is necessary."
            stop
        endif
        obj%c = c
        obj%sigma = sigma
        obj%SDEType = PF_SDE_GeometricBrown

    endif

end subroutine

function solveSDE(obj,X0,dt,step)  result(ret)
    class(SDE_) ,intent(inout) :: obj
    real(real64),intent(in) :: X0,dt
    integer(int32),intent(in) :: step
    real(real64) ::t, Bt
    real(real64),allocatable :: ret(:)
    integer(int32) :: n,i
    type(Random_) :: random

    if(obj%SDEType==PF_SDE_GeometricBrown)then
        ! Geometric Brown Motion
        
        n = step
        ret = zeros(n)
    
        Bt = 0.0d0
        t=0.0d0
        ret(1) = X0
        do i=2,n
            t = t + dt
            Bt = Bt + random%gauss(mu=0.0d0,sigma=1.0d0)
            ret(i) = X0*exp( (obj%c - 0.50d0*obj%sigma*obj%sigma)*t + obj%sigma*Bt )
        enddo
    endif

end function

end module SDEClass