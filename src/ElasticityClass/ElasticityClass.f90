!> This is a toolbox for elasticity/elastodynamics
module ElasticityClass
    use ArrayClass
    implicit none

    type :: Elasticity_
        ! default unit:
        ! density :: t/m^3
        ! Vs      :: m/s

    contains
        procedure,public :: to_YoungModulus => to_YoungModulusElasticity
        procedure,public :: to_PoissonRatio => to_PoissonRatioElasticity
        procedure,public :: to_AcousticImpedance => to_AcousticImpedanceElasticity
        procedure,public :: to_AngularFrequency => to_AngularFrequencyElasticity
        procedure,public :: to_R => to_R_Elasticity
        procedure,public :: to_T => to_T_Elasticity
        procedure,public :: to_ImpedanceRatio => to_ImpedanceRatioElasticity
        procedure,public :: to_SurfaceResponse => to_SurfaceResponseElasticity
        procedure,public :: to_LoveWavePhaseVelocity => to_LoveWavePhaseVelocityElasticity
    end type

contains

! ###########################################################
function to_YoungModulusElasticity(this,Vs,Vp,Density) result(ret)
    class(Elasticity_),intent(in) :: this
    real(real64),intent(in) :: Vs, Vp, Density
    real(real64) :: ret, vv

    vv = this%to_PoissonRatio(Vs=Vs,Vp=Vp)
    ret = Vs*Vs*density*2.0d0*(1.0d0+vv)
    
end function
! ###########################################################


! ###########################################################
function to_PoissonRatioElasticity(this,Vs,Vp) result(ret)
    class(Elasticity_),intent(in) :: this
    real(real64),intent(in) :: Vs, Vp
    real(real64) :: ret

    ret = (((Vp/Vs)**2) - 2.0d0)/2.0d0/((((Vp/Vs)**2) - 1.0d0))

end function
! ###########################################################

function to_AcousticImpedanceElasticity(this,Density,Vs) result(ret)
    class(Elasticity_),intent(in) :: this
    real(real64),optional,intent(in) :: Density, Vs
    real(real64) :: ret

    if(present(Density) .and. present(Vs) )then
        ret = Density*Vs
    endif

end function
! ###########################################################


! ###########################################################

function to_ImpedanceRatioElasticity(this,Density,Vs) result(ret)
    class(Elasticity_),intent(in) :: this
    real(real64),intent(in) :: Density(1:2), Vs(1:2)
    real(real64) :: ret

    
    ret = this%to_AcousticImpedance(Density(2),Vs(2) )/this%to_AcousticImpedance(Density(1),Vs(1) )
    

end function
! ###########################################################

! ###########################################################

function to_T_Elasticity(this,Density,Vs) result(ret)
    class(Elasticity_),intent(in) :: this
    real(real64),optional,intent(in) :: Density(1:2), Vs(1:2)
    real(real64) :: ret

    if(present(Density) .and. present(Vs) )then
        ret = 2.0d0*Density(1)*Vs(1)/( dot_product(Density,Vs) )
    endif

end function
! ###########################################################


! ###########################################################

function to_R_Elasticity(this,Density,Vs) result(ret)
    class(Elasticity_),intent(in) :: this
    real(real64),optional,intent(in) :: Density(1:2), Vs(1:2)
    real(real64) :: ret

    if(present(Density) .and. present(Vs) )then
        ret = ( Density(1)*Vs(1) - Density(2)*Vs(2)  ) /( dot_product(Density,Vs) )
    endif

end function
! ###########################################################


! ###########################################################

function to_AngularFrequencyElasticity(this,Hz) result(ret)
    class(Elasticity_),intent(in) :: this
    real(real64),optional,intent(in) :: Hz
    real(real64) :: ret
    type(Math_) :: math
    
    if(present(Hz) )then
        ret = Hz * 2.0d0 * math%PI    
    endif

end function
! ###########################################################

! ###########################################################

function to_SurfaceResponseElasticity(this,Density,Vs, H, Hz) result(ret)
    class(Elasticity_),intent(in) :: this
    real(real64),intent(in) :: Density(1:2), Vs(1:2), H, Hz
    real(real64) :: T, R, k
    complex(real64) :: ret
    type(Math_) :: math
    
    T = this%to_T(Density=Density, Vs=Vs)
    R = this%to_R(Density=reverse(Density), Vs=reverse(Vs))
    k = this%to_AngularFrequency(Hz = Hz)/Vs(2)
    
    ret = 2.0d0* T/( 1.0d0 - R*exp(math%i *k * 2.0d0*H)  )
end function
! ###########################################################



! ###########################################################

function to_LoveWavePhaseVelocityElasticity(this,Density,Vs, H, division,Mode) result(ret)
    class(Elasticity_),intent(in) :: this
    real(real64),intent(in) :: Density(1:2), Vs(1:2), H
    integer(int32),intent(in) :: Division
    integer(int32),optional,intent(in) :: Mode
    real(real64) :: T, R, k, mu(1:2),delta_c,c0,c1,c
    real(real64),allocatable :: ret(:,:)
    type(Math_) :: math
    integer(int32) :: i, Mode_order

    Mode_order = 0.0d0
    if(present(Mode) )then
        Mode_order = Mode
    endif
    ! Vs = sqrt( mu / rho )
    ! Vs*Vs*rho = mu
    mu = Vs*Vs*Density

    ret = zeros(division,2) ! f (Hz), c (m/s)
    delta_c = (Vs(1) - Vs(2))/(division+1)
    do i=1,division
        c = Vs(2) + delta_c*(i)
        c1 = Vs(2)
        c0 = Vs(1)
        ret(i,1) = c/H/math%PI/( sqrt(  c*c/c1/c1 -1.0d0 ) )&
            *atan(  mu(2)/mu(1)*sqrt(1.0d0-c*c/c0/c0)/sqrt(c*c/c1/c1-1.0d0)    ) &
            + math%PI * Mode_order
        ret(i,2) = c
    enddo

end function
! ###########################################################

end module 
