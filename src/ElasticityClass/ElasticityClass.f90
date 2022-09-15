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
        !procedure,public :: to_LoveWavePhaseVelocity => to_LoveWavePhaseVelocityElasticity
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



!! ###########################################################
!
!function to_LoveWavePhaseVelocityElasticity(this,Density,Vs, H, Hz) result(ret)
!    class(Elasticity_),intent(in) :: this
!    real(real64),intent(in) :: Density(1:2), Vs(1:2), H, Hz
!    real(real64) :: T, R, k
!    complex(real64) :: ret
!    type(Math_) :: math
!    
!end function
!! ###########################################################

end module 
