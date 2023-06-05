module ElastoPlasticityClass
    use ArrayClass
    use IOClass
    use MathClass
    use LinearSolverClass
    use FEMDomainClass
    use FEMSolverClass
    use iso_fortran_env
    implicit none


    abstract interface
        function PotentialFunction(CauchyStress,PlasticStrain,params) result(ret)
            use iso_fortran_env
            real(real64),intent(in) :: CauchyStress(:,:),PlasticStrain(:,:),params(:)
            real(real64) :: ret
        end function
    end interface


    type :: EP_Domain_
        procedure(PotentialFunction),nopass,pointer :: YieldFunction    => null()
        procedure(PotentialFunction),nopass,pointer :: PlasticPotential => null()
        type(FEMDomain_) :: femdomain 
        real(real64),allocatable :: YieldFunction_params(:,:) 
        real(real64),allocatable :: PlasticPotential_params(:,:)
        real(real64),allocatable :: ElasticPotential_params(:,:)

        ! basic fields
        real(real64),allocatable :: CauchyStress_field(:,:,:)  ! (stressid, gpid, elemid)
        real(real64),allocatable :: Strain_field(:,:,:)        ! (stressid, gpid, elemid)
        real(real64),allocatable :: PlasticStrain_field(:,:,:) ! (stressid, gpid, elemid)
        
        ! incremental form (working mem.)
        real(real64),allocatable :: dCauchyStress_field(:,:,:)  ! (stressid, gpid, elemid)
        real(real64),allocatable :: dStrain_field(:,:,:)  ! (stressid, gpid, elemid)
        real(real64),allocatable :: PlasticStrain_field_n(:,:,:) ! (stressid, gpid, elemid)

        real(real64),allocatable :: displacement(:)
    contains
        procedure,public :: importField => importFieldEpDomain
        procedure,public :: exportField => exportFieldEpDomain
    end type
    
    type :: ElastoPlasticity_
        
        type(FEMSolver_) :: femsolver 
        type(EP_Domain_),allocatable :: ep_domain(:)
        real(real64) :: tol = dble(1.0e-5)
        real(real64) :: gravity_accel(1:3) = [0.0d0,0.0d0,-9.810d0]
        integer(int32) :: MAX_NEWTON_LOOP_ITR = 10000

    contains
        procedure,public :: init  => initElastoPlasticity
        procedure,public :: solve => solveElastoPlasticity
        procedure,public :: solve_increment => solve_increment_ElastoPlasticity

        procedure,pass :: edit_YF_PP_ElastoPlasticity
        generic :: edit => edit_YF_PP_ElastoPlasticity

        procedure,public :: export => exportElastoPlasticity
        procedure,public :: exportField => exportFieldElastoPlasticity
        procedure,public :: importField => importFieldElastoPlasticity
        
        

        procedure,public :: get_internal_force =>get_internal_forceElastoPlasticity
        
        procedure,public :: update_stress_for_increment &
            => update_stress_for_incrementElastoPlasticity
        procedure,public :: get_delta_internal_force =>get_delta_internal_forceElastoPlasticity


        procedure,public :: get_external_force =>get_external_forceElastoPlasticity
        procedure,public :: fill_zero_at_DBC =>fill_zero_at_DBCElastoPlasticity
        procedure,public :: getYieldFunctionTemplate => getYieldFunctionTemplateElastoPlasticity
        

        procedure,public :: getPlasticStrain => getPlasticStrain_ElastoPlasticity
       procedure,public :: getPlasticStrain_n => getPlasticStrain_n_ElastoPlasticity

        procedure,public :: getStrain => getStrain_ElastoPlasticity
        procedure,public :: getCauchyStress => getCauchyStress_ElastoPlasticity
        procedure,public :: getdCauchyStress => getdCauchyStress_ElastoPlasticity
        
        procedure,public :: getTractionForce => getTractionForce_ElastoPlasticity
        
        procedure,public :: setPlasticStrain => setPlasticStrain_ElastoPlasticity
        procedure,public :: setCauchyStress => setCauchyStress_ElastoPlasticity
        procedure,public :: setdCauchyStress => setdCauchyStress_ElastoPlasticity
        
        procedure,public :: addStrain => addStrain_ElastoPlasticity
        procedure,public :: setdStrain => setdStrain_ElastoPlasticity

        procedure,public :: reset => rezeroElastoPlasticity
        procedure,public :: rezero => rezeroElastoPlasticity

        procedure,public :: I1 => I1ElastoPlasticity
        procedure,public :: J2 => J2ElastoPlasticity

        procedure,public :: I1_e => I1_e_ElastoPlasticity
        procedure,public :: J2_e => J2_e_ElastoPlasticity
    end type
contains



! #############################################
function to_StressTensor(YieldFunction,PlasticPotential,Strain,dStrain,CauchyStress,PlasticStrain,&
        YieldParams,PlasticParams,ElasticParams,pval,epsilon,Jmat)&
         result(tr_CauchyStress)
    procedure(PotentialFunction) :: YieldFunction
    procedure(PotentialFunction) :: PlasticPotential

    real(real64),intent(in) :: Strain(:,:),dStrain(:,:),ElasticParams(:),&
        YieldParams(:),PlasticParams(:),CauchyStress(:,:)
    real(real64),intent(inout) :: PlasticStrain(:,:)
    real(real64),optional,intent(inout) :: pval
    real(real64),optional,allocatable,intent(inout) :: Jmat(:,:)
    real(real64),intent(in) :: epsilon
    real(real64),allocatable :: tr_CauchyStress(:,:),dfds(:,:),E(:,:,:,:),dCauchyStress(:,:),&
        old_PlasticStrain(:,:),J_mat(:,:),X_vec(:),Y_vec(:),E_dfds(:,:),dX_vec(:),E_ee(:,:),&
        E11_epsilon(:,:),dfds_inv(:,:),dfds_forward(:,:),dfds_backward(:,:),ds(:,:),dE_e(:,:),&
        tr_sigma_vec(:),D_mat(:,:),tr_0_sigma(:,:),tr_f_CauchyStress(:,:),tr_b_CauchyStress(:,:),&
        E_ijkl(:,:,:,:),En(:,:),Ee(:,:),dgds(:,:)
    real(real64) :: f_val, dGamma,dGamma_lower,dGamma_upper,ddgamma, buf,G_val,K_val,eps,f_n_0,dg1,dg2
    integer(int32) :: i,j,k,l,n,m
    character(:),allocatable :: algorithm 
    real(real64) :: f_n,f_n1,dfdgamma,f_forward,f_backward
    integer(int32) :: max_itr
    type(IO_) :: f

    algorithm="ReturnMapping"
    
    tr_CauchyStress = zeros(3,3)
    ! (1)
    dCauchyStress = StVenant(ElasticStrain=dStrain,params=ElasticParams)
    
    
    ! (2)
    tr_CauchyStress = CauchyStress + dCauchyStress

    ! (3) 
    f_val = YieldFunction(CauchyStress=tr_CauchyStress, &
        PlasticStrain=PlasticStrain,params=YieldParams)
    dgamma = 0.0d0
    f_n_0 = f_val
    f_n   = f_val
    if(present(pval) )then
        pval = f_val
    endif
    
    
    dfds = zeros(3,3)
    if(is_elastic(f_val) )then    
        return
    else
        ! [Caution!]
        ! only for plastic potential with no hardening/softerning parameters associated with the plastic strain
        ! For such cases, Return mapping is required.
        if(algorithm=="ReturnMapping")then
            ! only for f(\sigma), not for f(\sigma, K, Hm ...etc.)
            
            max_itr = 4

            !tr_0_sigma = tr_CauchyStress
            dgamma = 0.0d0
            !dfds = d_dSigma(PlasticPotential=PlasticPotential,CauchyStress=tr_CauchyStress,&
            !        PlasticStrain=PlasticStrain,params=PlasticParams,epsilon=epsilon)
            !dfds = dfds/norm(dfds)
            dfdgamma = 0.0d0

            f_n = f_val

            !print *, zfill(0,4),dgamma,dfdgamma,f_val

            dgds = d_dSigma(PlasticPotential=PlasticPotential,CauchyStress=tr_CauchyStress,&
                PlasticStrain=PlasticStrain,params=PlasticParams,epsilon=epsilon)
            dgds = dgds/norm(dgds)  

            En = StVenant(ElasticStrain=dgds,params=ElasticParams)
            Ee = StVenant(ElasticStrain=dStrain,params=ElasticParams)
            

            dfds = d_dSigma(PlasticPotential=YieldFunction,CauchyStress=tr_CauchyStress,&
                PlasticStrain=PlasticStrain,params=YieldParams,epsilon=epsilon)

            dgamma = tensordot(dfds,Ee)/tensordot(dfds,En)
            
            dCauchyStress = StVenant(ElasticStrain=dStrain - dgamma*dgds,params=ElasticParams)

            tr_CauchyStress = CauchyStress + dCauchyStress
            f_n = YieldFunction(CauchyStress=tr_CauchyStress, &
                PlasticStrain=PlasticStrain,params=YieldParams)

            PlasticStrain = PlasticStrain + dgamma*dgds
 
            return
 
        elseif(algorithm=="ForwardEuler")then
            ! Forward Euler
            dfds = d_dSigma(PlasticPotential=PlasticPotential,CauchyStress=tr_CauchyStress,&
                PlasticStrain=PlasticStrain,params=PlasticParams,epsilon=epsilon)
            E = StVenant_StiffnessMatrix(params=ElasticParams)
            dGamma_upper = 0.0d0
            dGamma_lower = 0.0d0
            do i=1,3
                do j=1,3
                    do k=1,3
                        do l=1,3
                            dGamma_upper = dGamma_upper + dfds(i,j)*E(i,j,k,l)*dStrain(k,l)
                            dGamma_lower = dGamma_lower + dfds(i,j)*E(i,j,k,l)*dfds(k,l)
                        enddo
                    enddo
                enddo
            enddo
            dGamma = dGamma_upper/dGamma_lower
            PlasticStrain = old_PlasticStrain + dGamma*dfds
            ! Forward Euler
            ! ds_{ij} = E_{ijkl}( d\epsilon_{kl} - d \gamma*df/ds_{kl} )
            dCauchyStress = StVenant(ElasticStrain=dStrain - dgamma*dfds,params=ElasticParams)

            tr_CauchyStress = CauchyStress + dCauchyStress

            ! check yield function
            f_val = PlasticPotential(CauchyStress=tr_CauchyStress, &
                PlasticStrain=PlasticStrain,params=PlasticParams)

            if(present(pval) )then
                pval = f_val
            endif
        endif
    endif

end function to_StressTensor
! #############################################



! #############################################
function to_dStressTensor(YieldFunction,PlasticPotential,dStrain,CauchyStress,PlasticStrain,&
    YieldParams,PlasticParams,ElasticParams,pval,epsilon,new_PlasticStrain)&
     result(dCauchyStress)
procedure(PotentialFunction) :: YieldFunction
procedure(PotentialFunction) :: PlasticPotential

!! return increment of Cauchy tensor

real(real64),intent(in) :: dStrain(:,:),ElasticParams(:),&
    YieldParams(:),PlasticParams(:),CauchyStress(:,:)
real(real64),intent(in) :: PlasticStrain(:,:)
real(real64),intent(in) :: epsilon

real(real64),optional,intent(inout) :: pval
real(real64),optional,allocatable,intent(inout) :: new_PlasticStrain(:,:)


! local variables
real(real64),allocatable :: tr_CauchyStress(:,:),dfds(:,:),E(:,:,:,:),dCauchyStress(:,:),&
old_PlasticStrain(:,:),J_mat(:,:),X_vec(:),Y_vec(:),E_dfds(:,:),dX_vec(:),E_ee(:,:),&
E11_epsilon(:,:),dfds_inv(:,:),dfds_forward(:,:),dfds_backward(:,:),ds(:,:),dE_e(:,:),&
tr_sigma_vec(:),D_mat(:,:),tr_0_sigma(:,:),tr_f_CauchyStress(:,:),tr_b_CauchyStress(:,:),&
E_ijkl(:,:,:,:),En(:,:),Ee(:,:),dgds(:,:)

real(real64) :: f_val, dGamma,dGamma_lower,dGamma_upper,ddgamma, buf,G_val,K_val,eps,f_n_0,dg1,dg2
integer(int32) :: i,j,k,l,n,m
character(:),allocatable :: algorithm 
real(real64) :: f_n,f_n1,dfdgamma,f_forward,f_backward
integer(int32) :: max_itr
type(IO_) :: f


new_PlasticStrain = PlasticStrain
algorithm="ReturnMapping"

tr_CauchyStress = zeros(3,3)
! (1)
dCauchyStress = StVenant(ElasticStrain=dStrain,params=ElasticParams)


! (2)
tr_CauchyStress = CauchyStress + dCauchyStress

! (3) 
f_val = YieldFunction(CauchyStress=tr_CauchyStress, &
    PlasticStrain=new_PlasticStrain,params=YieldParams)
dgamma = 0.0d0
f_n_0 = f_val
f_n   = f_val
if(present(pval) )then
    pval = f_val
endif


dfds = zeros(3,3)
if(is_elastic(f_val) )then    
    return
else
    ! [Caution!]
    ! only for plastic potential with no hardening/softerning parameters associated with the plastic strain
    ! For such cases, Return mapping is required.
    if(algorithm=="ReturnMapping")then
        ! only for f(\sigma), not for f(\sigma, K, Hm ...etc.)
        
        max_itr = 4

        dgamma = 0.0d0
        dfdgamma = 0.0d0

        f_n = f_val

        dgds = d_dSigma(PlasticPotential=PlasticPotential,CauchyStress=tr_CauchyStress,&
            PlasticStrain=new_PlasticStrain,params=PlasticParams,epsilon=epsilon)
        dgds = dgds/norm(dgds)  

        En = StVenant(ElasticStrain=dgds,params=ElasticParams)
        Ee = StVenant(ElasticStrain=dStrain,params=ElasticParams)
        

        dfds = d_dSigma(PlasticPotential=YieldFunction,CauchyStress=tr_CauchyStress,&
            PlasticStrain=new_PlasticStrain,params=YieldParams,epsilon=epsilon)

        dgamma = tensordot(dfds,Ee)/tensordot(dfds,En)
        
        dCauchyStress = StVenant(ElasticStrain=dStrain - dgamma*dgds,params=ElasticParams)

        tr_CauchyStress = CauchyStress + dCauchyStress

        f_n = YieldFunction(CauchyStress=tr_CauchyStress, &
            PlasticStrain=new_PlasticStrain,params=YieldParams)

        new_PlasticStrain = new_PlasticStrain + dgamma*dgds
        !dCauchyStress = tr_CauchyStress - CauchyStress
        return
    else
        print *, "ERROR :: to_dStressTensor >> no such algorithm as",algorithm
        stop
    endif
endif

end function to_dStressTensor
! #############################################

function StVenant(ElasticStrain,params) result(CauchyStress)
    real(real64),intent(in) :: ElasticStrain(:,:),params(:)
    real(real64),allocatable :: CauchyStress(:,:)
    real(real64) :: lambda, mu

    lambda = params(1)
    mu     = params(2)
    CauchyStress = lambda*eyes(3,3)*trace(ElasticStrain) &
        + 2.0d0*mu*ElasticStrain

end function



! #############################################

! #############################################
function StVenant_StiffnessMatrix(params) result(E)
    real(real64),intent(in) :: params(:)
    real(real64),allocatable :: E(:,:,:,:),g(:,:)
    real(real64) :: lambda, mu
    integer(int32) :: i,j,k,l

    allocate(E(3,3,3,3) )
    g = eyes(3,3)
    lambda = params(1)
    mu     = params(2)
    !https://ss1.xrea.com/penguinitis.g1.xrea.com/study/note/elastic_coefficient.pdf
    do i=1,3
        do j=1,3
            do k=1,3
                do l=1,3
                    E(i,j,k,l) = lambda*lambda*g(i,j)*g(k,l) &
                        + mu*( g(i,k)*g(j,l) +g(i,l)*g(j,k)  )
                enddo
            enddo
        enddo
    enddo

end function


! #############################################
function StVenant_StiffnessMatrix_2D(params) result(E)
    real(real64),intent(in) :: params(:)
    real(real64),allocatable :: E(:,:)
    real(real64) :: lambda, mu
    integer(int32) :: i,j,k,l

    allocate(E(6,6) )
    
    lambda = params(1)
    mu     = params(2)

	E(1,1)= 2.0d0*mu + lambda
	E(1,2)= lambda
	E(1,3)= lambda
	E(2,1)= lambda
	E(2,2)= 2.0d0*mu + lambda
	E(2,3)= lambda
	E(3,1)= lambda
	E(3,2)= lambda
	E(3,3)= 2.0d0*mu + lambda
	E(4,4)= mu
	E(5,5)= mu
	E(6,6)= mu
end function


! #############################################


function MohrCoulomb(CauchyStress,PlasticStrain,params) result(ret)
    real(real64),intent(in) :: CauchyStress(:,:),PlasticStrain(:,:),params(:)
    
    real(real64) :: ret,c,phi,I_1, J_2,J_3,theta
    
    ! https://static.rocscience.cloud/assets/verification-and-theory/RSData/mohr-coulomb-model.pdf
    
    c   = params(1)
    phi = params(2)
    
    I_1 = to_I1(CauchyStress)
    J_2 = to_J2(CauchyStress)
    J_3 = to_J3(CauchyStress)
    theta = to_LodeAngle(CauchyStress)
    
    ret = I_1/3.0d0*sin(phi) &
        + sqrt(J_2)*(cos(theta)-1.0d0/sqrt(3.0d0)*sin(theta)*sin(phi) ) &
        - c * cos(phi) 


end function
! ##################################################
function DruckerPrager(CauchyStress,PlasticStrain,params) result(ret)
    real(real64),intent(in) :: CauchyStress(:,:),PlasticStrain(:,:),params(:)
    
    real(real64) :: ret,c,phi,I_1, J_2,J_3,theta, A, B
    
    ! https://static.rocscience.cloud/assets/verification-and-theory/RSData/mohr-coulomb-model.pdf
    
    c   = params(1)
    phi = params(2)
    
    I_1 = to_I1(CauchyStress)
    J_2 = to_J2(CauchyStress)
    J_3 = to_J3(CauchyStress)
    theta = to_LodeAngle(CauchyStress)
    !https://en.wikipedia.org/wiki/Drucker%E2%80%93Prager_yield_criterion
    ! middle circumscribes
    A = 6.0d0*c*cos(phi)/sqrt(3.0d0)/(3.0d0+sin(phi) )
    B = 2.0d0*sin(phi)/sqrt(3.0d0)/(3.0d0+sin(phi) )
    ret = sqrt(J_2) - A - B*I_1 

end function
! ##################################################
function VonMises(CauchyStress,PlasticStrain,params) result(ret)
    real(real64),intent(in) :: CauchyStress(:,:),PlasticStrain(:,:),params(:)
    
    real(real64) :: ret,k,J_2
    !https://www.engineersedge.com/material_science/von_mises.htm
    k   = params(1)
    
    J_2 = to_J2(CauchyStress)
    
    ret = J_2 - k**2


end function
! ##################################################

function Tresca(CauchyStress,PlasticStrain,params) result(ret)
    real(real64),intent(in) :: CauchyStress(:,:),PlasticStrain(:,:),params(:)
    
    real(real64) :: ret,k,J_2,J_3
    !https://www.engineersedge.com/material_science/von_mises.htm
    k   = params(1)
    
    J_2 = to_J2(CauchyStress)
    J_3 = to_J3(CauchyStress)
    
    ret =  4.0d0*(J_2**3) &
        - 27.0d0*(J_3**2)&
        - 36.0d0*(k**2)*(J_2**2) &
        + 96.0d0*(k**4)*(J_2) &
        - 64.0d0*(k**6)

end function
! ##################################################


function CamClay(CauchyStress,PlasticStrain,params) result(ret)
    real(real64),intent(in) :: CauchyStress(:,:),PlasticStrain(:,:),params(:)
    
    real(real64) :: ret,c,phi,J_2,J_3,p,q,M,theta,pc
    !https://www.engineersedge.com/material_science/von_mises.htm
    !http://manual.midasuser.com/JP_Common/FEANX/110/FEA_NX/%E3%83%A1%E3%83%83%E3%82%B7%E3%83%A5/%E7%89%B9%E6%80%A7_%E5%BA%A7%E6%A8%99%E7%B3%BB_%E9%96%A2%E6%95%B0/%E6%9D%90%E6%96%99/%E6%9D%90%E6%96%99%E4%B8%80%E8%88%AC/Modified_Cam_Clay.htm
    c   = params(1)
    phi   = params(2)
    pc    = params(3)
    
    J_2 = to_J2(CauchyStress)
    J_3 = to_J3(CauchyStress)
    theta = to_LodeAngle(CauchyStress)
    
    p = -3.0d0*to_I1(CauchyStress)
    q = 3.0d0*J_2
    M = 6.0d0*sin(phi)/(3.0d0-sin(phi) ) ! 厳密には正しくない
    ret = 3.0d0*J_2 + M*P*log(p/pc)

end function
! ##################################################

function is_elastic(val) result(ret)
    real(real64),intent(in) :: val
    logical :: ret

    if(val < 0.0d0)then
        ret = .true.
    else
        ret = .false.
    endif
end function

! ##################################################
function ModifiedCamClay(CauchyStress,PlasticStrain,params) result(ret)
    real(real64),intent(in) :: CauchyStress(:,:),PlasticStrain(:,:),params(:)
    
    real(real64) :: ret,c,phi,J_2,J_3,p,q,M,theta,pc
    !https://www.engineersedge.com/material_science/von_mises.htm
    !http://manual.midasuser.com/JP_Common/FEANX/110/FEA_NX/%E3%83%A1%E3%83%83%E3%82%B7%E3%83%A5/%E7%89%B9%E6%80%A7_%E5%BA%A7%E6%A8%99%E7%B3%BB_%E9%96%A2%E6%95%B0/%E6%9D%90%E6%96%99/%E6%9D%90%E6%96%99%E4%B8%80%E8%88%AC/Modified_Cam_Clay.htm
    !http://docs.itascacg.com/3dec700/common/models/camclay/doc/modelcamclay.html
    c   = params(1)
    phi   = params(2)
    pc    = params(3)
    
    J_2 = to_J2(CauchyStress)
    J_3 = to_J3(CauchyStress)
    theta = to_LodeAngle(CauchyStress)
    
    p = -3.0d0*to_I1(CauchyStress)
    !q = sqrt(3.0d0*J_2)
    M = 6.0d0*sin(phi)/(3.0d0-sin(phi) ) ! 厳密には正しくない
    ret = 3.0d0*J_2 + M*p*(p-pc)

end function
! ##################################################

! ##################################################
function to_I1(CauchyStress) result(ret)
    real(real64),intent(in) :: CauchyStress(:,:)
    real(real64) :: ret

    ret = trace(CauchyStress)/3.0d0

end function
! ##################################################

! ##################################################
function tensordot(a_ij,b_ij) result(ret)
    real(real64),intent(in) :: a_ij(:,:),b_ij(:,:)
    real(real64) :: ret
    integer(int32) :: i,j

    ret = 0.0d0
    do i=1,size(a_ij,1)
        do j=1,size(a_ij,1)
            ret = ret + a_ij(i,j)*b_ij(i,j)
        enddo
    enddo

end function
! ##################################################


! ##################################################
function tensorSelfDot(a_ij) result(ret)
    real(real64),intent(in) :: a_ij(:,:)
    real(real64) :: ret
    integer(int32) :: i,j

    ret = 0.0d0
    do i=1,size(a_ij,1)
        do j=1,size(a_ij,1)
            ret = ret + a_ij(i,j)*a_ij(i,j)
        enddo
    enddo

end function
! ##################################################


! ##################################################
function to_J1(CauchyStress) result(ret)
    real(real64),intent(in) :: CauchyStress(:,:)
    real(real64) :: ret

    ret = 0.0d0

end function
! ##################################################


! ##################################################
function to_J2(CauchyStress) result(ret)
    real(real64),intent(in) :: CauchyStress(:,:)
    real(real64) :: ret
    ! https://en.wikipedia.org/wiki/Cauchy_stress_tensor
    !ret = tensorSelfDot(to_DeviatricStress(CauchyStress))*0.50d0
    ret = 0.50d0*( &
                trace(matmul(CauchyStress,CauchyStress) ) &
                - trace(CauchyStress)*trace(CauchyStress)/3.0d0  &
                )

end function
! ##################################################


! ##################################################
function to_J3(CauchyStress) result(ret)
    real(real64),intent(in) :: CauchyStress(:,:)
    real(real64) :: ret
    ! https://en.wikipedia.org/wiki/Cauchy_stress_tensor
    ret =   ( &
                trace( matmul(CauchyStress, matmul(CauchyStress,CauchyStress) )  ) &
                - trace( matmul(CauchyStress,CauchyStress) )*trace(CauchyStress)   &
                + 2.0d0/9.0d0*trace(CauchyStress)*trace(CauchyStress)*trace(CauchyStress)  &
            )/3.0d0

end function
! ##################################################


! ##################################################
function to_LodeAngle(CauchyStress) result(ret)
    real(real64),intent(in) :: CauchyStress(:,:)
    real(real64) :: ret,J_2,J_3
    
    
    J_2 = to_J2(CauchyStress)
    J_3 = to_J3(CauchyStress)
    if(J_2==0.0d0)then
        ret = 0.0d0
    else
        if(abs(3.0d0*sqrt(3.0d0)/2.0d0*J_3/(J_2)**(1.50d0) )>1.0d0)then
            if(3.0d0*sqrt(3.0d0)/2.0d0*J_3/(J_2)**(1.50d0) > 0.0d0)then
                ret = 1.0d0/3.0d0*asin(1.0d0)    
            else
                ret = 1.0d0/3.0d0*asin(-1.0d0)
            endif
        else
            ret = 1.0d0/3.0d0*asin( - 3.0d0*sqrt(3.0d0)/2.0d0*J_3/(J_2)**(1.50d0)  )
        endif
    endif
end function
! ##################################################

! ##################################################
function to_DeviatricStress(CauchyStress) result(ret)
    real(real64),intent(in) :: CauchyStress(:,:)
    real(real64),allocatable :: ret(:,:)

    ret = CauchyStress - trace(CauchyStress)/3.0d0*eyes(size(CauchyStress,1),size(CauchyStress,1) )

end function
! ##################################################

function d_dSigma(PlasticPotential,CauchyStress,PlasticStrain,params,epsilon) result(ret)
    procedure(PotentialFunction) :: PlasticPotential
    

    real(real64),intent(in) :: CauchyStress(:,:),PlasticStrain(:,:),params(:)

    real(real64),optional,intent(in) :: epsilon

    real(real64),allocatable :: dsigma_tensor(:,:),ret(:,:)
    real(real64) :: dsigma,df
    integer(int32) :: i,j

    ret = zeros( size(CauchyStress,1), size(CauchyStress,2) )
    dsigma_tensor= zeros( size(CauchyStress,1), size(CauchyStress,2) )

    if(present(epsilon) )then
        dsigma=epsilon/2.0d0
    else
        dsigma = dble(1.0e-4)/2.0d0
    endif
    

    do i=1,size(CauchyStress,1)
        do j=i,size(CauchyStress,2)
            dsigma_tensor(:,:) = 0.0d0
            dsigma_tensor(i,j) = dsigma
            df = PlasticPotential(CauchyStress+dsigma_tensor,PlasticStrain,params) &
                - PlasticPotential(CauchyStress-dsigma_tensor,PlasticStrain,params) 
            df = df/(dsigma*2.0d0)
            ret(i,j) = df
            ret(j,i) = df
        enddo
    enddo


end function


subroutine getYieldFunctionTemplateElastoPlasticity(this,name)
    class(ElastoPlasticity_),intent(in) :: this
    character(*),intent(in) :: name
    type(IO_) :: f

    if(index(name,".f90")==0 )then
        call f%open(name+".f90","w")
    else
        call f%open(name,"w")
    endif

    call f%write("function MyYieldFunction(CauchyStress,PlasticStrain,params) result(ret)")
    call f%write("    use iso_fortran_env")
    call f%write("    real(real64),intent(in) :: CauchyStress(:,:),PlasticStrain(:,:),params(:)")
    call f%write("    real(real64) :: ret,k,J_2")
    call f%write("    k   = params(1)")
    call f%write("    J_2 = to_J2(CauchyStress)")
    call f%write("    ret = J_2 - k**2")
    call f%write("end function")
    call f%close()

end subroutine




function get_Return_mapping_tangent_matrix(PlasticPotential,CauchyStress,PlasticStrain,Strain,&
        ElasticParams,gamma,PlasticParams,epsilon) result(ret)
    procedure(PotentialFunction) :: PlasticPotential

    real(real64),intent(in) :: CauchyStress(:,:),PlasticStrain(:,:),ElasticParams(:),PlasticParams(:),&
         gamma,Strain(:,:)
    integer(int32),allocatable :: indx(:,:)

    real(real64),optional,intent(in) :: epsilon

    real(real64),allocatable :: dsigma_tensor(:,:),ret(:,:)
    real(real64) :: dsigma,df
    integer(int32) :: i,j

    real(real64) :: forward,backward
    real(real64),allocatable :: ds(:,:),dfds_forward(:,:),dfds_backward(:,:),dfds_vec(:), &
        dfds_backward_vec(:),dfds_forward_vec(:),E_dfds_backward(:),E_dfds_forward(:), &
        E_dfds_vec(:),dfds(:,:),X_vec(:),Y_vec(:),J_mat(:,:),tr_CauchyStress(:,:)

    indx = zeros(6,2)

    indx(1,1:2) = [1,1]
    indx(2,1:2) = [2,2]
    indx(3,1:2) = [3,3]
    indx(4,1:2) = [1,2]
    indx(5,1:2) = [2,3]
    indx(6,1:2) = [1,3]

    ret = zeros(6+1,6+1)
    ds = zeros(3,3)

    ret(1,1) = 1.0d0
    ret(2,2) = 1.0d0
    ret(3,3) = 1.0d0
    ret(4,4) = 1.0d0
    ret(5,5) = 1.0d0
    ret(6,6) = 1.0d0

    dfds_forward_vec = zeros(6)
    dfds_backward_vec = zeros(6)
    dfds_vec = zeros(6)

    do J=1,6
        ! forward
        ds(:,:) = 0.0d0
        ds( indx(J,1),indx(J,2) ) = epsilon

        dfds_forward = d_dSigma(PlasticPotential=PlasticPotential,CauchyStress=CauchyStress+ds,&
            PlasticStrain=PlasticStrain,params=PlasticParams,epsilon=epsilon)
        dfds_backward = d_dSigma(PlasticPotential=PlasticPotential,CauchyStress=CauchyStress-ds,&
            PlasticStrain=PlasticStrain,params=PlasticParams,epsilon=epsilon)
        
            
        do I=1,6
            dfds_forward_vec  = dfds_forward ( indx(I,1),indx(I,2) )
            dfds_backward_vec = dfds_backward( indx(I,1),indx(I,2) )
        enddo
            
        do I=1,6


            E_dfds_forward  = gamma*matmul( StVenant_StiffnessMatrix_2D(params=ElasticParams) ,dfds_forward_vec  )
            E_dfds_backward = gamma*matmul( StVenant_StiffnessMatrix_2D(params=ElasticParams) ,dfds_backward_vec )
        
            forward  = E_dfds_forward(  I )
            backward = E_dfds_backward( I )

            ret(I,J) = ret(I,J) + (forward - backward)/(2.0d0*epsilon)
        enddo

        I = 7
        forward = PlasticPotential(CauchyStress=CauchyStress+ds,PlasticStrain=PlasticStrain,&
            params=PlasticParams)
        backward = PlasticPotential(CauchyStress=CauchyStress-ds,PlasticStrain=PlasticStrain,&
            params=PlasticParams)
        ret(I,J) = ret(I,J) + (forward - backward)/(2.0d0*epsilon)

    enddo

    J=7
    dfds = d_dSigma(PlasticPotential=PlasticPotential,CauchyStress=CauchyStress,&
        PlasticStrain=PlasticStrain,params=PlasticParams,epsilon=epsilon)
    
    do I=1,6
        dfds_vec = dfds_backward( indx(I,1),indx(I,2) )
    enddo
    E_dfds_vec = matmul( StVenant_StiffnessMatrix_2D(params=ElasticParams) ,dfds_vec ) 
        
    ret(1:6,J) = E_dfds_vec(:)

    ret(7,7) = 0.0d0



end function
! ###################################################

subroutine initElastoPlasticity(this,femdomains,&
    default_YieldFunction,default_YieldFunction_params,&
    default_PlasticPotential,default_PlasticPotential_params )
    class(ElastoPlasticity_),intent(inout) :: this
    type(FEMDomain_),target,intent(in) :: femdomains(:)
    real(real64),intent(in) :: default_YieldFunction_params(:)
    real(real64),intent(in) :: default_PlasticPotential_params(:)
    procedure(PotentialFunction) :: default_YieldFunction
    procedure(PotentialFunction) :: default_PlasticPotential
    integer(int32) :: i,j,n,ne,ngp

    if(allocated(this%ep_domain) ) deallocate(this%ep_domain)
    n = size(femdomains)
    allocate(this%ep_domain(n))



    do i=1,n
        this%ep_domain(i)%femdomain        = femdomains(i)
        this%ep_domain(i)%YieldFunction    => default_YieldFunction
        this%ep_domain(i)%PlasticPotential => default_PlasticPotential
        this%ep_domain(i)%YieldFunction_params &
            = zeros( femdomains(i)%ne(),size(default_YieldFunction_params)  )
        do j=1,femdomains(i)%ne()
            this%ep_domain(i)%YieldFunction_params(j,:) = default_YieldFunction_params(:)
        enddo
        this%ep_domain(i)%PlasticPotential_params &
            = zeros( femdomains(i)%ne(),size(default_PlasticPotential_params)  )
        do j=1,femdomains(i)%ne()
            this%ep_domain(i)%PlasticPotential_params(j,:) = default_PlasticPotential_params(:)
        enddo

        ngp = this%ep_domain(i)%femdomain%ngp()
        ne = this%ep_domain(i)%femdomain%ne()
        this%ep_domain(i)%ElasticPotential_params = zeros(ne,2) ! E, v
        this%ep_domain(i)%CauchyStress_field = zeros(6,ngp,ne)  
        this%ep_domain(i)%Strain_field       = zeros(6,ngp,ne)        
        this%ep_domain(i)%PlasticStrain_field = zeros(6,ngp,ne) 
        this%ep_domain(i)%displacement = &
            zeros(this%ep_domain(i)%femdomain%nn()*this%ep_domain(i)%femdomain%nd() ) 
    enddo


    

end subroutine

! ###################################################

! ###################################################

subroutine edit_YF_PP_ElastoPlasticity(this,DomainID,YieldFunction,PlasticPotential)
    class(ElastoPlasticity_),intent(inout) :: this
    integer(int32),intent(in) :: DomainID
    procedure(PotentialFunction) :: YieldFunction
    procedure(PotentialFunction) :: PlasticPotential
    

    if(associated(this%ep_domain(DomainID)%YieldFunction) ) nullify(this%ep_domain(DomainID)%YieldFunction)
    this%ep_domain(DomainID)%YieldFunction => YieldFunction

    if(associated(this%ep_domain(DomainID)%PlasticPotential) ) nullify(this%ep_domain(DomainID)%PlasticPotential)
    this%ep_domain(DomainID)%PlasticPotential => PlasticPotential
    
end subroutine
! ###################################################


subroutine solveElastoPlasticity(this,  &
    YoungModulus,PoissonRatio,Density, &
    fix_node_list_x, &
    fix_node_list_y, &
    fix_node_list_z, &
    fix_value_list_x, &
    fix_value_list_y, &
    fix_value_list_z) 

class(ElastoPlasticity_),intent(inout) :: this
real(real64),intent(in) :: YoungModulus(:)
real(real64),intent(in) :: PoissonRatio(:)
real(real64),intent(in) :: Density(:)


integer(int32),optional,intent(in) :: fix_node_list_x(:)
integer(int32),optional,intent(in) :: fix_node_list_y(:)
integer(int32),optional,intent(in) :: fix_node_list_z(:)
real(real64),optional,intent(in) :: fix_value_list_x(:)
real(real64),optional,intent(in) :: fix_value_list_y(:)
real(real64),optional,intent(in) :: fix_value_list_z(:)


real(real64),allocatable :: disp_tr(:),d_disp(:),&
    F_int(:), &
    F_ext(:), &
    Residual_vector(:)

integer(int32) :: newton_loop_itr
integer(int32) :: ElementID
type(CRS_) :: K_matrix

if(size(this%ep_domain)==1 )then

    this%ep_domain(1)%ElasticPotential_params(:,1) = YoungModulus(:)*PoissonRatio(:)/(1.0d0+PoissonRatio(:) )&
        /(1.0d0-2.0d0*PoissonRatio(:) )
    this%ep_domain(1)%ElasticPotential_params(:,2) = YoungModulus(:)/2.0d0/(1.0d0+PoissonRatio(:))
    
    if(.not.this%femsolver%initialized)then
        call this%femsolver%init(NumDomain=1)
        call this%femsolver%setDomain(FEMDomain=this%ep_domain(1)%femdomain,DomainID=1)
        call this%femsolver%setCRS(DOF=3)
    else
        call this%femsolver%zeros()
    endif


    !$OMP parallel 
    !$OMP do
    do ElementID = 1, this%ep_domain(1)%femdomain%ne()
        call this%femsolver%setMatrix(DomainID=1,ElementID=ElementID,DOF=3,&
            Matrix=this%ep_domain(1)%femdomain%StiffnessMatrix(ElementID=ElementID,&
            E=YoungModulus(ElementID),&
            v=PoissonRatio(ElementID) ) )
        call this%femsolver%setVector(DomainID=1,ElementID=ElementID,DOF=3,&
            Vector=this%ep_domain(1)%femdomain%MassVector(&
                ElementID=ElementID,&
                DOF=this%ep_domain(1)%femdomain%nd() ,&
                Density=Density(ElementID),&
                Accel=this%gravity_accel&
                ) &    
            )
    enddo
    !$OMP end do
    !$OMP end parallel
    call this%femsolver%fix(DomainID=1,IDs=fix_node_list_x*3-2,FixValues=fix_value_list_x)
    call this%femsolver%fix(DomainID=1,IDs=fix_node_list_y*3-1,FixValues=fix_value_list_y)
    call this%femsolver%fix(DomainID=1,IDs=fix_node_list_z*3-0,FixValues=fix_value_list_z)
    
    disp_tr = this%femsolver%solve()
    
    K_matrix = this%femsolver%getCRS()

    d_disp = disp_tr
    print *, "[ok] trial disp done!"
    ! perform modified Newton-Raphson method
    do newton_loop_itr=1,this%MAX_NEWTON_LOOP_ITR
        !F_int = this%get_internal_force(dU=d_disp) ! \int_{\Omega} B \sigma d \Omega
        !dU -> d_eps -> 
        F_int = this%get_internal_force(dU=d_disp) ! \int_{\Omega} B \sigma d \Omega
        
        F_ext = this%get_external_force() ! Traction force
        F_ext=0.0d0

        Residual_vector = F_ext - F_int
        if(norm(Residual_vector) == 0.0d0)then
            exit
        endif
        call this%fill_zero_at_DBC(values=Residual_vector,&
            idx= (fix_node_list_x*3-2) &
            // (fix_node_list_y*3-1) // (fix_node_list_z*3-0)  )
        
        ! solve [K]{du} = {R}
        call K_matrix%BiCGSTAB(x=d_disp, b=Residual_vector)
        
        disp_tr = disp_tr - d_disp
        print *, newton_loop_itr,norm(d_disp),norm(Residual_vector),norm(F_int)
        if(norm(d_disp)<this%TOL )then
            exit
        endif
    enddo

    this%ep_domain(1)%displacement = disp_tr
    
else
    print *, "[ERROR] solveElastoPlasticity >> only for single-domain problem"
    stop
endif
end subroutine

! ###################################################

! ###################################################


subroutine solve_increment_ElastoPlasticity(this,  &
    YoungModulus,PoissonRatio,delta_Density, &
    fix_node_list_x, &
    fix_node_list_y, &
    fix_node_list_z, &
    fix_value_list_x, &
    fix_value_list_y, &
    fix_value_list_z) 

class(ElastoPlasticity_),intent(inout) :: this
real(real64),intent(in) :: YoungModulus(:)
real(real64),intent(in) :: PoissonRatio(:)
real(real64),intent(in) :: delta_Density(:)


integer(int32),optional,intent(in) :: fix_node_list_x(:)
integer(int32),optional,intent(in) :: fix_node_list_y(:)
integer(int32),optional,intent(in) :: fix_node_list_z(:)
real(real64),optional,intent(in) :: fix_value_list_x(:)
real(real64),optional,intent(in) :: fix_value_list_y(:)
real(real64),optional,intent(in) :: fix_value_list_z(:)


real(real64),allocatable :: d_disp_tr(:),d_d_disp(:),&
    dF_int(:), &
    dF_ext(:), &
    Residual_vector(:)

integer(int32) :: newton_loop_itr
integer(int32) :: ElementID,DomainID
type(CRS_) :: K_matrix

DomainID = 1 ! only for single-domain

if(.not.allocated(this%ep_domain(domainID)%PlasticStrain_field_n) )then
    this%ep_domain(domainID)%PlasticStrain_field_n = this%ep_domain(domainID)%PlasticStrain_field
endif

if(.not.allocated(this%ep_domain(DomainID)%dCauchyStress_field) )then
    this%ep_domain(DomainID)%dCauchyStress_field = this%ep_domain(DomainID)%CauchyStress_field
endif


if(.not.allocated(this%ep_domain(DomainID)%dStrain_field) )then
    this%ep_domain(DomainID)%dStrain_field = this%ep_domain(DomainID)%Strain_field
endif

this%ep_domain(domainID)%PlasticStrain_field = this%ep_domain(domainID)%PlasticStrain_field_n
this%ep_domain(DomainID)%dCauchyStress_field(:,:,:) = 0.0d0
this%ep_domain(DomainID)%dStrain_field(:,:,:) = 0.0d0

if(size(this%ep_domain)==1 )then

    this%ep_domain(1)%ElasticPotential_params(:,1) = YoungModulus(:)*PoissonRatio(:)/(1.0d0+PoissonRatio(:) )&
        /(1.0d0-2.0d0*PoissonRatio(:) )
    this%ep_domain(1)%ElasticPotential_params(:,2) = YoungModulus(:)/2.0d0/(1.0d0+PoissonRatio(:))
    
    if(.not.this%femsolver%initialized)then
        call this%femsolver%init(NumDomain=1)
        call this%femsolver%setDomain(FEMDomain=this%ep_domain(1)%femdomain,DomainID=1)
        call this%femsolver%setCRS(DOF=3)
    else
        call this%femsolver%zeros()
    endif


    do ElementID = 1, this%ep_domain(1)%femdomain%ne()
        call this%femsolver%setMatrix(DomainID=1,ElementID=ElementID,DOF=3,&
            Matrix=this%ep_domain(1)%femdomain%StiffnessMatrix(ElementID=ElementID,&
            E=YoungModulus(ElementID),&
            v=PoissonRatio(ElementID) ) )
        call this%femsolver%setVector(DomainID=1,ElementID=ElementID,DOF=3,&
            Vector=this%ep_domain(1)%femdomain%MassVector(&
                ElementID=ElementID,&
                DOF=this%ep_domain(1)%femdomain%nd() ,&
                Density=delta_Density(ElementID),&
                Accel=this%gravity_accel&
                ) &    
            )
    enddo
    call this%femsolver%fix(DomainID=1,IDs=fix_node_list_x*3-2,FixValues=fix_value_list_x)
    call this%femsolver%fix(DomainID=1,IDs=fix_node_list_y*3-1,FixValues=fix_value_list_y)
    call this%femsolver%fix(DomainID=1,IDs=fix_node_list_z*3-0,FixValues=fix_value_list_z)
    
    d_disp_tr = this%femsolver%solve()
    
    K_matrix = this%femsolver%getCRS()

    d_d_disp = d_disp_tr
    dF_int = zeros(size(d_disp_tr) )
    print *, "[ok] trial delta-disp done!"
    ! perform modified Newton-Raphson method
    do newton_loop_itr=1,this%MAX_NEWTON_LOOP_ITR
        !dF_int = this%get_internal_force(dU=d_d_disp) ! \int_{\Omega} B \sigma d \Omega
        !dU -> d_eps -> 
        
        call this%update_stress_for_increment(dU=d_d_disp) ! \int_{\Omega} B \sigma d \Omega
        dF_int = this%get_delta_internal_force()

        
        dF_ext = this%get_external_force() ! Traction force
        dF_ext = 0.0d0

        call this%fill_zero_at_DBC(values=dF_ext,&
            idx= (fix_node_list_x*3-2) &
            // (fix_node_list_y*3-1) // (fix_node_list_z*3-0)  )
        !print *, "norm(dF_ext)",norm(dF_ext)

        Residual_vector = dF_ext - dF_int
        if(norm(Residual_vector) == 0.0d0)then
            exit
        endif

        call this%fill_zero_at_DBC(values=Residual_vector,&
            idx= (fix_node_list_x*3-2) &
            // (fix_node_list_y*3-1) // (fix_node_list_z*3-0)  )
        
        ! solve [K]{du} = {R}
        call K_matrix%BiCGSTAB(x=d_d_disp, b=Residual_vector)
        
        d_disp_tr = d_disp_tr - d_d_disp
        print *, newton_loop_itr,norm(d_d_disp),norm(Residual_vector),norm(dF_int)
        if(norm(d_d_disp)<this%TOL )then
            exit
        endif
    enddo

    this%ep_domain(1)%displacement = this%ep_domain(1)%displacement + d_disp_tr
    
else
    print *, "[ERROR] solveElastoPlasticity >> only for single-domain problem"
    stop
endif

this%ep_domain(DomainID)%Strain_field = this%ep_domain(DomainID)%Strain_field&
     + this%ep_domain(DomainID)%dStrain_field
    
this%ep_domain(domainID)%PlasticStrain_field_n = &
    this%ep_domain(domainID)%PlasticStrain_field

this%ep_domain(DomainID)%CauchyStress_field = &
    this%ep_domain(DomainID)%CauchyStress_field  &
    + this%ep_domain(DomainID)%dCauchyStress_field 

this%ep_domain(DomainID)%dCauchyStress_field(:,:,:) = 0.0d0
this%ep_domain(DomainID)%dStrain_field(:,:,:) = 0.0d0

end subroutine

! ###################################################

function get_internal_forceElastoPlasticity(this,dU) result(ret)
    class(ElastoPlasticity_),intent(inout) :: this
    real(real64),intent(in) :: dU(:)
    real(real64),allocatable :: ret(:),PlasticStrain(:,:),dStrain(:,:),Te(:),&
        StressVector(:),Bmat(:,:),CauchyStress(:,:),dU_mat(:,:)
    type(ShapeFunction_) :: sf
        
    integer(int32) :: ElementID,GaussPointID,num_node,i,j

    num_node = size(dU)/3
    dU_mat = zeros( num_node, 3 )
    do i=1,num_node
        do j=1,3
            dU_mat(i,j) = dU(  (i-1)*3 + j )
        enddo
    enddo


    if( size(this%ep_domain)==1)then
        if(norm(dU)/=0.0d0)then
            
            ! Update stress and plastic strain
            !$OMP parallel default(shared) private(GaussPointID,PlasticStrain,dStrain,CauchyStress)
            !$OMP do
            do ElementID = 1, this%ep_domain(1)%femdomain%ne()
                do GaussPointID = 1, this%ep_domain(1)%femdomain%ngp()

                    PlasticStrain = this%getPlasticStrain(ElementID=ElementID,GaussPointID=GaussPointID)
                    dStrain = this%ep_domain(1)%femdomain%getStrainTensor(&
                        ElementID=ElementID,GaussPointID=GaussPointID,&
                        displacement=dU_mat)

                    !print *, minval(dU_mat),maxval(dU_mat)
                    CauchyStress = to_StressTensor(&
                        YieldFunction=this%ep_domain(1)%YieldFunction ,&
                        PlasticPotential=this%ep_domain(1)%PlasticPotential,&
                        Strain=this%getStrain(ElementID=ElementID,GaussPointID=GaussPointID),&
                        dStrain=dStrain,&
                        CauchyStress=this%getCauchyStress(ElementID=ElementID,GaussPointID=GaussPointID),&
                        PlasticStrain=PlasticStrain,&
                        YieldParams=this%ep_domain(1)%YieldFunction_params(ElementID,:),&
                        PlasticParams=this%ep_domain(1)%PlasticPotential_params(ElementID,:),&
                        ElasticParams=this%ep_domain(1)%ElasticPotential_params(ElementID,:),&
                        epsilon=dble(1.0e-4) &
                        )

                    call this%setPlasticStrain(&
                        ElementID=ElementID,&
                        GaussPointID=GaussPointID,&
                        PlasticStrain=PlasticStrain)

                    call this%setCauchyStress(&
                        ElementID=ElementID,&
                        GaussPointID=GaussPointID,&
                        CauchyStress=CauchyStress)

                    call this%addStrain(&
                        ElementID=ElementID,&
                        GaussPointID=GaussPointID,&
                        dStrain=dStrain)
                enddo
            enddo
            !$OMP end do
            !$OMP end parallel 
        endif

        ret = zeros(this%ep_domain(1)%femdomain%nn()*this%ep_domain(1)%femdomain%nd() )
        !$OMP parallel default(shared) private(GaussPointID,sf,Bmat,StressVector,Te)
        !$OMP do reduction(+:ret)
        ! Perform gauss integral to compute ret(:)
        do ElementID = 1, this%ep_domain(1)%femdomain%ne()
            do GaussPointID = 1, this%ep_domain(1)%femdomain%ngp()
                ! get shape function
			    sf = this%ep_domain(1)%FEMdomain%getShapeFunction(&
                    ElementID=ElementID,GaussPointID=GaussPointID)
                ! get B-matrix
			    Bmat = this%ep_domain(1)%FEMdomain%BMatrix(&
                    shapefunction=sf,ElementID=ElementID)
                ! get Stress vector
                StressVector = this%ep_domain(1)%CauchyStress_field(:,GaussPointID,ElementID)

                Te = matmul(transpose(Bmat),StressVector)*sf%detJ

                ret = ret + &
                this%ep_domain(1)%FEMdomain%asGlobalVector(LocalVector=Te,ElementID=ElementID,&
                    DOF=this%ep_domain(1)%FEMdomain%nd() )

            enddo
        enddo
        !$OMP end do
        !$OMP end parallel 


    else
        print *, "[ERROR] get_internal_forceElastoPlasticity >> only for single-domain problem"
        stop
    endif

end function

! ###################################################


! ###################################################
! ###################################################

subroutine update_stress_for_incrementElastoPlasticity(this,dU) 
    class(ElastoPlasticity_),intent(inout) :: this
    real(real64),intent(in) :: dU(:)
    real(real64),allocatable :: ret(:),PlasticStrain(:,:),PlasticStrain_n(:,:),dStrain(:,:),dTe(:),&
        dStressVector(:),Bmat(:,:),dCauchyStress(:,:),CauchyStress(:,:),dU_mat(:,:),new_PlasticStrain(:,:)
    type(ShapeFunction_) :: sf
        
    integer(int32) :: ElementID,GaussPointID,num_node,i,j

    num_node = size(dU)/3
    dU_mat = zeros( num_node, 3 )
    do i=1,num_node
        do j=1,3
            dU_mat(i,j) = dU(  (i-1)*3 + j )
        enddo
    enddo


    if( size(this%ep_domain)==1)then
        if(norm(dU)/=0.0d0 )then
            ! Update stress and plastic strain
            !$OMP parallel default(shared) private(GaussPointID,PlasticStrain_n,new_PlasticStrain,dStrain,dCauchyStress)
            !$OMP do
            do ElementID = 1, this%ep_domain(1)%femdomain%ne()
                do GaussPointID = 1, this%ep_domain(1)%femdomain%ngp()

                    PlasticStrain_n = this%getPlasticStrain_n(ElementID=ElementID,GaussPointID=GaussPointID)
                    dStrain = this%ep_domain(1)%femdomain%getStrainTensor(&
                        ElementID=ElementID,GaussPointID=GaussPointID,&
                        displacement=dU_mat)

                    !print *, minval(dU_mat),maxval(dU_mat)
                    dCauchyStress = to_dStressTensor(&
                        YieldFunction=this%ep_domain(1)%YieldFunction ,&
                        PlasticPotential=this%ep_domain(1)%PlasticPotential,&
                        dStrain=dStrain,&
                        CauchyStress=this%getCauchyStress(ElementID=ElementID,GaussPointID=GaussPointID) &
                            + this%getdCauchyStress(ElementID=ElementID,GaussPointID=GaussPointID) ,&
                        PlasticStrain=PlasticStrain_n,&
                        YieldParams=this%ep_domain(1)%YieldFunction_params(ElementID,:),&
                        PlasticParams=this%ep_domain(1)%PlasticPotential_params(ElementID,:),&
                        ElasticParams=this%ep_domain(1)%ElasticPotential_params(ElementID,:),&
                        epsilon=dble(1.0e-4), &
                        new_PlasticStrain=new_PlasticStrain &
                        )

                    call this%setPlasticStrain(&
                        ElementID=ElementID,&
                        GaussPointID=GaussPointID,&
                        PlasticStrain=new_PlasticStrain)

                    !call this%setdCauchyStress(&
                    !    ElementID=ElementID,&
                    !    GaussPointID=GaussPointID,&
                    !    dCauchyStress=&
                    !    this%getdCauchyStress(ElementID=ElementID,GaussPointID=GaussPointID) &
                    !     + dCauchyStress)

                    call this%setdCauchyStress(&
                        ElementID=ElementID,&
                        GaussPointID=GaussPointID,&
                        dCauchyStress=&
                        this%getdCauchyStress(ElementID=ElementID,GaussPointID=GaussPointID) &
                         + dCauchyStress)

                    call this%setdStrain(&
                        ElementID=ElementID,&
                        GaussPointID=GaussPointID,&
                        dStrain=dStrain)
                

                enddo
            enddo
            !$OMP end do
            !$OMP end parallel 
        endif
        
    else
        print *, "[ERROR] get_internal_forceElastoPlasticity >> only for single-domain problem"
        stop
    endif

end subroutine

! ###################################################


function get_delta_internal_forceElastoPlasticity(this) result(ret)
    class(ElastoPlasticity_),intent(inout) :: this
    
    real(real64),allocatable :: ret(:),PlasticStrain(:,:),PlasticStrain_n(:,:),dStrain(:,:),dTe(:),&
        dStressVector(:),Bmat(:,:),dCauchyStress(:,:),CauchyStress(:,:),dU_mat(:,:),new_PlasticStrain(:,:)
    type(ShapeFunction_) :: sf
        
    integer(int32) :: ElementID,GaussPointID,num_node,i,j


    if( size(this%ep_domain)==1)then
        
        ret = zeros(this%ep_domain(1)%femdomain%nn()*this%ep_domain(1)%femdomain%nd() )
        !$OMP parallel default(shared) private(GaussPointID,sf,Bmat,dStressVector,dTe)
        !$OMP do reduction(+:ret)
        ! Perform gauss integral to compute ret(:)
        do ElementID = 1, this%ep_domain(1)%femdomain%ne()
            do GaussPointID = 1, this%ep_domain(1)%femdomain%ngp()
                ! get shape function
			    sf = this%ep_domain(1)%FEMdomain%getShapeFunction(&
                    ElementID=ElementID,GaussPointID=GaussPointID)
                ! get B-matrix
			    Bmat = this%ep_domain(1)%FEMdomain%BMatrix(&
                    shapefunction=sf,ElementID=ElementID)
                ! get Stress vector
                dStressVector = this%ep_domain(1)%dCauchyStress_field(:,GaussPointID,ElementID)

                dTe = matmul(transpose(Bmat),dStressVector)*sf%detJ

                ret = ret + &
                this%ep_domain(1)%FEMdomain%asGlobalVector(LocalVector=dTe,ElementID=ElementID,&
                    DOF=this%ep_domain(1)%FEMdomain%nd() )

            enddo
        enddo
        !$OMP end do
        !$OMP end parallel 

    else
        print *, "[ERROR] get_internal_forceElastoPlasticity >> only for single-domain problem"
        stop
    endif

end function

! ###################################################


! ##################################################
function getPlasticStrain_ElastoPlasticity(this,GaussPointID,ElementID) result(ret)
    class(ElastoPlasticity_),intent(in) :: this
    integer(int32),intent(in) :: GaussPointID,ElementID
    real(real64),allocatable :: ret(:,:),ret_vec(:)

    ret_vec = this%ep_domain(1)%PlasticStrain_field(:,GaussPointID,ElementID)
    ret = zeros(3,3)
    ret(1,1) = ret_vec(1)
    ret(2,2) = ret_vec(2)
    ret(3,3) = ret_vec(3)
    ret(1,2) = ret_vec(4)
    ret(2,3) = ret_vec(5)
    ret(1,3) = ret_vec(6)
    ret(2,1) = ret_vec(4)
    ret(3,2) = ret_vec(5)
    ret(3,1) = ret_vec(6)
end function
! ##################################################


! ##################################################
function getPlasticStrain_n_ElastoPlasticity(this,GaussPointID,ElementID) result(ret)
    class(ElastoPlasticity_),intent(in) :: this
    integer(int32),intent(in) :: GaussPointID,ElementID
    real(real64),allocatable :: ret(:,:),ret_vec(:)

    ret_vec = this%ep_domain(1)%PlasticStrain_field_n(:,GaussPointID,ElementID)
    ret = zeros(3,3)
    ret(1,1) = ret_vec(1)
    ret(2,2) = ret_vec(2)
    ret(3,3) = ret_vec(3)
    ret(1,2) = ret_vec(4)
    ret(2,3) = ret_vec(5)
    ret(1,3) = ret_vec(6)
    ret(2,1) = ret_vec(4)
    ret(3,2) = ret_vec(5)
    ret(3,1) = ret_vec(6)
end function
! ##################################################


! ##################################################
function getStrain_ElastoPlasticity(this,GaussPointID,ElementID) result(ret)
    class(ElastoPlasticity_),intent(in) :: this
    integer(int32),intent(in) :: GaussPointID,ElementID
    real(real64),allocatable :: ret(:,:),ret_vec(:)

    ret_vec = this%ep_domain(1)%Strain_field(:,GaussPointID,ElementID)
    ret = zeros(3,3)
    ret(1,1) = ret_vec(1)
    ret(2,2) = ret_vec(2)
    ret(3,3) = ret_vec(3)
    ret(1,2) = ret_vec(4)
    ret(2,3) = ret_vec(5)
    ret(1,3) = ret_vec(6)
    ret(2,1) = ret_vec(4)
    ret(3,2) = ret_vec(5)
    ret(3,1) = ret_vec(6)
end function
! ##################################################


! ##################################################
function getCauchyStress_ElastoPlasticity(this,GaussPointID,ElementID) result(ret)
    class(ElastoPlasticity_),intent(in) :: this
    integer(int32),intent(in) :: GaussPointID,ElementID
    real(real64),allocatable :: ret(:,:),ret_vec(:)

    ret_vec = this%ep_domain(1)%CauchyStress_field(:,GaussPointID,ElementID)
    ret = zeros(3,3)
    ret(1,1) = ret_vec(1)
    ret(2,2) = ret_vec(2)
    ret(3,3) = ret_vec(3)
    ret(1,2) = ret_vec(4)
    ret(2,3) = ret_vec(5)
    ret(1,3) = ret_vec(6)
    ret(2,1) = ret_vec(4)
    ret(3,2) = ret_vec(5)
    ret(3,1) = ret_vec(6)
end function
! ##################################################
! ##################################################
function getdCauchyStress_ElastoPlasticity(this,GaussPointID,ElementID) result(ret)
    class(ElastoPlasticity_),intent(in) :: this
    integer(int32),intent(in) :: GaussPointID,ElementID
    real(real64),allocatable :: ret(:,:),ret_vec(:)

    ret_vec = this%ep_domain(1)%dCauchyStress_field(:,GaussPointID,ElementID)
    ret = zeros(3,3)
    ret(1,1) = ret_vec(1)
    ret(2,2) = ret_vec(2)
    ret(3,3) = ret_vec(3)
    ret(1,2) = ret_vec(4)
    ret(2,3) = ret_vec(5)
    ret(1,3) = ret_vec(6)
    ret(2,1) = ret_vec(4)
    ret(3,2) = ret_vec(5)
    ret(3,1) = ret_vec(6)
end function
! ##################################################



subroutine setPlasticStrain_ElastoPlasticity(this,ElementID,GaussPointID,PlasticStrain) 
    class(ElastoPlasticity_),intent(inout) :: this
    integer(int32),intent(in) :: ElementID, GaussPointID
    real(real64),intent(in) :: PlasticStrain(:,:)

    this%ep_domain(1)%PlasticStrain_field(1,GaussPointID,ElementID) = PlasticStrain(1,1)
    this%ep_domain(1)%PlasticStrain_field(2,GaussPointID,ElementID) = PlasticStrain(2,2)
    this%ep_domain(1)%PlasticStrain_field(3,GaussPointID,ElementID) = PlasticStrain(3,3)
    this%ep_domain(1)%PlasticStrain_field(4,GaussPointID,ElementID) = PlasticStrain(1,2)
    this%ep_domain(1)%PlasticStrain_field(5,GaussPointID,ElementID) = PlasticStrain(2,3)
    this%ep_domain(1)%PlasticStrain_field(6,GaussPointID,ElementID) = PlasticStrain(1,3)
    

end subroutine

subroutine setCauchyStress_ElastoPlasticity(this,ElementID,GaussPointID,CauchyStress) 
    class(ElastoPlasticity_),intent(inout) :: this
    integer(int32),intent(in) :: ElementID, GaussPointID
    real(real64),intent(in) :: CauchyStress(:,:)

    this%ep_domain(1)%CauchyStress_field(1,GaussPointID,ElementID) = CauchyStress(1,1)
    this%ep_domain(1)%CauchyStress_field(2,GaussPointID,ElementID) = CauchyStress(2,2)
    this%ep_domain(1)%CauchyStress_field(3,GaussPointID,ElementID) = CauchyStress(3,3)
    this%ep_domain(1)%CauchyStress_field(4,GaussPointID,ElementID) = CauchyStress(1,2)
    this%ep_domain(1)%CauchyStress_field(5,GaussPointID,ElementID) = CauchyStress(2,3)
    this%ep_domain(1)%CauchyStress_field(6,GaussPointID,ElementID) = CauchyStress(1,3)
    

end subroutine


subroutine setdCauchyStress_ElastoPlasticity(this,ElementID,GaussPointID,dCauchyStress) 
    class(ElastoPlasticity_),intent(inout) :: this
    integer(int32),intent(in) :: ElementID, GaussPointID
    real(real64),intent(in) :: dCauchyStress(:,:)

    this%ep_domain(1)%dCauchyStress_field(1,GaussPointID,ElementID) = dCauchyStress(1,1)
    this%ep_domain(1)%dCauchyStress_field(2,GaussPointID,ElementID) = dCauchyStress(2,2)
    this%ep_domain(1)%dCauchyStress_field(3,GaussPointID,ElementID) = dCauchyStress(3,3)
    this%ep_domain(1)%dCauchyStress_field(4,GaussPointID,ElementID) = dCauchyStress(1,2)
    this%ep_domain(1)%dCauchyStress_field(5,GaussPointID,ElementID) = dCauchyStress(2,3)
    this%ep_domain(1)%dCauchyStress_field(6,GaussPointID,ElementID) = dCauchyStress(1,3)
    

end subroutine


subroutine addStrain_ElastoPlasticity(this,ElementID,GaussPointID,dStrain) 
    class(ElastoPlasticity_),intent(inout) :: this
    integer(int32),intent(in) :: ElementID, GaussPointID
    real(real64),intent(in) :: dStrain(:,:)

    this%ep_domain(1)%Strain_field(1,GaussPointID,ElementID) = this%ep_domain(1)%Strain_field(1,GaussPointID,ElementID) &
        + dStrain(1,1)
    this%ep_domain(1)%Strain_field(2,GaussPointID,ElementID) = this%ep_domain(1)%Strain_field(2,GaussPointID,ElementID) &
        + dStrain(2,2)
    this%ep_domain(1)%Strain_field(3,GaussPointID,ElementID) = this%ep_domain(1)%Strain_field(3,GaussPointID,ElementID) &
        + dStrain(3,3)
    this%ep_domain(1)%Strain_field(4,GaussPointID,ElementID) = this%ep_domain(1)%Strain_field(4,GaussPointID,ElementID) &
        + dStrain(1,2)
    this%ep_domain(1)%Strain_field(5,GaussPointID,ElementID) = this%ep_domain(1)%Strain_field(5,GaussPointID,ElementID) &
        + dStrain(2,3)
    this%ep_domain(1)%Strain_field(6,GaussPointID,ElementID) = this%ep_domain(1)%Strain_field(6,GaussPointID,ElementID) &
        + dStrain(1,3)
    

end subroutine

subroutine setdStrain_ElastoPlasticity(this,ElementID,GaussPointID,dStrain) 
    class(ElastoPlasticity_),intent(inout) :: this
    integer(int32),intent(in) :: ElementID, GaussPointID
    real(real64),intent(in) :: dStrain(:,:)

    this%ep_domain(1)%dStrain_field(1,GaussPointID,ElementID)  = dStrain(1,1)
    this%ep_domain(1)%dStrain_field(2,GaussPointID,ElementID)  = dStrain(2,2)
    this%ep_domain(1)%dStrain_field(3,GaussPointID,ElementID)  = dStrain(3,3)
    this%ep_domain(1)%dStrain_field(4,GaussPointID,ElementID)  = dStrain(1,2)
    this%ep_domain(1)%dStrain_field(5,GaussPointID,ElementID)  = dStrain(2,3)
    this%ep_domain(1)%dStrain_field(6,GaussPointID,ElementID)  = dStrain(1,3)
    

end subroutine


! ###################################################

function get_external_forceElastoPlasticity(this) result(ret)
    class(ElastoPlasticity_),intent(inout) :: this
    real(real64),allocatable :: ret(:)

    ret = this%femsolver%CRS_RHS(:)

end function

! ###################################################


! ###################################################

subroutine fill_zero_at_DBCElastoPlasticity(this,values,idx) 
    class(ElastoPlasticity_),intent(inout) :: this
    real(real64),intent(inout) :: values(:)
    integer(int32),intent(in) :: idx(:)

    values(idx(:) ) = 0.0d0


end subroutine

! ###################################################



! ###################################################
subroutine exportElastoPlasticity(this,name,step,amp)
    class(ElastoPlasticity_),intent(inout) :: this
    character(*),intent(in) :: name
    real(real64),optional,intent(in) :: amp
    real(real64) :: mag
    integer(int32),optional,intent(in) :: step
    integer(int32) :: i,n

    mag = input(default=1.0d0,option=amp)
    do i=1,size(this%ep_domain)
        call this%ep_domain(i)%femdomain%deform(disp=mag*this%ep_domain(i)%displacement)
    enddo
    if(present(step) )then
        do i=1,size(this%ep_domain)
            call this%ep_domain(i)%femdomain%vtk( name // "_domain_"// zfill(i,5)&
                 //"_step_"//zfill(step,5) )
            call this%ep_domain(i)%femdomain%vtk( name // "_domain_"// zfill(i,5)&
                 // "_s11" //"_step_"//zfill(step,5),&
                 scalar=this%ep_domain(1)%CauchyStress_field(1,1,:) )
            call this%ep_domain(i)%femdomain%vtk( name // "_domain_"// zfill(i,5)&
                 // "_s22" //"_step_"//zfill(step,5),&
                 scalar=this%ep_domain(1)%CauchyStress_field(2,1,:) )
            call this%ep_domain(i)%femdomain%vtk( name // "_domain_"// zfill(i,5)&
                 // "_s33" //"_step_"//zfill(step,5),&
                 scalar=this%ep_domain(1)%CauchyStress_field(3,1,:) )
            call this%ep_domain(i)%femdomain%vtk( name // "_domain_"// zfill(i,5)&
                 // "_s12" //"_step_"//zfill(step,5),&
                 scalar=this%ep_domain(1)%CauchyStress_field(4,1,:) )
            call this%ep_domain(i)%femdomain%vtk( name // "_domain_"// zfill(i,5)&
                 // "_s23" //"_step_"//zfill(step,5),&
                 scalar=this%ep_domain(1)%CauchyStress_field(5,1,:) )
            call this%ep_domain(i)%femdomain%vtk( name // "_domain_"// zfill(i,5)&
                 // "_s13" //"_step_"//zfill(step,5),&
                 scalar=this%ep_domain(1)%CauchyStress_field(6,1,:) )

            call this%ep_domain(i)%femdomain%vtk( name // "_domain_"// zfill(i,5)&
                 // "_I1" //"_step_"//zfill(step,5),&
                 scalar=this%I1() )
            call this%ep_domain(i)%femdomain%vtk( name // "_domain_"// zfill(i,5)&
                 // "_J2" //"_step_"//zfill(step,5),&
                 scalar=this%J2() )

            call this%ep_domain(i)%femdomain%vtk( name // "_domain_"// zfill(i,5)&
                 // "_eI1" //"_step_"//zfill(step,5),&
                 scalar=this%I1_e() )
            call this%ep_domain(i)%femdomain%vtk( name // "_domain_"// zfill(i,5)&
                 // "_eJ2" //"_step_"//zfill(step,5),&
                 scalar=this%J2_e() )
        enddo
    else
        do i=1,size(this%ep_domain)
            call this%ep_domain(i)%femdomain%vtk( name // "_domain_"// zfill(i,5)&
                 )
            call this%ep_domain(i)%femdomain%vtk( name // "_domain_"// zfill(i,5)&
                 // "_s11",scalar=this%ep_domain(1)%CauchyStress_field(1,1,:) )
            call this%ep_domain(i)%femdomain%vtk( name // "_domain_"// zfill(i,5)&
                 // "_s22",scalar=this%ep_domain(1)%CauchyStress_field(2,1,:) )
            call this%ep_domain(i)%femdomain%vtk( name // "_domain_"// zfill(i,5)&
                 // "_s33",scalar=this%ep_domain(1)%CauchyStress_field(3,1,:) )
            call this%ep_domain(i)%femdomain%vtk( name // "_domain_"// zfill(i,5)&
                 // "_s12",scalar=this%ep_domain(1)%CauchyStress_field(4,1,:) )
            call this%ep_domain(i)%femdomain%vtk( name // "_domain_"// zfill(i,5)&
                 // "_s23",scalar=this%ep_domain(1)%CauchyStress_field(5,1,:) )
            call this%ep_domain(i)%femdomain%vtk( name // "_domain_"// zfill(i,5)&
                 // "_s13",scalar=this%ep_domain(1)%CauchyStress_field(6,1,:) )

            call this%ep_domain(i)%femdomain%vtk( name // "_domain_"// zfill(i,5)&
                 // "_I1",scalar=this%I1() )
            call this%ep_domain(i)%femdomain%vtk( name // "_domain_"// zfill(i,5)&
                 // "_J2",scalar=this%J2() )

            call this%ep_domain(i)%femdomain%vtk( name // "_domain_"// zfill(i,5)&
                 // "_eI1",scalar=this%I1_e() )
            call this%ep_domain(i)%femdomain%vtk( name // "_domain_"// zfill(i,5)&
                 // "_eJ2",scalar=this%J2_e() )
        enddo
    endif

    call this%exportField(name=name)

    do i=1,size(this%ep_domain)
        call this%ep_domain(i)%femdomain%deform(disp=-mag*this%ep_domain(i)%displacement)
    enddo
end subroutine
! ###################################################


! ###################################################
function I1ElastoPlasticity(this) result(I1)
    class(ElastoPlasticity_),intent(inout) :: this
    real(real64),allocatable :: I1(:),stress(:,:)
    integer(int32) :: i,j
    Stress = zeros(3,3)
    I1 = zeros(this%ep_domain(1)%femdomain%ne())
    do i=1,this%ep_domain(1)%femdomain%ne()
        stress = 0.0d0
        do j=1,this%ep_domain(1)%femdomain%ngp()
            stress(1,1) = stress(1,1) + this%ep_domain(1)%CauchyStress_field(1,j,i)
            stress(2,2) = stress(2,2) + this%ep_domain(1)%CauchyStress_field(2,j,i)
            stress(3,3) = stress(3,3) + this%ep_domain(1)%CauchyStress_field(3,j,i)
            stress(1,2) = stress(1,2) + this%ep_domain(1)%CauchyStress_field(4,j,i)
            stress(2,3) = stress(2,3) + this%ep_domain(1)%CauchyStress_field(5,j,i)
            stress(1,3) = stress(1,3) + this%ep_domain(1)%CauchyStress_field(6,j,i)
        enddo
        I1(i) = to_I1(stress)
    enddo

end function
! ###################################################
function J2ElastoPlasticity(this) result(J2)
    class(ElastoPlasticity_),intent(inout) :: this
    real(real64),allocatable :: J2(:),stress(:,:)
    integer(int32) :: i,j
    Stress = zeros(3,3)
    J2 = zeros(this%ep_domain(1)%femdomain%ne())
    do i=1,this%ep_domain(1)%femdomain%ne()
        stress = 0.0d0
        do j=1,this%ep_domain(1)%femdomain%ngp()
            stress(1,1) = stress(1,1) + this%ep_domain(1)%CauchyStress_field(1,j,i)
            stress(2,2) = stress(2,2) + this%ep_domain(1)%CauchyStress_field(2,j,i)
            stress(3,3) = stress(3,3) + this%ep_domain(1)%CauchyStress_field(3,j,i)
            stress(1,2) = stress(1,2) + this%ep_domain(1)%CauchyStress_field(4,j,i)
            stress(2,3) = stress(2,3) + this%ep_domain(1)%CauchyStress_field(5,j,i)
            stress(1,3) = stress(1,3) + this%ep_domain(1)%CauchyStress_field(6,j,i)
        enddo
        J2(i) = to_J2(stress)
    enddo

end function

! ###################################################
function I1_e_ElastoPlasticity(this) result(I1)
    class(ElastoPlasticity_),intent(inout) :: this
    real(real64),allocatable :: I1(:),Strain(:,:)
    integer(int32) :: i,j

    Strain = zeros(3,3)
    I1 = zeros(this%ep_domain(1)%femdomain%ne())
    do i=1,this%ep_domain(1)%femdomain%ne()
        Strain = 0.0d0
        do j=1,this%ep_domain(1)%femdomain%ngp()
            Strain(1,1) = Strain(1,1) + this%ep_domain(1)%Strain_field(1,j,i)
            Strain(2,2) = Strain(2,2) + this%ep_domain(1)%Strain_field(2,j,i)
            Strain(3,3) = Strain(3,3) + this%ep_domain(1)%Strain_field(3,j,i)
            Strain(1,2) = Strain(1,2) + this%ep_domain(1)%Strain_field(4,j,i)
            Strain(2,3) = Strain(2,3) + this%ep_domain(1)%Strain_field(5,j,i)
            Strain(1,3) = Strain(1,3) + this%ep_domain(1)%Strain_field(6,j,i)
        enddo
        I1(i) = to_I1(Strain)
    enddo

end function
! ###################################################
function J2_e_ElastoPlasticity(this) result(J2)
    class(ElastoPlasticity_),intent(inout) :: this
    real(real64),allocatable :: J2(:),Strain(:,:)
    integer(int32) :: i,j

    Strain = zeros(3,3)
    J2 = zeros(this%ep_domain(1)%femdomain%ne())
    do i=1,this%ep_domain(1)%femdomain%ne()
        Strain = 0.0d0
        do j=1,this%ep_domain(1)%femdomain%ngp()
            Strain(1,1) = Strain(1,1) + this%ep_domain(1)%Strain_field(1,j,i)
            Strain(2,2) = Strain(2,2) + this%ep_domain(1)%Strain_field(2,j,i)
            Strain(3,3) = Strain(3,3) + this%ep_domain(1)%Strain_field(3,j,i)
            Strain(1,2) = Strain(1,2) + this%ep_domain(1)%Strain_field(4,j,i)
            Strain(2,3) = Strain(2,3) + this%ep_domain(1)%Strain_field(5,j,i)
            Strain(1,3) = Strain(1,3) + this%ep_domain(1)%Strain_field(6,j,i)
        enddo
        J2(i) = to_J2(Strain)
    enddo

end function
! ##################################################

function getTractionForce_ElastoPlasticity(this,NodeList) result(ret)
    class(ElastoPlasticity_),intent(inout) :: this
    integer(int32),intent(in) :: NodeList(:)
    real(real64),allocatable :: t(:)
    real(real64) :: ret(1:3) ! kN
    integer(int32) :: nn,i
    
    nn = 0
    do i=1,size(this%ep_domain)
        nn = nn + this%ep_domain(i)%femdomain%nn()
    enddo

    t = this%get_internal_force(dU=zeros(nn*3 ) ) 

    ret(1) = sum(t(3*NodeList(:)-2))
    ret(2) = sum(t(3*NodeList(:)-1))
    ret(3) = sum(t(3*NodeList(:)-0))

end function
! ######################################################

! ######################################################
subroutine rezeroElastoPlasticity(this)
    class(ElastoPlasticity_),intent(inout) :: this
    integer(int32) :: i

    do i=1,size(this%ep_domain)
        this%ep_domain(i)%CauchyStress_field = 0.0d0
        this%ep_domain(i)%Strain_field = 0.0d0
        this%ep_domain(i)%PlasticStrain_field = 0.0d0
        this%ep_domain(i)%displacement = 0.0d0
    enddo

end subroutine
! ######################################################

subroutine exportFieldElastoPlasticity(this,name)
    class(ElastoPlasticity_),intent(in) :: this
    character(*),intent(in) :: name
    integeR(int32):: DomainID
    
    if(.not.allocated(this%ep_domain) )then
        return
    else
        do DomainID=1,size(this%ep_domain)
            call this%ep_domain(DomainID)%exportField(name+"_"+str(DomainID)+"_")
        enddo
    endif
end subroutine
! ######################################################

subroutine importFieldElastoPlasticity(this,name,num_domain)
    class(ElastoPlasticity_),intent(inout) :: this
    character(*),intent(in) :: name
    integer(int32),intent(in) :: num_domain
    integeR(int32):: DomainID

    if(.not.allocated(this%ep_domain) ) then
        allocate(this%ep_domain(DomainID) )
    endif

    do DomainID=1,size(this%ep_domain)
        call this%ep_domain(DomainID)%importField(name+"_"+str(DomainID)+"_")
    enddo
    
end subroutine
! ######################################################

subroutine exportFieldEpDomain(this,name)
    class(EP_Domain_),intent(in) :: this
    character(*),intent(in)  :: name
    type(IO_) :: f

    call to_csv(name+"_CauchyStress_field",this%CauchyStress_field)
    call to_csv(name+"_displacement"      ,this%displacement)

end subroutine
! ######################################################

! ######################################################

subroutine importFieldEpDomain(this,name)
    class(EP_Domain_),intent(inout) :: this
    character(*),intent(in)  :: name
    type(IO_) :: f
    integer(int32) :: n1,n2,n3

    n1 = 6
    n2 = this%femdomain%ngp()
    n3 = this%femdomain%ne()
    this%CauchyStress_field = from_csv(name+"_CauchyStress_field",n1,n2,n3)
    
    n1 = this%femdomain%nd()*this%femdomain%nn()
    this%displacement       = from_csv(name+"_displacement",n1)

end subroutine

end module ElastoPlasticityClass

