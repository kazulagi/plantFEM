module StressClass
    use MathClass
    use StrainClass
    implicit none

    type :: Stress_
        
        ! hyper
        real(8),allocatable :: sigma(:,:)
        real(8),allocatable :: sigma_n(:,:)
        real(8),allocatable :: S(:,:)
        real(8),allocatable :: P(:,:)

        ! derivatives
        real(8),allocatable :: dSdC(:,:,:,:)
        
        ! hypo
        real(8),allocatable :: sigma_dot(:,:)
        real(8),allocatable :: sigma_j(:,:)
        real(8),allocatable :: sigma_o(:,:)
        real(8),allocatable :: sigma_t(:,:)

        ! derivatives (dsigma/deps)
        real(8),allocatable :: E(:,:,:,:)

        integer :: TheoryID
        
        character*40 :: StrainTheory
        
        ! Please input one of following keyphrases

        ! 1 : Finite_Elasticity
        ! 2 : Finite_ElastoPlasticity
        ! 3 : Infinitesimal_Elasticity
        ! 4 : Infinitesimal_ElastoPlasticity
        ! 5 : Small_strain

    contains
        procedure,public :: init        => initStress
        procedure,public :: getRate     => getStressRate  
        procedure,public :: getStress   => getStress 
        procedure,public :: getDerivative => getStressDerivative
        procedure,public :: delete      => deleteStress
    end type
contains


! ###############################
subroutine initStress(obj,StrainTheory)
    class(Stress_),intent(inout) :: obj
    character(*),intent(in) :: StrainTheory
    real(8) :: delta(3,3)

    delta(:,:)=0.0d0
    delta(1,1)=1.0d0
    delta(2,2)=1.0d0
    delta(3,3)=1.0d0

    if(allocated(obj%sigma) )then
        call obj%delete()
    endif

    obj%StrainTheory=StrainTheory

    if(    trim(obj%StrainTheory)=="Finite_Elasticity")then
        obj%theoryID=1

        ! hyper
        allocate(obj%sigma(3,3) )
        allocate(obj%sigma_n(3,3) )
        allocate(obj%S(3,3) )
        allocate(obj%P(3,3) )

        ! hypo
        allocate(obj%sigma_dot(0,0) )
        allocate(obj%sigma_j(0,0) )
        allocate(obj%sigma_o(0,0) )
        allocate(obj%sigma_t(0,0) )

        obj%sigma(:,:)  = 0.0d0
        obj%sigma_n(:,:)  = 0.0d0
        obj%S(:,:)      = 0.0d0
        obj%P(:,:)      = 0.0d0

    elseif(trim(obj%StrainTheory)=="Finite_ElastoPlasticity")then
        obj%theoryID=2

        ! hyper
        allocate(obj%sigma(3,3) )
        allocate(obj%sigma_n(3,3) )
        allocate(obj%S(3,3) )
        allocate(obj%P(3,3) )

        ! hypo
        allocate(obj%sigma_dot(0,0) )
        allocate(obj%sigma_j(0,0) )
        allocate(obj%sigma_o(0,0) )
        allocate(obj%sigma_t(0,0) )


        obj%sigma(:,:)  = 0.0d0
        obj%sigma_n(:,:)  = 0.0d0
        obj%S(:,:)      = 0.0d0
        obj%P(:,:)      = 0.0d0
        
    elseif(trim(obj%StrainTheory)=="Infinitesimal_Elasticity")then
        obj%theoryID=3

        ! hyper
        allocate(obj%sigma(3,3) )
        allocate(obj%sigma_n(3,3) )
        allocate(obj%S(0,0) )
        allocate(obj%P(0,0) )

        ! hypo
        allocate(obj%sigma_dot(3,3) )
        allocate(obj%sigma_j(3,3) )
        allocate(obj%sigma_o(3,3) )
        allocate(obj%sigma_t(3,3) )


        obj%sigma(:,:)  = 0.0d0
        obj%sigma_n(:,:)  = 0.0d0
        obj%sigma_dot(:,:) = 0.0d0
        obj%sigma_j(:,:) = 0.0d0
        obj%sigma_o(:,:) = 0.0d0
        obj%sigma_t(:,:) = 0.0d0
        
    elseif(trim(obj%StrainTheory)=="Infinitesimal_ElastoPlasticity")then
        obj%theoryID=4

        ! hyper
        allocate(obj%sigma(3,3) )
        allocate(obj%sigma_n(3,3) )
        allocate(obj%S(0,0) )
        allocate(obj%P(0,0) )

        ! hypo
        allocate(obj%sigma_dot(3,3) )
        allocate(obj%sigma_j(3,3) )
        allocate(obj%sigma_o(3,3) )
        allocate(obj%sigma_t(3,3) )

        obj%sigma(:,:)  = 0.0d0
        obj%sigma_n(:,:)  = 0.0d0
        obj%sigma_dot(:,:) = 0.0d0
        obj%sigma_j(:,:) = 0.0d0
        obj%sigma_o(:,:) = 0.0d0
        obj%sigma_t(:,:) = 0.0d0

    elseif(trim(obj%StrainTheory)=="Small_strain")then
        obj%theoryID=5

        ! hyper
        allocate(obj%sigma(3,3) )
        allocate(obj%sigma_n(3,3) )
        allocate(obj%S(0,0) )
        allocate(obj%P(0,0) )

        ! hypo
        allocate(obj%sigma_dot(0,0) )
        allocate(obj%sigma_j(0,0) )
        allocate(obj%sigma_o(0,0) )
        allocate(obj%sigma_t(0,0) )

        obj%sigma(:,:)  = 0.0d0
        obj%sigma_n(:,:)  = 0.0d0

    else
        print *, trim(StrainTheory)
        print *, "Please input one of following keyphrases"
        print *, " ---->"
        print *, "Finite_Elasticity"
        print *, "Finite_ElastoPlasticity"
        print *, "Infinitesimal_Elasticity"
        print *, "Infinitesimal_ElastoPlasticity"
        print *, "Small_strain"
        print *, " <----"
    endif


end subroutine
! ###############################


! ###############################
subroutine deleteStress(obj)
    class(Stress_),intent(inout) :: obj

        ! hyper
    deallocate(obj%sigma )
    deallocate(obj%sigma_n )
    deallocate(obj%S )
    deallocate(obj%P )
    
    ! hypo
    deallocate(obj%sigma_dot )
    deallocate(obj%sigma_j )
    deallocate(obj%sigma_o )
    deallocate(obj%sigma_t )


    obj%TheoryID = 0
    
    obj%StrainTheory = " "
    

end subroutine
! ###############################


! ###############################
subroutine getStressRate(obj,Strain,Type)
    class(Stress_),intent(inout) :: obj
    class(Strain_),intent(inout) :: strain
    character(*),intent(in) :: Type

    if(trim(Type) == "Jaumann" )then
        obj%sigma_j = obj%sigma_dot + matmul(obj%sigma, strain%w) - matmul(strain%w, obj%sigma) 
    elseif(trim(Type) == "Oldroyd" )then
        obj%sigma_o = obj%sigma_dot + matmul(strain%l, obj%sigma) + matmul(obj%sigma, transpose(strain%l) )  
    elseif(trim(Type) == "Truesdell" )then
        obj%sigma_t = obj%sigma_dot - matmul(strain%l, obj%sigma) &
            - matmul(obj%sigma, transpose(strain%l) ) + trace(strain%l)*obj%sigma
    else
        print *, "ERROR :: getStressRate :: invalid stress rate",trim(Type)
        return
    endif

end subroutine
! ###############################


! ###############################
subroutine getStress(obj,Strain,Type,lambda,mu,K,G,c,phi)
    class(Stress_),intent(inout) :: obj
    class(Strain_),intent(inout) :: strain
    character(*),intent(in) :: Type
    real(8),optional,intent(in) :: lambda,mu,K,G,c,phi
    real(8),allocatable :: F_inv(:,:),Cp_inv(:,:),delta(:,:),C_inv(:,:),M(:,:)
    real(8) :: detC,detCp,f,J2_M,I1_M,theta_M,BI,fc,f_MC

    
    if(size(Strain%F,1)==3 )then
        ! Finite Strain Theory
        allocate( F_inv(3,3),Cp_inv(3,3),C_inv(3,3),delta(3,3),M(3,3) )
        call inverse_rank_2(strain%F,F_inv )
        call inverse_rank_2(strain%Cp,Cp_inv )
        call inverse_rank_2(strain%C,C_inv )
        detC=strain%detF*strain%detF
        detCp=det_mat(strain%Cp,size(strain%Cp,1) )
        delta(:,:)=0.0d0
        delta(1,1)=1.0d0
        delta(2,2)=1.0d0
        delta(3,3)=1.0d0
        
        if(trim(Type) == "StVenant" )then
            ! Conventional St.Venant 
            obj%S(:,:)=lambda*0.50d0*trace(strain%C - delta )*delta(:,:)+mu*(strain%C(:,:)-delta(:,:) )
            obj%sigma(:,:) = 1.0d0/strain%detF*( matmul(strain%F, matmul(obj%S, transpose(strain%F)  ) ) )
        elseif(trim(Type) == "StVenant_Kirchhoff" )then
            ! St.Venant-Kirchhoff (Wallin and Ristinmaa, 2005 )
            print *, "Finite Strain St.Venant-Kirchhoff will be implemented soon."
        elseif(trim(Type) == "NeoHookean" )then
            ! Conventional Neo-Hookean
            obj%S(:,:)=lambda*log(strain%detF)*C_inv(:,:)+mu*(delta(:,:) - C_inv(:,:)  )
            obj%sigma(:,:) = 1.0d0/strain%detF*( matmul(strain%F, matmul(obj%S, transpose(strain%F)  ) ) )
        elseif(trim(Type) == "MCDP" )then
            ! elastic part is Modified Neo-Hookean (Vladimirov, 2008; 2010)
            obj%S(:,:)=lambda*0.50d0*(detC/detCp - 1.0d0 )*C_inv(:,:)&
                - mu*(Cp_inv(:,:) - C_inv(:,:) )
            obj%sigma(:,:) = 1.0d0/strain%detF*( matmul(strain%F, matmul(obj%S, transpose(strain%F)  ) ) )

            ! Check yield criterion (Mohr-Coulomb)
            M(:,:)=matmul(strain%C,obj%S)
            J2_M    = invariant_J2(M)
            I1_M    = invariant_I1(M)
            theta_M = invariant_theta(M)
            BI      = (1.0d0+sin(phi) )/(1.0d0 - sin(phi) )
            fc      = (2.0d0*c *cos(phi) )/(1.0d0- sin(phi) )
            f_MC=sqrt(J2_M) + ( (BI-1)*I1_M-3.0d0*fc )/( 3.0d0*(BI+1.0d0)*cos(theta_M) + sqrt(BI-1.0d0)*sin(theta_M)*fc )

            ! Return-mapping

        elseif(trim(Type) == "CamClay" )then
            print *, "FeFp Cam-clay will be implemented soon."
            !obj%S(:,:)=P0*( 1.0d0/ )
            !obj%sigma(:,:) = 1.0d0/obj%detF*( matmul(obj%F, matmul(obj%S, transpose(obj%F)  ) ) )
        else
            print *, "ERROR :: getStressFinitestrain :: invalid stress rate",trim(Type)
            return
        endif
    else
        if(size(obj%sigma_t,1)==3 )then
            ! Infinitesimal strain theory
            if(trim(Type) == "LinearElastic" )then
                
            elseif(trim(Type) == "MCDP" )then
            
            elseif(trim(Type) == "CamClay" )then
    
            else
                print *, "ERROR :: getStressInfinitesimal :: invalid stress rate",trim(Type)
                return
            endif
        else
            ! Small strain
            if(trim(Type) == "LinearElastic" )then
            
            elseif(trim(Type) == "MCDP" )then
            
            elseif(trim(Type) == "CamClay" )then
    
            else
                print *, "ERROR :: getStressSmallStrain :: invalid stress rate",trim(Type)
                return
            endif
        endif
    endif

end subroutine
! ###############################


! ###############################
subroutine getStressDerivative(obj,Strain,Type,lambda,mu)
    class(Stress_),intent(inout) :: obj
    class(Strain_),intent(inout) :: strain
    character(*),intent(in) :: Type
    real(8),optional,intent(in) :: lambda,mu
    real(8),allocatable :: F_inv(:,:)

    
    if(size(Strain%F,1)==3 )then
        ! Finite Strain Theory
        allocate( F_inv(3,3))
        call inverse_rank_2(strain%F,F_inv )
        if(trim(Type) == "StVenant" )then

        elseif(trim(Type) == "NeoHookean" )then
        
        elseif(trim(Type) == "MCDP" )then
        
        elseif(trim(Type) == "CamClay" )then

        else
            print *, "ERROR :: getStressFinitestrain :: invalid stress rate",trim(Type)
            return
        endif
    else
        if(size(obj%sigma_t,1)==3 )then
            ! Infinitesimal strain theory
            if(trim(Type) == "LinearElastic" )then
            
            elseif(trim(Type) == "MCDP" )then
            
            elseif(trim(Type) == "CamClay" )then
    
            else
                print *, "ERROR :: getStressInfinitesimal :: invalid stress rate",trim(Type)
                return
            endif
        else
            ! Small strain
            if(trim(Type) == "LinearElastic" )then
            
            elseif(trim(Type) == "MCDP" )then
            
            elseif(trim(Type) == "CamClay" )then
    
            else
                print *, "ERROR :: getStressSmallStrain :: invalid stress rate",trim(Type)
                return
            endif
        endif
    endif

end subroutine
! ###############################

end module 