program main
    use IOClass
    use ElastoPlasticityClass
    implicit none

    type(ElastoPlasticity_) :: ep
    real(real64),allocatable :: sigma(:,:)
    real(real64) :: c, phi, pc, k
    type(Random_) :: random

    sigma = eyes(3,3)
    !sigma(1,1) = -1.0d0 - 10.0d0*random%random()! kPa
    !sigma(2,2) = -1.0d0 - 10.0d0*random%random()! kPa
    !sigma(3,3) = -1.0d0 - 10.0d0*random%random()! kPa
    sigma(1,1) = -1.0d0 
    sigma(2,2) = -3.0d0 
    sigma(3,3) = -3.0d0 
    
    
    print *, ">>>> stress nvariants >>>>>"
    call print("I1")
    call print(to_I1(sigma) )
    call print("J2")
    call print(to_J2(sigma) )
    call print("J3")
    call print(to_J3(sigma) )
    call print("\theta (rad.)")
    call print(to_LodeAngle(sigma) )
    call print("\theta (deg.)")
    call print(degrees(to_LodeAngle(sigma) ))
    call print("s_{ij}")
    call print(to_DeviatricStress(sigma) )
    
    c   = 1.0d0
    phi = radian(30.0d0)
    pc = 10.0d0
    print *, ">>>> Yield functions >>>>>"
    call print("Mohr-Coulomb")
    call print( MohrCoulomb(&
                CauchyStress=sigma,&
                PlasticStrain=zeros(3,3),&
                params=[c, phi] )&
                )
    call print( is_elastic(MohrCoulomb(&
                CauchyStress=sigma,&
                PlasticStrain=zeros(3,3),&
                params=[c, phi] )&
                ))

    call print("Drucker-Prager")
    call print( DruckerPrager(&
                CauchyStress=sigma,&
                PlasticStrain=zeros(3,3),&
                params=[c, phi] )&
                )
    call print( is_elastic(DruckerPrager(&
                CauchyStress=sigma,&
                PlasticStrain=zeros(3,3),&
                params=[c, phi] )&
                ))
    k = 1.0d0 
    call print("von Mises")
    call print( VonMises(&
                CauchyStress=sigma,&
                PlasticStrain=zeros(3,3),&
                params=[k] )&
                )
    call print( is_elastic(VonMises(&
                CauchyStress=sigma,&
                PlasticStrain=zeros(3,3),&
                params=[k] )&
                ))

    call print("Tresca")
    print *, Tresca(&
                CauchyStress=sigma,&
                PlasticStrain=zeros(3,3),&
                params=[k] )
    print *, is_elastic(Tresca(&
                CauchyStress=sigma,&
                PlasticStrain=zeros(3,3),&
                params=[k] ))

    call print("Cam-clay")
    print *, Camclay(&
                CauchyStress=sigma,&
                PlasticStrain=zeros(3,3),&
                params=[c,phi,pc] )
    print *, is_elastic(Camclay(&
                CauchyStress=sigma,&
                PlasticStrain=zeros(3,3),&
                params=[c,phi,pc] ))

    call print("Mofdified Cam-clay")
    print *, ModifiedCamclay(&
                CauchyStress=sigma,&
                PlasticStrain=zeros(3,3),&
                params=[c,phi,pc] )
    print *, is_elastic(ModifiedCamclay(&
                CauchyStress=sigma,&
                PlasticStrain=zeros(3,3),&
                params=[c,phi,pc] ))
    
    print *, ">>>> df/d{\sigma} >>>>>"
    call print("Mohr-Coulomb")
    call print(d_dsigma(&
            PlasticPotential=MohrCoulomb, &
            CauchyStress=sigma, &
            PlasticStrain=zeros(3,3),&
            params=[c, phi], &
            epsilon=dble(1.0e-4) &
            ))
    call print("von Mises")
    call print(d_dsigma(&
            PlasticPotential=VonMises, &
            CauchyStress=sigma, &
            PlasticStrain=zeros(3,3),&
            params=[c, phi], &
            epsilon=dble(1.0e-4) &
            ))
    print *, "Template for user-defined yield functions >> my_yield_func.f90"
    call ep%getYieldFunctionTemplate("my_yield_func")
    
end program main
