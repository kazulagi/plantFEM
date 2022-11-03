program main
    use IOClass
    use ElastoPlasticityClass
    implicit none

    real(real64),allocatable :: sigma(:,:)
    real(real64) :: c, phi, k
    type(Random_) :: random

    sigma = eyes(3,3)
    sigma(1,1) = -1.0d0 - random%random()
    sigma(2,2) = -1.0d0 - random%random()
    sigma(3,3) = -1.0d0 - random%random()
    

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
    
    c   = 0.0d0
    phi = radian(30.0d0)
    call print("Yield Function(MC)")
    call print( MohrCoulomb(&
                CauchyStress=sigma,&
                PlasticStrain=zeros(3,3),&
                params=[c, phi] )&
                )
    k = 1.0d0 
    call print("Yield Function(VonMises)")
    call print( VonMises(&
                CauchyStress=sigma,&
                PlasticStrain=zeros(3,3),&
                params=[k] )&
                )
    
    call print("Yield Function(Tresca)")
    print *, Tresca(&
                CauchyStress=sigma,&
                PlasticStrain=zeros(3,3),&
                params=[k] )
                
                
end program main
