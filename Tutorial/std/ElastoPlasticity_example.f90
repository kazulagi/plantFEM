program main
    use IOClass
    use FEMDomainClass
    use ElastoPlasticityClass
    implicit none

    type(ElastoPlasticity_) :: ep
    real(real64),allocatable :: new_stress(:,:),stress(:,:),strain(:,:),&
        plasticstrain(:,:),dStrain(:,:),Displacement(:,:)
    real(real64) :: c, phi, psi, E, nu, lambda, mu
    integer(int32) :: num_cycle
    type(IO_) :: f(8)
    type(FEMDomain_) :: cube, cloned_cube

    call cube%create("Cube3D",x_num=1,y_num=1,z_num=1)
    call cube%vtk("cube")

    new_stress = zeros(3,3)
    stress = zeros(3,3)
    strain = zeros(3,3)
    plasticstrain = zeros(3,3)
    dStrain = zeros(3,3)

    c   = 10.0d0
    phi = radian(30.0d0)
    psi = radian(10.0d0)
    E   = 180.0d0*1000.0d0
    nu  = 0.3000d0
    
    lambda = E*nu/(1.0d0 + nu)/(1.0d0 - 2.0d0*nu)
    mu     = E/2.0d0/(1.0d0*nu)

    call f(1)%open("single_elem_s11") 
    call f(2)%open("single_elem_s22") 
    call f(3)%open("single_elem_s33") 
    call f(4)%open("single_elem_s12") 
    call f(5)%open("single_elem_s23") 
    call f(6)%open("single_elem_s13") 
    call f(7)%open("single_elem_I1") 
    call f(8)%open("single_elem_J2") 
    
    do i_i=1,10000
        Displacement = zeros(cube%nn(),cube%nd() )
        
        Displacement(5,1) = -0.000000010d0
        Displacement(6,1) = -0.000000010d0
        Displacement(7,1) = -0.000000010d0
        Displacement(8,1) = -0.000000010d0
        

        Displacement(5,3) = -0.00!0000010d0
        Displacement(6,3) = -0.00!0000010d0
        Displacement(7,3) = -0.00!0000010d0
        Displacement(8,3) = -0.00!0000010d0

        dstrain = cube%getStrainTensor(Displacement=Displacement,ElementID=1,GaussPointID=2 )
        cloned_cube = cube
        call cloned_cube%deform(disp=to_vector(Displacement*i_i*1000.0d0))
        call cloned_cube%vtk("clone")
        
        ! How to use
        ! <input>
        ! (1) YieldFunction : real64 function  :: a yield function f(\sigma) 
        ! (2) DruckerPrager : real64 function  :: a plastic potential g(\sigma)
        ! (3) Strain        : real64 (1:3,1:3) :: strain tensor at last timestep
        ! (4) dStrain       : real64 (1:3,1:3) :: increment of strain
        ! (5) CauchyStress  : real64 (1:3,1:3) :: Cauchy's stress tensor at last timestep
        ! (6) PlasticStrain : real64 (1:3,1:3) :: plastic strain tensor at last timestep
        ! (7) YieldParams   : real64 (:) :: parameters for yield function
        ! (8) PlasticParams : real64 (:) :: parameters for plastic potential
        ! (9) ElasticParams : real64 (:) :: parameters for elastic potential
        ! (10)epsilon(optional): real64  :: machine epsilon for computing derivative
        
        ! <output>
        ! return value :: real64 (1:3,1:3) :: updated Cauchy's stress tensor
        ! (6) PlasticStrain : real64 (1:3,1:3) :: updated plastic strain tensor 

        new_stress = to_StressTensor(&
            YieldFunction   = MohrCoulomb,&
            PlasticPotential= VonMises,&
            Strain=Strain,&
            dStrain=dStrain,&
            CauchyStress=stress,&
            PlasticStrain=PlasticStrain,&
            YieldParams  = [c,phi],&
            PlasticParams= [c,psi],&
            ElasticParams=[lambda,mu], &
            epsilon=dble(1.0e-4) &
            ) 
            stress = new_stress
            strain = strain + dStrain

        call print(strain)
        
        write(f(1)%fh,*) i_i,new_stress(1,1)
        write(f(2)%fh,*) i_i,new_stress(2,2)
        write(f(3)%fh,*) i_i,new_stress(3,3)
        write(f(4)%fh,*) i_i,new_stress(1,2)
        write(f(5)%fh,*) i_i,new_stress(2,3)
        write(f(6)%fh,*) i_i,new_stress(1,3)
        write(f(7)%fh,*) i_i,to_I1(new_stress)
        write(f(8)%fh,*) i_i,to_J2(new_stress)
        

        
    enddo

    

    call f(1)%close()
    call f(2)%close()
    call f(3)%close()
    call f(4)%close()
    call f(5)%close()
    call f(6)%close()

end program main