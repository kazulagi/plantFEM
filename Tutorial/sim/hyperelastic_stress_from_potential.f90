program main
    use MathClass
    use ElastoPlasticityClass
    implicit none

    real(real64) :: sigma(3,3),e_p(3,3),s(3,3),ext_ans(3,3),app_ans(3,3),ElasticStrain(3,3),e_v
    real(real64) :: stiffness_tensor(:,:,:,:)
    real(real64),allocatable :: params(:)
    type(IO_) :: f(1:4)
    type(Random_) :: random
    

    ElasticStrain(:,:) = 0.0d0 
    ElasticStrain(1,1) = 1.0d0/1000.0d0
    ElasticStrain(2,2) = 1.0d0/1000.0d0
    ElasticStrain(3,3) = 1.0d0/1000.0d0

    sigma(:,:) = 0.0d0
    sigma(1,1) = 1.0d0
    sigma(1,2) = 3.0d0
    sigma(2,1) = 3.0d0
    sigma(2,2) = 0.0d0
    sigma(3,3) = 6.0d0

    e_p(:,:)   = 0.0d0 
    

    s = sigma - to_I1(sigma)*eyes(3,3)
    ! elastic potentials
    ! first and second Lame constant

    call f(1)%open("StVenant.txt","w")
    call f(2)%open("neoHookean_Vladinirov.txt","w")
    call f(3)%open("neoHookean_Simo.txt","w")
    call f(4)%open("neoHookean.txt","w")
    do i_i=-300,1000
        

        ElasticStrain(1,1) = 1.0d0/1000.0d0*i_i
        ElasticStrain(2,2) = 1.0d0/1000.0d0*i_i
        ElasticStrain(3,3) = 1.0d0/1000.0d0*i_i
        ElasticStrain(2,1) = 1.0d0/10000.0d0*i_i
        ElasticStrain(1,2) = 1.0d0/10000.0d0*i_i
        e_v = trace(ElasticStrain)

        params  = [1.0d0, 0.30d0]
        !call print(">> df/dsigma of StVenant >>")
        !call print(d_dsigma(StVenant,ElasticStrain,params))
        write(f(1)%fh,*) e_v,to_I1(d_dsigma(StVenant,ElasticStrain,params)),&
            to_J2(d_dsigma(StVenant,ElasticStrain,params))
        !call print("")
        
        !call print(">> df/dsigma of StVenant >>")
        !call print(d_dsigma(neoHookean_Vladimirov,ElasticStrain,params))
        write(f(2)%fh,*) e_v,to_I1(d_dsigma(neoHookean_Vladimirov,ElasticStrain,params)),&
            to_J2(d_dsigma(neoHookean_Vladimirov,ElasticStrain,params))
        write(f(3)%fh,*) e_v,to_I1(d_dsigma(neoHookean_Simo,ElasticStrain,params)),&
            to_J2(d_dsigma(neoHookean_Simo,ElasticStrain,params))
        write(f(4)%fh,*) e_v,to_I1(d_dsigma(neoHookean,ElasticStrain,params)),&
            to_J2(d_dsigma(neoHookean_Simo,ElasticStrain,params))
    enddo
    call f(1)%close()
    call f(2)%close()
    call f(3)%close()
    call f(4)%close()

end program main