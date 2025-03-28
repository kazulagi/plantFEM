program main
    use MathClass
    use ElastoPlasticityClass
    implicit none

    real(real64) :: sigma(3,3),e_p(3,3),params(1),s(3,3),ext_ans(3,3),app_ans(3,3)
    type(IO_) :: f
    type(Random_) :: random

    sigma(:,:) = 0.0d0 
    sigma(1,1) = 1.0d0
    sigma(1,2) = 3.0d0
    sigma(2,1) = 3.0d0
    sigma(2,2) = 0.0d0
    sigma(3,3) = 6.0d0

    e_p(:,:)   = 0.0d0 
    params(:)  = 1.0d0

    s = sigma - to_I1(sigma)*eyes(3,3)

    call print("\partial tr(sigma)/ \partial sigma_{ij} = \delta_{ij}")
    call print(">> approximate solution >>")
    call print(d_dsigma(trace_sigma,sigma,e_p,params))
    call print(">> exact solution >>")
    call print(eyes(3,3))
    call print("")
    ! \partial tr(sigma)/ \partial sigma_{ij} = \delta_{ij}

    call print("\partial J_2/ \partial sigma_{ij} = s_{ij} = sigma_{ij} - I_1 \delta_{ij} )")
    call print(">> approximate solution >>")
    call print(d_dsigma(J2_function,sigma,e_p,params))
    call print(">> exact solution >>")
    call print(sigma - to_I1(sigma)*eyes(3,3))
    call print("")


    call print("\partial J_3/ \partial sigma_{ij} = s_{ij} = s_{im}s_{mj}- J_2 \cfrac{2}{3}\delta{ij}) )")
    call print(d_dsigma(J3_function,sigma,e_p,params))
    call print(">> exact solution >>")
    call print(matmul(s,s) - to_J2(sigma)*2.0d0/3.0d0*eyes(3,3) )
    call print("")
    
    !call print("\partial Lode_angle/ \partial sigma_{ij} = s_{ij} = ")
    !call print(d_dsigma(Lode_function,sigma,e_p,params))
    !call print(">> exact solution >>")
    !call print("")

    call f%open("stress_tensor_derivative_auto_test.csv","w")
    do i_i=1,1000*1000

        sigma = (2*random%randn(3,3)-1.0d0)
        ! symmetrize
        sigma = 0.50d0*(sigma + transpose(sigma))

        e_p(:,:)   = 0.0d0 
        params(:)  = 1.0d0
        s = sigma - to_I1(sigma)*eyes(3,3)

        !call print("\partial tr(sigma)/ \partial sigma_{ij} = \delta_{ij}")
        !call print(">> approximate solution >>")
        app_ans = d_dsigma(trace_sigma,sigma,e_p,params)
        ext_ans = eyes(3,3)
        write(f%fh,*) i_i,",",1,",",maxval(app_ans-ext_ans),",",minval(app_ans-ext_ans)
        !call print("")
        ! \partial tr(sigma)/ \partial sigma_{ij} = \delta_{ij}

        !call print("\partial J_2/ \partial sigma_{ij} = s_{ij} = sigma_{ij} - I_1 \delta_{ij} )")
        !call print(">> approximate solution >>")
        app_ans = d_dsigma(J2_function,sigma,e_p,params)
        ext_ans =  sigma - to_I1(sigma)*eyes(3,3)
        write(f%fh,*) i_i,",",2,",",maxval(app_ans-ext_ans),",",minval(app_ans-ext_ans)
        !call print("")


        !call print("\partial J_3/ \partial sigma_{ij} = s_{ij} = s_{im}s_{mj}- J_2 \cfrac{2}{3}\delta{ij}) )")
        app_ans = d_dsigma(J3_function,sigma,e_p,params)
        ext_ans = matmul(s,s) - to_J2(sigma)*2.0d0/3.0d0*eyes(3,3) 
        write(f%fh,*) i_i,",",3,",",maxval(app_ans-ext_ans),",",minval(app_ans-ext_ans)
        !call print("")
    
    enddo
    call f%close()


contains

!-----------------------------------------------
function trace_sigma(CauchyStress,PlasticStrain,params) result(ret)
    complex(real64), intent(in) :: CauchyStress(:, :), PlasticStrain(:, :) 
    real(real64), intent(in) ::  params(:)
    complex(real64) :: ret! variables should be complex numbers

    ret = trace(CauchyStress)

end function
!-----------------------------------------------


!-----------------------------------------------
function J2_function(CauchyStress,PlasticStrain,params) result(ret)
    complex(real64), intent(in) :: CauchyStress(:, :), PlasticStrain(:, :) 
    real(real64), intent(in) ::  params(:)
    complex(real64) :: ret! variables should be complex numbers

    ret = to_J2(CauchyStress)

end function
!-----------------------------------------------



!-----------------------------------------------
function J3_function(CauchyStress,PlasticStrain,params) result(ret)
    complex(real64), intent(in) :: CauchyStress(:, :), PlasticStrain(:, :) 
    real(real64), intent(in) ::  params(:)
    complex(real64) :: ret! variables should be complex numbers

    ret = to_J3(CauchyStress)
    !ret = det_mat(CauchyStress-to_I1(CauchyStress)*eyes(3,3),size(CauchyStress,1))

end function
!-----------------------------------------------


!-----------------------------------------------
function Lode_function(CauchyStress,PlasticStrain,params) result(ret)
    complex(real64), intent(in) :: CauchyStress(:, :), PlasticStrain(:, :) 
    real(real64), intent(in) ::  params(:)
    complex(real64) :: ret! variables should be complex numbers

    !ret = to_J3(CauchyStress)
    ret = to_LodeAngle(CauchyStress)
    
end function
!-----------------------------------------------



end program main