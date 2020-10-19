program main
    use plantFEM
    implicit none


    real(real64) :: re64=100.0d0
    real(real64) :: vec(100)=1.0d0
    real(real64) :: vec3(3)=1.0d0
    real(real64) :: tensor(3,3)=0.0d0

    print *, "100 radian = ", degrees(re64),"degrees"
    print *, "100 degrees= ", radian(re64),"radian"

    print *, "real64 => string: ", str(re64)

    print *, "norm of vec(100) : ", norm(vec)

    tensor(:,:) = tensor_product(vec3,vec3)

    print *, tensor(1,:)
    print *, tensor(2,:)
    print *, tensor(3,:)

    tensor(:,:)=0.0d0

    tensor(1,1)=1.0d0
    tensor(2,2)=1.0d0
    tensor(3,3)=1.0d0

    print *, "det(tensor) : ", det_mat(tensor,size(tensor,1) )

    print *, "...etc."
    
end program main