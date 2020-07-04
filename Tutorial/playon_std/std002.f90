program main
    use std
    implicit none

    ! This example utilizes MathClass, where
    ! You can handle some basic operations.

    ! How to use:

    ! First, create the instance
    real(real64) :: vector1(2) = 1.0d0
    real(real64) :: vector2(3) = (/ 1.0d0, 2.0d0, 3.0d0/)
    real(real64) :: vector3(3) = (/ 2.0d0, -1.0d0, -1.0d0/)
    real(real64) :: vector4(3) = (/ 3.0d0, 2.0d0, 1.0d0/)
    real(real64) :: array(3,3) = 2.0d0
    real(real64) :: ret
    real(real64) :: ret_vector(3)
    real(real64) :: ret_array(3,3)
    
    ! dynamic allocatable array
    real(real64),allocatable :: dyna_array(:,:)

    !  #1  Linear algebra
    !----> Get Norm
    ret = norm(vector1)
    print *, "norm : "
    print *,  ret

    !----> Get dot-product (already implemented in Fortran90/95)
    print *, "dot_product : "
    ret = dot_product(vector1,vector1)
    print *, ret

    !----> Get corss-product ! only for rank(vector==3)
    ret_vector = cross_product(vector2, vector3)
    print *, "cross_product : "
    print *, ret_vector(:)

    !----> Get tensor-product 
    ret_array = tensor_product(vector2, vector3)
    print *, "tensor_product : "
    print *, ret_array(1,1:3)
    print *, ret_array(2,1:3)
    print *, ret_array(3,1:3)

    !----> Get determinant
    ret_array = tensor_product(vector2, vector3)
    ret = det(ret_array, 3 )
    print *, "determinant: "
    print *, ret


    !----> Get transpose  (already implemented in Fortran90/95)
    ret_array = transpose(ret_array)
    print *, "transposed matrix : "
    print *, ret_array(1,1:3)
    print *, ret_array(2,1:3)
    print *, ret_array(3,1:3)

    !----> Get identity matrix ! 
    ret_array = identity_matrix(3)
    print *, "identity matrix : "
    print *, ret_array(1,1:3)
    print *, ret_array(2,1:3)
    print *, ret_array(3,1:3)


    !----> Get inverse-matrix ! only for rank(vector==2 or 3)
    ret_array(1,2)=2.0d0
    ret_array = inverse(ret_array)
    print *, "inverse : "
    print *, ret_array(1,1:3)
    print *, ret_array(2,1:3)
    print *, ret_array(3,1:3)

    !----> Get trace of matrix
    ret = trace(ret_array)
    print *, "trace : "
    print *, ret


    !----> Get symmetric/ansymmetric(skew-sym)-matrix 
    ! prepair
    ret_array = identity_matrix(3)
    ret_array(1,2)=2.0d0
    ! perform
    ret_array = sym(ret_array,3)
    print *, "sym : "
    print *, ret_array(1,1:3)
    print *, ret_array(2,1:3)
    print *, ret_array(3,1:3)
    ! prepair
    ret_array = identity_matrix(3)
    ret_array(1,2)=2.0d0
    ! perform
    ret_array = asym(ret_array,3)
    print *, "asym (skew-sym) : "
    print *, ret_array(1,1:3)
    print *, ret_array(2,1:3)
    print *, ret_array(3,1:3)

    ! you can also dynamic allocate an array.
    allocate(dyna_array(3,3) )
    dyna_array = identity_matrix(3)
    print *, "dynamic array : "
    print *, dyna_array(1,1:3)
    print *, dyna_array(2,1:3)
    print *, dyna_array(3,1:3)

    ! #2 conversion and some tools
    ! ----> you can convert number into string
    print *, "number :: "
    print *, str(100.0d0)
    print *, str(123)

    ! -----> "def" is default value, and if "opt" is optional, then,
    print *, "default and option : "
    ret = input(default = 100.0d0, option = 20.0d0)
    print *, ret
    ret = input(default = 100.0d0)
    print *, ret

    ! If you want to add some technical Math, please send me a pull-request.

    ! Importance Index 7 / 10 : [*******   ]

end program main