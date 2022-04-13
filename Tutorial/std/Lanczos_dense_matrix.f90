program main
    use plantfem
    implicit none

    real(real64),allocatable :: a(:,:), V(:,:), T(:,:)

    a = zeros(3,3)

    a(1,:) = [1.0d0, 2.0d0, 0.0d0]
    a(2,:) = [2.0d0, 2.0d0, 2.1d0]
    a(3,:) = [0.0d0, 2.1d0, 1.0d0]
    
    ! Lanczos algorithm
    call Lanczos(A=A, V=V, T=T)

    call print("A")
    call print(A)
    call print("V")
    call print(V)
    call print("T")
    call print(T)
    call print("VTV* (=A) ")
    call print(matmul(V, matmul(T, transpose(V) )) )
    call print("V*AV (=T)")
    call print(matmul(transpose(V), matmul(A, V )) )
    
    
end program main