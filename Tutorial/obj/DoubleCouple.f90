program main
    use EarthClass
    implicit none
    
    type(FEMDomain_) :: cube
    real(real64),allocatable :: MomentVector(:),ForceVector(:,:)
    real(real64) :: phi,delta,lambda,M

    ! 2023/06/16 21:24 depth 47 km, Mj 4.9
    phi    = radian(196.9d0)
    delta  = radian(26.1d0)
    lambda = radian(100.8d0)
    M      = 10.0d0 ! teki-toh
    
    MomentVector = to_MomentTensor(phi=phi,delta=delta,lambda=lambda,M=M) .get. [(1,1),(2,2),(3,3),(1,2),(2,3),(3,1)]

    call cube%create("Cube3D",x_num=10,y_num=10,z_num=10)
    
    ! f = -Div(M)
    ForceVector = matmul(transpose(cube%Bmatrix(ElementID=1)),MomentVector)
    
    ! show
    call print(ForceVector)

end program main