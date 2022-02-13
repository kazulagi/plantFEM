use FEMSolverClass
implicit none

real(real64),allocatable :: K(:,:), M(:,:),eigen_vector(:),eigen_value(:),&
x(:,:),eigen_vectors(:,:),lambda_mins(:)
real(real64)::lambda_min
integer(int32) :: i

K = zeros(4,4)
M = zeros(4,4)

K(1,:) = [1.0d0, 2.0d0, 0.0d0,0.0d0]
K(2,:) = [2.0d0, 2.0d0, 3.0d0,0.0d0]
K(3,:) = [0.0d0, 3.0d0, 5.0d0,0.0d0]
K(4,:) = [0.0d0, 0.0d0, 0.0d0,1.0d0]

M(1,:) = [1.0d0, 0.0d0, 0.0d0,0.0d0]
M(2,:) = [0.0d0, 1.0d0, 0.0d0,0.0d0]
M(3,:) = [0.0d0, 0.0d0, 1.0d0,0.0d0]
M(4,:) = [0.0d0, 0.0d0, 0.0d0,1.0d0]

eigen_vector =  LOBPCG_dense_single(K,M,lambda_min)

print *, "eigen_value",lambda_min
print *, "eigen_vector",eigen_vectors

end