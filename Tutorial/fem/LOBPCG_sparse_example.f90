use FEMSolverClass
implicit none

real(real64),allocatable :: K(:,:), M(:,:),eigen_vector(:),eigen_value(:),&
x(:,:),eigen_vectors(:,:),lambda_mins(:),sample_mat(:,:),lambda(:),K_sp_val(:),&
bvec(:),new_vector(:),new_matrix(:,:)
real(real64)::lambda_min
integer(int32),allocatable :: K_sp_col(:),K_sp_rowptr(:)
integer(int32) :: i
type(FEMSolver_) :: FEMSolver
type(Random_) :: random

K = zeros(5,5)
M = eye(5,5)

K(1,:) = [2.0d0, 2.0d0, 0.0d0,0.0d0,0.0d0]
K(2,:) = [2.0d0, 4.0d0, 2.0d0,0.0d0,0.0d0]
K(3,:) = [0.0d0, 2.0d0, 4.0d0,2.0d0,0.0d0]
K(4,:) = [0.0d0, 0.0d0, 2.0d0,4.0d0,2.0d0]
K(5,:) = [0.0d0, 0.0d0, 0.0d0,2.0d0,1.0d0]

call to_CRS(K,K_sp_val,K_sp_col,K_sp_rowptr)


lambda_mins = zeros(1)
eigen_vectors =  LOBPCG_sparse(K_sp_val,K_sp_col,K_sp_rowptr,lambda_mins)

!eigen_vector =  LOBPCG_dense_single(K,M,lambda_min)

print *, "eigen_value",lambda_mins

print *, "eigen_vector"
call print(eigen_vectors)

!> Ans.
! eigen_value -0.35521847100475296     
!eigen_vector
!-0.22020298528984170     
! 0.25931306916251190     
!-0.34447954900490979     
! 0.49082877819227444     
!-0.72435373143693627   

!> Ans@ Scipy
! [-0.35521838  0.47871512  2.549596    5.122324    7.2045794 ]

! [[ 0.22020283  0.6878815  -0.537812   -0.38525033  0.20166838]
!  [-0.25931296 -0.5232321  -0.1477898  -0.6014385   0.52479976]
!  [ 0.3444794   0.23334298  0.64498943  0.0477451   0.6392136 ]
!  [-0.49082875  0.11239842 -0.3199573   0.6282312   0.49940613]
!  [ 0.72435385 -0.43123636 -0.4129556   0.3047946   0.16097984]]

!Theoretical solution:\lambda_1 = −0.35521847100475...
! plantFEM solution: \lambda_1  = -0.35521847100475296
! scipy solution:    \lambda_1  = -0.35521838

end
