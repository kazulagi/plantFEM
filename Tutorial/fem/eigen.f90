use FEMSolverClass
implicit none

type(CRS_) :: CRS
type(FEMSolver_) :: solver
real(real64),allocatable :: eigen_value(:),eigen_vectors(:,:)
real(real64),allocatable :: DenseMatrix(:,:)


DenseMatrix = zeros(5,5)
DenseMatrix(1,1:5) = dble([1,2,0,0,0])
DenseMatrix(2,1:5) = dble([2,12,0,3,0])
DenseMatrix(3,1:5) = dble([0,0,5,3,1])
DenseMatrix(4,1:5) = dble([0,3,3,3,4])
DenseMatrix(5,1:5) = dble([0,0,1,4,6])

call solver%import(to_CRS(DenseMatrix))
call solver%keepThisMatrixAs("A")

DenseMatrix = eyes(5,5)
call solver%import(to_CRS(DenseMatrix))
call solver%keepThisMatrixAs("B")

call solver%eig(num_eigen=5,eigen_value=eigen_value,eigen_vectors=eigen_vectors)

call print(eigen_value)
call print(eigen_vectors)


end