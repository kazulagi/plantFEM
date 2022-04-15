program main
use plantfem
implicit none

real(real64),allocatable :: A(:,:), V(:,:), T(:,:)
real(real64),allocatable :: D(:),E(:)
real(real64),allocatable :: WORK(:)
integer(int32),allocatable :: IWORK(:),IFAIL(:)
real(real64),allocatable :: Eigen_vectors(:,:),eigen_values(:)
integer(int32) :: num_eigen,INFO
real(real64)  :: VL, VU, ABSTOL
integer(int32):: IL, IU 
character(1) :: JOBZ="V"
character(1) :: RANGE="A"

!LAPACK
A = zeros(3,3)
A(1,:) = [1.0d0, 2.0d0, 3.0d0]
A(2,:) = [2.0d0, 1.0d0, 1.0d0]
A(3,:) = [3.0d0, 1.0d0, 8.0d0]

call Lanczos(A=A, V=V, T=T)
call print("--")
call print(V)
call print("--")
call print(T)
D = getDiag(T)
E = getSubDiag(T)

num_eigen = size(D)
eigen_values = zeros(size(D))
eigen_vectors = zeros(size(D),num_eigen )
WORK = zeros(5*size(D) )
IWORK = int(zeros(5*size(D) ))
IFAIL = int(zeros(size(D) ))
ABSTOL = dble(1.0e-14)

! LAPACK
call DSTEVX( JOBZ, RANGE, size(D),&
    D, E, VL, VU, IL, IU,ABSTOL,&
    num_eigen,eigen_values, eigen_vectors, size(D), &
    WORK, IWORK, IFAIL, INFO )

call print(eigen_values)

!eigen_Vectors = matmul(V,matmul(eigen_values, transpose(V) ) )
eigen_Vectors = matmul(V,eigen_vectors)
do i_i=1,3
    eigen_vectors(:,i_i) = eigen_vectors(:,i_i)/norm( eigen_vectors(:,i_i) )
    eigen_vectors(:,i_i) =eigen_vectors(:,i_i)/eigen_vectors(3,i_i)   
enddo
call print(eigen_vectors)

! same as
! https://www.emathhelp.net/ja/calculators/linear-algebra/eigenvalue-and-eigenvector-calculator/


contains

function getDiag(A) result(Diag_vec)
    real(real64),intent(in) :: A(:,:)
    real(real64),allocatable :: Diag_vec(:)
    integer(int32) :: i

    allocate(Diag_vec(size(A,1) ) )
    do i=1,size(A,1)
        Diag_vec(i) = A(i,i)
    enddo

end function

function getSubDiag(A) result(subDiag_vec)
    real(real64),intent(in) :: A(:,:)
    real(real64),allocatable :: subDiag_vec(:)
    integer(int32) :: i

    allocate(subDiag_vec(size(A,1)-1 ) )
    do i=1,size(A,1)-1
        subDiag_vec(i) = A(i,i+1)
    enddo
    
end function

end program main