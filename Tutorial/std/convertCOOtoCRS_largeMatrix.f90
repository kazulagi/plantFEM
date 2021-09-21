use LinearSolverClass
use TimeClass
use RandomClass
implicit none

integer(int32),parameter :: n=300000

type(LinearSolver_) :: solver
type(Time_) :: time
type(Random_) :: random
real(real64) :: mm(n),x(n)
integer(int32) :: i, j

solver%val = zeros(n)
solver%Index_I = int(zeros(n))
solver%Index_J = int(zeros(n))
solver%b = zeros(n)
solver%b_Index_J = int(zeros(n))
do i=1,n
    ! COO matrix
    solver%val(i) = 1.0d0
    solver%Index_I(i) = i
    solver%Index_J(i) = i
    !RHS
    solver%b(i) = dble(i)
    solver%b_Index_J(i) = i
enddo

! matmul operation
!call time%start()
!mm = matmul(Amat,solver%b)
!call time%show()
!print *, norm(mm)

! matmul operation
mm(:) = 0.0d0

call time%start()
call solver%convertCOOtoCRS(OpenMP=false)
call time%show()


call time%start()
do i=1,1000
    mm = solver%matmulCRS(OpenMP=false)
enddo
call time%show()
print *, norm(mm)

! matmul operation
mm(:) = 0.0d0
call time%start()
do i=1,1000
    mm = solver%matmulCOO(OpenMP=false)
enddo
call time%show()
print *, norm(mm)



end