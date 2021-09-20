use LinearSolverClass
use TimeClass
implicit none

type(LinearSolver_) :: solver
type(Time_) :: time
real(real64) :: Amat(5,5),mm(5)
integer(int32) :: i, j

Amat(1,:) = [ 1.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0 ]
Amat(2,:) = [ 2.0d0, 2.0d0, 0.0d0, 0.0d0, 0.0d0 ]
Amat(3,:) = [ 0.0d0, 3.0d0, 3.0d0, 0.0d0, 0.0d0 ]
Amat(4,:) = [ 2.0d0, 2.0d0, 2.0d0, 1.0d0, 0.0d0 ]
Amat(5,:) = [ 0.0d0, 0.0d0, 0.0d0, 0.0d0, 1.0d0 ]

solver%val = [1.0d0,2.0d0,2.0d0,3.0d0,3.0d0,2.0d0,2.0d0,2.0d0,1.0d0,1.0d0]
solver%Index_I = [1, 2, 2, 3, 3, 4,4,4,4,5]
solver%Index_J = [1, 1, 2, 2, 3, 1,2,3,4,5]

solver%b = [1.0d0, 2.0d0, 3.0d0,4.0d0,5.0d0]
solver%b_Index_J = [1, 2, 3, 4, 5]
call solver%convertCOOtoCRS()

print *, solver%CRS_val
print *, solver%CRS_Index_I
print *, solver%CRS_Index_J

call time%start()
mm = matmul(Amat,solver%b)
call time%show()
print *, mm

mm(:) = 0.0d0
call time%start()
mm = solver%matmulCRS()
call time%show()
print *, mm

end