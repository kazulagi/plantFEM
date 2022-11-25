use SparseClass
use FEMSolverClass
implicit none

type(COO_) :: COO
type(CRS_) :: CRS
type(FEMSolver_) :: solver
real(real64) :: epsilon,D,Le
type(IO_) :: f
real(real64),allocatable :: C(:),b(:),X(:)


epsilon = 10000.d0
Le = 1.0d0
D =  1.0d0
!epsilon = -0.10d0
call COO%init(6)
call COO%add(1,1, 1.0d0*D*Le)
call COO%add(1,2,-1.0d0*D*Le)
call COO%add(2,1,-1.0d0*D*Le)
call COO%add(2,2, 1.0d0*D*Le)

call COO%add(2,2, 1.0d0*D*Le/2.0d0)
call COO%add(2,3,-1.0d0*D*Le/2.0d0)
call COO%add(3,2,-1.0d0*D*Le/2.0d0)
call COO%add(3,3, 1.0d0*D*Le/2.0d0)

call COO%add(4,4, 1.0d0*D*Le/2.0d0)
call COO%add(4,5,-1.0d0*D*Le/2.0d0)
call COO%add(5,4,-1.0d0*D*Le/2.0d0)
call COO%add(5,5, 1.0d0*D*Le/2.0d0)

call COO%add(5,5, 1.0d0*D*Le)
call COO%add(5,6,-1.0d0*D*Le)
call COO%add(6,5,-1.0d0*D*Le)
call COO%add(6,6, 1.0d0*D*Le)

call COO%add(2,2, epsilon)
call COO%add(3,3, epsilon)
call COO%add(4,4, epsilon)
call COO%add(5,5, epsilon)

call COO%add(2,4, -epsilon)
call COO%add(3,5, -epsilon)
call COO%add(4,2, -epsilon)
call COO%add(5,3, -epsilon)

! boundary condition

call COO%update(1,2,0.0d0)
call COO%update(2,1,0.0d0)
call COO%update(1,1,1.0d0)

call COO%update(5,6,0.0d0)
call COO%update(6,5,0.0d0)
call COO%update(6,6,1.0d0)


 
CRS = COO%to_CRS()

C = zeros(6)
b = zeros(6)
b(1:2) = -0.10d0
b(5:6) = -1.00d0

call CRS%BICGSTAB(X=C,b=b)
X = dble([0.0,1.0,2.0,1.0,2.0,3.0])
call f%open("EbOFEM_diffusion_"+str(epsilon)+".txt","w")
call f%write(X(1:3) .h. C(1:3) )
call f%write(" ")
call f%write(" ")
call f%write(X(4:6) .h. C(4:6) )
call f%close()
call f%plot(option=" w l")

end 