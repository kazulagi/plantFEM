use LinearSolverClass
use TimeClass
use RandomClass
use MPIClass
implicit none

integer(int32),parameter :: n=1000000

logical,parameter :: CRS=.false.

type(LinearSolver_) :: solver
type(Time_) :: time
type(MPI_) :: mpid
type(Random_) :: random
real(real64) :: mm(n),x(n)
integer(int32) :: i, j, itr,m

call mpid%start()

m = n + (n-1)*2 
solver%val = zeros(m)   
solver%Index_I = int(zeros(m))
solver%Index_J = int(zeros(m))
solver%b = zeros(n)
solver%b_Index_J = int(zeros(n))
itr = 0 
do i=1,n
    ! COO matrix

    if(i==1)then
        itr = itr+1
        solver%val(itr) = 1.0d0
        solver%Index_I(itr) = i
        solver%Index_J(itr) = i
        itr = itr+1
        solver%val(itr) = 0.50d0
        solver%Index_I(itr) = i
        solver%Index_J(itr) = i+1
    endif

    if(2<=i .and. i <=n-1)then
        itr = itr+1
        solver%val(itr) = 0.50d0
        solver%Index_I(itr) = i
        solver%Index_J(itr) = i-1
        itr = itr+1
        solver%val(itr) = 0.50d0
        solver%Index_I(itr) = i
        solver%Index_J(itr) = i    
        itr = itr+1
        solver%val(itr) = 0.50d0
        solver%Index_I(itr) = i
        solver%Index_J(itr) = i+1
        if(i==n-1)then
            solver%val(itr) = 0.00d0
        endif
    endif

    if(i==n)then
        itr = itr+1
        solver%val(itr) = 1.00d0
        solver%Index_I(itr) = i
        solver%Index_J(itr) = i
        itr = itr+1
        solver%val(itr) = 0.00d0
        solver%Index_I(itr) = i
        solver%Index_J(itr) = i-1
    endif
    
    !RHS
    solver%b_Index_J(i) = i
enddo

solver%b(n-1) = -5.0d0
solver%b(n) = 10.0d0
! matmul operation
!call time%start()
!mm = matmul(Amat,solver%b)
!call time%show()
!print *, norm(mm)

! matmul operation
!print *, solver%val
!print *, solver%Index_I
!print *, solver%Index_J
!print *, solver%b



solver%x = zeros(size(solver%b) )

!print *, solver%val
!print *, solver%Index_I
!print *, solver%Index_J
!print *, solver%b
!stop
! matmul operation
solver%itrmax = 10000
solver%er0 = dble(1.0e-12)

if(CRS)then
    !>>>>>>> CRS format
    solver%x=0.0d0
    call time%start()
    call solver%convertCOOtoCRS(OpenMP=false)
    call time%show()
    call bicgstab_CRS(solver%val, solver%CRS_index_I, solver%index_J, &
        solver%x, solver%b, solver%itrmax, solver%er0)
    call time%show()
    print *,"CRS", norm(solver%x)
else
    !>>>>>>> COO format
    solver%x=0.0d0
    call time%start()
    call bicgstab_COO(solver%val, solver%index_I, solver%index_J, &
        solver%x, solver%b, solver%itrmax, solver%er0)
    call time%show()
    print *,"COO", norm(solver%x)
endif



call mpid%end()

end