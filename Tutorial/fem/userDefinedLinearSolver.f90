module MyLinearSolvers
    use iso_fortran_env
    implicit none
contains

!> user-defined linear solver
function my_Solver(row_ptr,col_idx,val,rhs,x0) result(x)
    integer(int32),intent(in) :: row_ptr(:),col_idx(:)
    real(real64),intent(in) :: val(:),rhs(:),x0(:)
    real(real64),allocatable :: x(:)
    
    ! just returns x0 as solution
    x = x0

end function
!>-

end module

program main
    use MyLinearSolvers
    use FEMSolverClass    
    implicit none

    type(FEMSolver_) :: solver
    type(COO_) :: COO

    call COO%init(2)
    call COO%set(1,1,1.0d0)
    call COO%set(1,2,2.0d0)
    call COO%set(2,1,1.0d0)
    call COO%set(2,2,1.0d0)


    call solver%setCRS(COO%to_CRS() )
    call solver%setRHS([2.0d0,-1.0d0])

    print *, solver%solve(LinearSolver=my_solver,x0=[2.0d0,2.0d0])

end program main