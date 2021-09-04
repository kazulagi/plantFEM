program main
    use std
    implicit none

    type(LinearSolver_) :: app
    type(time_) :: time
    integer(int32) :: i

    call time%start()
    
    ! set matrix (only non-zero value)
    call app%set(init=.true.)
    call app%set(1,1, entryvalue=  1.0d0)
    call app%set(2,2, entryvalue=  2.0d0)
    call app%set(2,3, entryvalue= -1.0d0)
    call app%set(3,2, entryvalue= -1.0d0)
    call app%set(3,3, entryvalue=  2.0d0)
    call app%set(4,4, entryvalue=  1.0d0)
    
    ! set Right-hand side vector (only non-zero value)
    call app%set(1, entryvalue=  0.5d0)
    call app%set(2, entryvalue=  0.5d0)
    call app%set(3, entryvalue=  0.2d0)
    call app%set(4, entryvalue=  0.2d0)
    
    ! solve!
    call app%solve(Solver="BiCGSTAB",CRS=.true.)
    
    ! show result
    print *, app%x

    call time%show()
    
end program