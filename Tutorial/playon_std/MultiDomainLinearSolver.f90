use LinearSolverClass

type(LinearSolver_) :: solver

! 2 unknowns for 1st domain, 2 unknowns for 2nd domain
call solver%init(NumberOfNode=(/ 2, 2 /), DOF = 1 )
! ( 1,  1  1, 1 )(x1) = (0)
! ( 1,  1  1, -1)(x2) = (4)
! ( 1,  1  -1, 1)(x3) = (-4)
! ( 1,  -1  1, 1)(x4) = (2)

! ix (x1) = 1


call solver%set(low=1,column=1,entryvalue= 1.0d0,row_DomainID=1,column_DomainID=1)
call solver%set(low=2,column=1,entryvalue= 1.0d0,row_DomainID=2,column_DomainID=2)
call solver%set(low=2,column=2,entryvalue=-1.0d0,row_DomainID=2,column_DomainID=1)
call solver%set(low=1,column=2,entryvalue= 1.0d0,row_DomainID=1,column_DomainID=1)
call solver%set(low=1,column=2,entryvalue= 1.0d0,row_DomainID=1,column_DomainID=2)
call solver%set(low=2,column=2,entryvalue=-1.0d0,row_DomainID=1,column_DomainID=2)
call solver%set(low=2,column=1,entryvalue= 1.0d0,row_DomainID=2,column_DomainID=1)
call solver%set(low=1,column=1,entryvalue=-1.0d0,row_DomainID=2,column_DomainID=2)
call solver%set(low=2,column=1,entryvalue= 1.0d0,row_DomainID=1,column_DomainID=1)
call solver%set(low=2,column=2,entryvalue= 1.0d0,row_DomainID=1,column_DomainID=1)
call solver%set(low=1,column=1,entryvalue= 1.0d0,row_DomainID=1,column_DomainID=2)
call solver%set(low=2,column=1,entryvalue= 1.0d0,row_DomainID=1,column_DomainID=2)
call solver%set(low=1,column=1,entryvalue= 1.0d0,row_DomainID=2,column_DomainID=1)
call solver%set(low=1,column=2,entryvalue= 1.0d0,row_DomainID=2,column_DomainID=1)
call solver%set(low=1,column=2,entryvalue= 1.0d0,row_DomainID=2,column_DomainID=2)
call solver%set(low=2,column=2,entryvalue= 1.0d0,row_DomainID=2,column_DomainID=2)


call solver%set(low=1,entryvalue= 0.0d0,row_DomainID=1)
call solver%set(low=2,entryvalue= 4.0d0,row_DomainID=1)
call solver%set(low=1,entryvalue=-4.0d0,row_DomainID=2)
call solver%set(low=2,entryvalue= 2.0d0,row_DomainID=2)


call solver%fix(nodeid=1,entryvalue= 1.0d0,row_DomainID=1)

call solver%solve("BiCGSTAB")

call print( reshape(solver%val(:),4,4) )

print *, solver%Index_I(:)
print *, solver%Index_J(:)
print *, solver%row_Domain_ID(:)
print *, solver%column_Domain_ID(:)

print *, solver%b(:)
print *, solver%b_Index_J(:)
print *, solver%b_Domain_ID(:)


! (x1) = (1)
! (x2) = (-1)
! (x3) = (2)
! (x4) = (-2)

print *, "ans      :",solver%x(:)
print *, "true ans :",1.0d0,-1.0d0,2.0d0,-2.0d0
print *, "L2 norm : ",dot_product( solver%x-(/1.0d0,-1.0d0,2.0d0,-2.0d0/), solver%x-(/1.0d0,-1.0d0,2.0d0,-2.0d0/)  )


end