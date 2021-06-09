use LinearSolverClass

type(LinearSolver_) :: solver
real(real64) :: A_e1(2,2),A_e2(2,2),A_i1(4,4)
real(real64) :: b_e1(2),b_e2(2),b_i1(4)
integer(int32) :: conn_e1(1),conn_e2(1),conn_i1(2)
integer(int32) :: dom_e1(1),dom_e2(1),dom_i1(2),DOF

! 2 unknowns for 1st domain, 2 unknowns for 2nd domain
call solver%init(NumberOfNode=(/ 1, 1 /), DOF = 2 )
! ( 1,  1  1, 1 )(x1) = (0)
! ( 1,  1  1, -1)(x2) = (4)
! ( 1,  1  -1, 1)(x3) = (-4)
! ( 1,  -1  1, 1)(x4) = (2)

! ix (x1) = 1

DOF = 2
! Domain #1 
!x-node#1-Domain1, y-node#1-Domain1
!(        1,            1)(x-node#1-Domain1) = (0)
!(        1,            1)(y-node#1-Domain1) = (4)
A_e1(1,1:2) = (/1.0d0, 1.0d0 /)
A_e1(2,1:2) = (/1.0d0, 1.0d0 /)
b_e1(1)     = 0.0d0
b_e1(2)     = 4.0d0
conn_e1(1) = 1
dom_e1(1)  = 1

! Domain #2
!x-node#1-Domain2, y-node#1-Domain2
!(       -1,              1)(x-node#1-Domain2) = (-4)
!(        1,              1)(y-node#1-Domain2) = ( 2)
A_e2(1,1:2) = (/-1.0d0, 1.0d0 /)
A_e2(2,1:2) = (/ 1.0d0, 1.0d0 /)
b_e2(1)     =-4.0d0
b_e2(2)     = 2.0d0
conn_e2(1) = 1
dom_e2(1)  = 2

! Interface #1 

! ( 0,  0   1,  1 )(x-node#1-Domain1)= (0)
! ( 0,  0   1, -1)(y-node#1-Domain1) = (0)
! ( 1,  1   0,  0)(x-node#1-Domain2) = (0)
! ( 1, -1   0,  0)(y-node#1-Domain2) = (0)
A_i1(1,1:4) = (/0.0d0, 0.0d0, 1.0d0, 1.0d0 /)
A_i1(2,1:4) = (/0.0d0, 0.0d0, 1.0d0,-1.0d0 /)
A_i1(3,1:4) = (/1.0d0, 1.0d0, 0.0d0, 0.0d0 /)
A_i1(4,1:4) = (/1.0d0,-1.0d0, 0.0d0, 0.0d0 /)
b_i1(1:4)   = 0.0d0
conn_i1(1:2) = (/1, 1/) ! Node #1, Node #1
dom_i1(1:2)  = (/1, 2/) ! Domain#1, Domain#2

call solver%assemble(connectivity=conn_i1 ,DOF=DOF,eMatrix=A_i1,DomainIDs=dom_i1)
call solver%assemble(connectivity=conn_e1 ,DOF=DOF,eMatrix=A_e1,DomainIDs=dom_e1)
call solver%assemble(connectivity=conn_e2 ,DOF=DOF,eMatrix=A_e2,DomainIDs=dom_e2)
call solver%assemble(connectivity=conn_e1 ,DOF=DOF,eVector=b_e1,DomainIDs=dom_e1)
call solver%assemble(connectivity=conn_e2 ,DOF=DOF,eVector=b_e2,DomainIDs=dom_e2)
call solver%assemble(connectivity=conn_i1 ,DOF=DOF,eVector=b_i1,DomainIDs=dom_i1)

! assemble global matrix
print *, "A = "
call print(solver%globalMatrix() )
print *, "b = "
call print(solver%globalVector() )


call solver%fix(nodeid=1,entryvalue= 1.0d0,entryID=1,DOF=DOF,row_DomainID=1)
print *, "Introduced Dirichlet B.C. >>>> "
print *, "A = "
call print(solver%globalMatrix() )
print *, "b = "
call print(solver%globalVector() )


call solver%solve("BiCGSTAB")


print *, "ans      :",solver%x(:)
print *, "true ans :",1.0d0,-1.0d0,2.0d0,-2.0d0
print *, "L2 norm : ",dot_product( solver%x-(/1.0d0,-1.0d0,2.0d0,-2.0d0/), solver%x-(/1.0d0,-1.0d0,2.0d0,-2.0d0/)  )


end