use COOClass
implicit none

type(COO_) :: A
type(CRS_) :: A_CRS
type(CCS_) :: A_CCS
real(real64),allocatable :: RHS(:),true_val(:)


call A%init(4)
call A%update(1,1,  1.0d0)
call A%update(1,2,  1.0d0)
call A%update(1,3,  0.0d0)
call A%update(1,4,  3.0d0)
call A%update(2,1,  2.0d0)
call A%update(2,2,  1.0d0)
call A%update(2,3, -1.0d0)
call A%update(2,4,  1.0d0)
call A%update(3,1,  3.0d0)
call A%update(3,2, -1.0d0)
call A%update(3,3, -1.0d0)
call A%update(3,4,  2.0d0)
call A%update(4,1, -1.0d0)
call A%update(4,2,  2.0d0)
call A%update(4,3,  3.0d0)
call A%update(4,4, -1.0d0)

! 1.0      1.0      0.0      3.0
! 2.0      1.0      -1.0      1.0
! 3.0     -1.0      -1.0      2.0
!-1.0      2.0      3.0      -1.0
! >> 
!1.0        1.0        0.0        3.0     
!2.0       -1.0       -1.0       -5.0     
!3.0        4.0        3.0        13.0     
!-1.0       -3.0        0.0       -13.0  

A_CRS = A%to_CRS()
call A_CRS%ILU(0)
do i_i=1,4
    print *, A_CRS%get(i_i,1),A_CRS%get(i_i,2),A_CRS%get(i_i,3),A_CRS%get(i_i,4)
enddo


!call A%remove()
call A%init(3)
call A%update(1,1, 1.0d0)
call A%update(1,2, 6.0d0)
call A%update(1,3, 7.0d0)
call A%update(2,1, 10.0d0)
call A%update(2,2, 20.0d0)
call A%update(2,3, 23.0d0)
call A%update(3,1, 15.0d0)
call A%update(3,2, 50.0d0)
call A%update(3,3, 67.0d0)

A_CRS = A%to_CRS()
! size
print *, A_CRS%size()

! getter/setter

print *, "test >>  getter"
if(A_CRS%get(1,1)==1.0d0) then 
    print *, "[Passed!]", 1,1, A_CRS%get(1,1)
else
    print *, "[Failed!]",A_CRS%get(1,1)
endif
if(A_CRS%get(1,2)==6.0d0) then 
    print *, "[Passed!]", 1,2, A_CRS%get(1,2)
else
    print *, "[Failed!]",A_CRS%get(1,2)
endif
if(A_CRS%get(1,3)==7.0d0) then 
    print *, "[Passed!]", 1,3, A_CRS%get(1,3)
else
    print *, "[Failed!]",A_CRS%get(1,3)
endif
if(A_CRS%get(2,1)==10.0d0) then 
    print *, "[Passed!]", 2,1, A_CRS%get(2,1)
else
    print *, "[Failed!]",A_CRS%get(2,1)
endif
if(A_CRS%get(2,2)==20.0d0) then 
    print *, "[Passed!]", 2,2, A_CRS%get(2,2)
else
    print *, "[Failed!]",A_CRS%get(2,2)
endif
if(A_CRS%get(2,3)==23.0d0) then 
    print *, "[Passed!]", 2,3, A_CRS%get(2,3)
else
    print *, "[Failed!]",A_CRS%get(2,3)
endif
if(A_CRS%get(3,1)==15.0d0) then 
    print *, "[Passed!]", 3,1, A_CRS%get(3,1)
else
    print *, "[Failed!]",A_CRS%get(3,1)
endif
if(A_CRS%get(3,2)==50.0d0) then 
    print *, "[Passed!]", 3,2, A_CRS%get(3,2)
else
    print *, "[Failed!]",A_CRS%get(3,2)
endif
if(A_CRS%get(3,3)==67.0d0) then 
    print *, "[Passed!]", 3,3, A_CRS%get(3,3)
else
    print *, "[Failed!]",A_CRS%get(3,3)
endif

print *, "test >>  setter"
call A_CRS%update(1,1,5.0d0)
if(A_CRS%get(1,1)==5.0d0) then 
    print *, "[Passed!]", 1,1, A_CRS%get(1,1)
else
    print *, "[Failed!]"
endif



print *, "test >> is_nonzero"

print *,  A_CRS%is_nonzero(2,3)," should be T"
print *,  A_CRS%is_nonzero(4,3)," should be F"


call A_CRS%ILU(0)

if( A_CRS%get(1,1)==5.0d0)then
    print *, "[Passed!]", A_CRS%get(1,1)
else
    print *, "Failed!",A_CRS%get(1,1)
endif
if( A_CRS%get(1,2)==6.0d0)then
    print *, "[Passed!]", A_CRS%get(1,2)
else
    print *, "Failed!",A_CRS%get(1,2)
endif
if( A_CRS%get(1,3)==7.0d0)then
    print *, "[Passed!]", A_CRS%get(1,3)
else
    print *, "Failed!",A_CRS%get(1,3)
endif
if( A_CRS%get(2,1)==2.0d0)then
    print *, "[Passed!]", A_CRS%get(2,1)
else
    print *, "Failed!",A_CRS%get(2,1)
endif
if( A_CRS%get(2,2)==8.0d0)then
    print *, "[Passed!]", A_CRS%get(2,2)
else
    print *, "Failed!",A_CRS%get(2,2)
endif
if( A_CRS%get(2,3)==9.0d0)then
    print *, "[Passed!]", A_CRS%get(2,3)
else
    print *, "Failed!",A_CRS%get(2,3)
endif
if( A_CRS%get(3,1)==3.0d0)then
    print *, "[Passed!]", A_CRS%get(3,1)
else
    print *, "Failed!",A_CRS%get(3,1)
endif
if( A_CRS%get(3,2)==4.0d0)then
    print *, "[Passed!]", A_CRS%get(3,2)
else
    print *, "Failed!",A_CRS%get(3,2)
endif
if( A_CRS%get(3,3)==10.0d0)then
    print *, "[Passed!]", A_CRS%get(3,3)
else
    print *, "Failed!",A_CRS%get(3,3)
endif


A_CCS = A_CRS%to_CCS()

! [5,  6,  7]
! [2,  8,  9]
! [3,  4, 10]
true_val = dble([5,2,3,6,8,4,7,9,10])
if(norm(A_CCS%val-true_val) < dble(1.0e-12) )then
    print *, "[Passed!] CRS%to_CRS()"
else
    print *, "Failed >> ERROR NORM :: ",norm(A_CCS%val-true_val)
endif


end