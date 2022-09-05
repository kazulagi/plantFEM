use COOClass
implicit none

type(COO_) :: A
type(CRS_) :: A_
real(real64),allocatable :: RHS(:)

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

A_ = A%to_CRS()

! size
print *, A_%size()

! getter/setter

print *, "test >>  getter"
if(A_%get(1,1)==1.0d0) then 
    print *, "[Passed!]", 1,1, A_%get(1,1)
else
    print *, "[Failed!]"
endif
if(A_%get(1,2)==6.0d0) then 
    print *, "[Passed!]", 1,2, A_%get(1,2)
else
    print *, "[Failed!]"
endif
if(A_%get(1,3)==7.0d0) then 
    print *, "[Passed!]", 1,3, A_%get(1,3)
else
    print *, "[Failed!]"
endif
if(A_%get(2,1)==10.0d0) then 
    print *, "[Passed!]", 2,1, A_%get(2,1)
else
    print *, "[Failed!]"
endif
if(A_%get(2,2)==20.0d0) then 
    print *, "[Passed!]", 2,2, A_%get(2,2)
else
    print *, "[Failed!]"
endif
if(A_%get(2,3)==23.0d0) then 
    print *, "[Passed!]", 2,3, A_%get(2,3)
else
    print *, "[Failed!]"
endif
if(A_%get(3,1)==15.0d0) then 
    print *, "[Passed!]", 3,1, A_%get(3,1)
else
    print *, "[Failed!]"
endif
if(A_%get(3,2)==50.0d0) then 
    print *, "[Passed!]", 3,2, A_%get(3,2)
else
    print *, "[Failed!]"
endif
if(A_%get(3,3)==67.0d0) then 
    print *, "[Passed!]", 3,3, A_%get(3,3)
else
    print *, "[Failed!]"
endif

print *, "test >>  setter"
call A_%update(1,1,5.0d0)

if(A_%get(1,1)==5.0d0) then 
    print *, "[Passed!]", 1,1, A_%get(1,1)
else
    print *, "[Failed!]"
endif



print *, "test >> is_nonzero"

print *,  A_%is_nonzero(2,3)," should be T"
print *,  A_%is_nonzero(4,3)," should be F"


RHS = [2.0d0,3.0d0,4.0d0]
call A_%ILU(0, RHS=RHS)
print *, "RHS",RHS
print *, " should be"
print *, [0.540d0,-0.350d0,0.20d0]



if( A_%get(1,1)==5.0d0)then
    print *, "[Passed!]", A_%get(1,1)
else
    print *, "Failed!"
endif
if( A_%get(1,2)==6.0d0)then
    print *, "[Passed!]", A_%get(1,2)
else
    print *, "Failed!"
endif
if( A_%get(1,3)==7.0d0)then
    print *, "[Passed!]", A_%get(1,3)
else
    print *, "Failed!"
endif
if( A_%get(2,1)==2.0d0)then
    print *, "[Passed!]", A_%get(2,1)
else
    print *, "Failed!"
endif
if( A_%get(2,2)==8.0d0)then
    print *, "[Passed!]", A_%get(2,2)
else
    print *, "Failed!"
endif
if( A_%get(2,3)==9.0d0)then
    print *, "[Passed!]", A_%get(2,3)
else
    print *, "Failed!"
endif
if( A_%get(3,1)==3.0d0)then
    print *, "[Passed!]", A_%get(3,1)
else
    print *, "Failed!"
endif
if( A_%get(3,2)==4.0d0)then
    print *, "[Passed!]", A_%get(3,2)
else
    print *, "Failed!"
endif
if( A_%get(3,3)==10.0d0)then
    print *, "[Passed!]", A_%get(3,3)
else
    print *, "Failed!"
endif




end