use SparseClass
use TimeClass
implicit none

type(CRS_)  :: a, b, c, d, e
type(BCRS_) ::  bcrs
type(Time_) :: time
real(real64),allocatable :: ans(:)
integer(int32) :: n
real(real64) :: x,y
!ans = exp(x)*y
x = 1.0d0; y = 1.0d0
n = 1000*1000
call a%eyes(n)
call b%eyes(n)
a = x*a 
b = x*b
call bcrs%set([1,1],a)
call bcrs%set([2,2],b)

! show shape of BCRS-formatted matrix
call bcrs%showShape()

! compute exp(x)*y
ans = bcrs%exp(y*ones(2*n),max_itr=1000)

print *, ans(1),ans(size(ans)),exp(1.0d0) 

end