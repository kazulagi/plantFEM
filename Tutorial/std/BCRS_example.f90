use SparseClass
implicit none

type(CRS_)  :: a, b, c, d, e
type(BCRS_) ::  bcrs

a = to_crs(ones(2,2))
b = to_crs(2*ones(2,5))
c = to_crs(3*ones(5,2))
d = to_crs(4*ones(5,5))
e = to_crs(5*ones(4,2))

call bcrs%set([1,1],a)
call bcrs%set([1,2],b)
!call bcrs%set([2,1],c)
call bcrs%set([2,2],d)
call bcrs%set([3,3],e)

print *, bcrs%row_range(3,3)
print *, bcrs%col_range(3,3)

call bcrs%showShape()
print *, bcrs%shape()

print *, bcrs%matmul(ones(9))


end