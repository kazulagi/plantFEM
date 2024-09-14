use MathClass
use IOClass
use ArrayClass
use RandomClass
use TimeClass
implicit none

type(Random_) :: random
integer(int32),allocatable :: Amat(:,:),order(:)
type(IO_) :: f
type(Time_) :: time
integer(int32) :: i, n

n = 10
Amat = int(random%randn(n,4)*4)
call print(Amat)
order = [(i,i=1,size(Amat,1))]
print *, "sort start"
print *, "-"

call sort_and_remove_duplication(Amat,order)
call print(Amat)
print *, "-"
call print(order)

end