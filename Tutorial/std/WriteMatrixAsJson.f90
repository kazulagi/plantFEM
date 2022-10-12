use IOClass
use ArrayClass
implicit none

real(real64),allocatable :: a(:)
real(real64),allocatable :: mat(:,:)
type(IO_) :: f

a = "[1.0, 2.0, 3.0, 4.0, 2.0, 1.0, 0.0, -1.0]"
mat = reshape(a,2,4)

call f%open("test.json","w")
call f%dump(key="mat",valueVector=mat)
call f%dump(key="mat_int",valueVector=int(mat))
call f%close()

end