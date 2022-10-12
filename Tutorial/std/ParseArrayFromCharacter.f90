use IOClass
use ArrayClass
implicit none

type(IO_) :: f
real(real64),allocatable :: a(:)

! create a file
call f%open("test.txt","w")
call f%write("[1.000, 2.000, 3.000]")
call f%close()

call f%open("test.txt","r")
a = f%readline()

call print(dot_product(a,a) )

end