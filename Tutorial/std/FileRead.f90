use IOClass
use TimeClass
implicit none

type(IO_) :: f

call f%open("test.json","r")
print *, f%readline()
print *, f%readline()
call f%rewind()
print *, f%readline()
print *, f%readline()
call f%goBack() ! go back 1 line
print *, f%readline()
print *, f%readline()
call f%goBack(lines=2) ! go bach 2 lines
print *, f%readline()
print *, f%readline()
call f%rewind()
print *, f%readline()
call f%goForward(2) ! go forward 2 lines
print *, f%readline()
call f%close()

end