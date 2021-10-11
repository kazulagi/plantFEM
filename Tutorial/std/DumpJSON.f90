use IOClass
use ArrayClass
implicit none

type(IO_) :: f

! allocatable array
real(real64),allocatable :: a(:), c(:)
integer(int32),allocatable :: c_int(:)

! dump JSON
a = linspace([0.0d0, 10.0d0],10)
call f%open("test.json","w")
call f%dump("key",a)
call f%dump("key_int",[1,2,3])
call f%dump("hello",1)
call f%dump("hello_real32",1.0)
call f%dump("hello_char","char")
call f%close()

! parse Vector from JSON
c = f%parse("test.json",key1="key") 
call print(c)

! parse Vector from JSON
c_int = f%parse("test.json",key1="key_int") 
call print(c_int)


end