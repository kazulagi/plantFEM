use IOClass
implicit none

type(IO_) :: f
integer(int32) :: n

! binary IO
call f%open("test.bin","w",binary=.true.)
call f%write(100)
call f%close()

call f%open("test.bin","r",binary=.true.)
read(f%fh) n
call f%close()
print *, n


end