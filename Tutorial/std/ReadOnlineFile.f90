use IOClass
implicit none

type(IO_) :: f

call f%open("https://raw.githubusercontent.com/kazulagi/plantFEM/master/Tutorial/obj/realSoybeanConfig.json","r")
do while(.not.f%EOF)
    print *, f%readline()
enddo
call f%close()

end