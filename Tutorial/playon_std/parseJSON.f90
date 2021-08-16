use IOClass
implicit none

type(IO_) :: f
character(:),allocatable :: line

! It can only parse strings in JSON files. 

! Before RUNNING it, create a json file

! < test.json >

!{
!    "hello":100000,
!    "info":{
!        "hello":"20000"
!    }
!}

call f%open("test.json","r")

line =  f%parse(key1="info",key2="hello")

print *, trim(line)

print *, fint(line)
print *, freal(line)

print *, fint16(line)
print *, fint32(line)
print *, fint64(line)

print *, freal32(line)
print *, freal64(line)
print *, freal128(line)


call f%close()
end