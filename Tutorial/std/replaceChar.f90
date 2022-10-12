use DictionaryClass
implicit none

character(:),allocatable :: line
type(Dictionary_) :: words

line = "Tom Tom Burger"
print *, line
call replace(line, "T", "D")
print *, line

words = split(line,"m ")
print *, words%get(1)
print *, words%get(2)
print *, words%get(3)
print *, words%get(4)


end
