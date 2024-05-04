use ArrayClass
use ListClass
implicit none

type(List_) :: my_list

! replace string/character and convert string to list
print *, re("hello abc-def-g!","abc","abc-abc")

my_list = split("1.0,2.0,3.0",",")
call my_list%print()
my_list = to_list("[a,%,3.0]")
call my_list%print()

end