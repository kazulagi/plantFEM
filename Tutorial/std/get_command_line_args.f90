use std
use MathClass
implicit none

type(List_) :: list1,list2

! get command line argument as list
! same as sys.argv(), but index should be start from 1,
! index 0  does not indicates the program name

list1 = argv()
list2 = argv()
list1 = list1 // list2

! ./a.out hello world 123

print *, list1 .get. 1 ! hello
print *, list1 .get. 2 ! world
print *, float(list1 .get. 3) !123.00000000000

end