use IOClass
implicit none

type(IO_) :: f
real(real64),allocatable :: vec(:), mat(:,:)
integer(int32),allocatable :: int_vec(:)

! import csv or txt file as vector
! e.g.
! test_wi.csv
! 100.0,
! 200.0,
! 300.0,
! ...
vec =  f%import("test.csv")
print *, vec(:)

! import csv or txt file as INTEGER vector
! e.g.
! test_wi.csv
! 100,
! 200,
! 300,
! ...
int_vec =  int(f%import("test_wi.csv"))
print *, int_vec(:)


! import csv or txt file as matrix
! e.g.
! test_wi.csv
! 100.0, 200.0, 300.0
! 200.0, 300.0, 400.0
! 300.0, 600.0, 900.0
! ...
mat =  f%import("test.csv",num_column=3)
print *, mat(:,1)

print *, "---------------"
! import csv or txt file as vector, considering its index
! e.g.
! test_wi.csv
! 1, 100.0
! 2, 200.0
! 3, 300.0
! ...

vec =  f%import("test_wi.csv",with_index=true)
print *, vec(:)

end