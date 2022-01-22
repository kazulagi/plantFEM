use IOClass
implicit none

real(real64),allocatable :: a(:,:)
type(IO_) :: f

a = f%import("test.csv",num_column=2)

a(1,1) = 2.0d0
print *, a(:,1)

call f%export("test2.csv",a)

end