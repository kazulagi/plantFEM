use IOClass
implicit none

type(IO_) :: f
real(real64),allocatable :: real_vector(:)
integer(int32),allocatable :: int_vector(:)

! parse real- or int- vector in json
real_vector = to_vector(f%parse("test.json",key1="real_vector"),num_entity=3)
int_vector    = to_intvector(f%parse("test.json",key1="int_vector"),num_entity=3)

print *, real_vector
print *, int_vector

end