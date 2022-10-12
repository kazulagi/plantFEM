use ArrayClass
implicit none

type(Array_) :: array1, array2

array1 = to_array([1,2,3],[4,5,6],[7,8,9],dtype="float")
array2 = to_array([0,0,1],dtype="float")

! matrix-vector multiplication
array1 = matmul(array1, array2)

!array1 = array1%T() ! transpose
! or
!array1 = transpose(array1)

print *, array1%dtype
call print(array1)

end