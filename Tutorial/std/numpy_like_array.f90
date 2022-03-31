use ArrayClass
implicit none

type(Array_) :: array1, array2, array3,array4

array1 = to_array([1,2,3],[4,5,6],[7,8,9],dtype="float")
array2 = to_array([0,0,1],dtype="float")
array3 = to_array("eye",ndim=3) ! identity matrix
array4 = to_array("random",ndim=3)

!check array
call print(array4)

! dot product
call print(dot_product(array1, array3) )

! matrix-vector multiplication
array1 = matmul(array1, array2)

array1 = array1%T() ! transpose

! or
!array1 = transpose(array1)

print *, array1%dtype
call print(array1)


print *, array4%dtype
call print(array4)


call array4%set(row=2,col=1, val=2.0d0)
print *, array4%dtype
call print(array4)


end