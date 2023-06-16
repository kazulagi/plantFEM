use MathClass
implicit none

real(real64),allocatable :: a(:)
a = dble( [1, 2, 5, 6, -2] )

print *, "Original"
print *, a
call heapsort(size(a),a )
print *, "Sorted"
print *, a

end