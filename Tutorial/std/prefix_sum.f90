use ArrayClass
implicit none

print *, prefix_sum( [1,2,3,4] )
print *, prefix_sum( [-1.0,3.0,2.0,1.0] )
print *, prefix_sum( [-1.0d0,3.0d0,2.0d0,1.0d0] )

end