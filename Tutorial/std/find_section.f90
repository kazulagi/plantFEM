use ArrayClass
implicit none

! find inter-node id
! return value :: between a and b as [a,b]
print *, find_section( sorted_list=[0.0d0,1.0d0,2.0d0,3.0d0,5.0d0],given_value=-12.0d0 )
print *, find_section( sorted_list=[0.0d0,1.0d0,2.0d0,3.0d0,5.0d0],given_value=  1.60d0 )
print *, find_section( sorted_list=[0.0d0,1.0d0,2.0d0,3.0d0,5.0d0],given_value=100.60d0 )


end