use ArrayClass
implicit none

integer(int32),allocatable :: vec1(:),vec2(:),vec3(:)

vec1 = [1, 2, 3]
vec2 = [2, 4, 6]

vec3 = vec1 // vec2

print *, vec3

end