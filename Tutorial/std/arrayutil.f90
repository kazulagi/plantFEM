use ArrayClass
implicit none

print *, [-1,2,3] .and. [-10000,-1,5,9]
print *, [6,7,8,9] .get. [3]
print *, [6,7,8,9] .indexOf. [8]

end