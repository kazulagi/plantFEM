use std
implicit none

integer(int32),allocatable :: A(:,:), X(:),x1

! Same as MATLAB rand
call print( rand() )

call print( rand(3) )

call print( rand(3,3) )


! Same as MATLAB randi

x1 = randi(10)
call print(X1)

X = randi(10,10)
call print(X)

A = randi([-10, 10],5,5)
call print(A)

end