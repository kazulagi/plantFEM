use ArrayClass
use IOClass
implicit none

! H-stack
call print( zeros(3,2) .h. ones(3,2) .h. 2*ones(3,2) )
! H-stack
call print( zeros(3) .h. ones(3,2) .h. 2*ones(3,2) )
! V-stack
call print( zeros(3,2) .v. ones(3,2) .v. 2*ones(3,2) )
! V-stack
call print( zeros(3) .v. ones(3,2) .v. 2*ones(3,2) )
end