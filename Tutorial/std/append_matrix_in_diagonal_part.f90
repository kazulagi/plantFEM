use ArrayClass
implicit none

! append matrix in diagonal part
call print( eyes(2,2) .diag. 2.0d0*eyes(3,3))
call print( -1*int(eyes(2,2)) .diag. 4*int(eyes(3,3)))

end