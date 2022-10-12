use plantfem 
implicit none

integer(int32) :: i

do i = 0,61
    print *, char(ichar("A")+i )
enddo

end