
use std
implicit none

real(real64),allocatable :: xyz(:,:)

! sample data
xyz = parse(name="Tutorial/std/test.dat",row=[2,11],col=[24,41],num_col=3 )
call print(xyz)

end 