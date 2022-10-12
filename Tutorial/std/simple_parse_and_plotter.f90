
use std
implicit none

type(IO_) :: f
real(real64),allocatable :: dat(:,:)
integer(int32) :: i

! sample data
dat = parse(name="Tutorial/std/test.dat",row=[2,11],col=[24,41],num_col=3 )

! col #1 vs col #2 
call f%plot(x=dat(:,1), fx=dat(:,2), option="with lines" )

! col #2 vs col #3 
call f%plot(x=dat(:,2), fx=dat(:,3), option="with lines" )

! sequential plot
call f%plot(x=dble([ (i,i=1,size(dat,1)) ]), fx=dat(:,2), option="with lines" )


end 