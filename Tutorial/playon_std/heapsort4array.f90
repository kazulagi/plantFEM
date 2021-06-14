use ArrayClass

implicit none

integer(int32) :: a(8,5),i
real(real64) :: val(8)
integer(int32) :: indx(8)
type(random_) :: random
a(:,:) = 1
do i=1,8
    val(i) = random%random()
    indx(i) =8- i

    indx(7)=0
    print *,indx(i),val(i)
enddo


a(1,:) = (/1, 1, 2, 51 ,3/)
a(2,:) = (/1, 1, 1, 3 ,18/)
a(3,:) = (/1, 5, 1, 3 ,18/)
a(4,:) = (/2, 1, 3, 2 ,2/)
a(5,:) = (/1, 1, 1, 1 ,2/)
a(6,:) = (/1, 1, 2, 1 ,3/)
a(7,:) = (/1, 1, 1, 1 ,3/)
a(8,:) = (/2, 1, 1, 1 ,3/)



do i=1,8
    print *, a(i,:) , val(i)
enddo

call heapsortArray(a,val)

print *, "Done!"
do i=1,8
    print *, a(i,:) , val(i)
enddo


end