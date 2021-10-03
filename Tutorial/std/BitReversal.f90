use BitClass
implicit none

type(Bit_) :: bit
integer(int32) :: i

![000]~[111]
call bit%init(n=3)

! Bit reversal
do i=0,7
    print *, "-----------"
    bit = i
    print *,  bit%int()
    bit = reverse(bit)
    print *,  bit%int()
enddo

end