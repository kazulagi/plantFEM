
use EarthClass
implicit none

type(Earth_) :: Earth
type(IO_) :: f
integer(int32) :: nYear  = 2022
integer(int32) :: nMonth = 7
integer(int32) :: nDay   = 10
integer(int32) :: nHour  = 0
integer(int32) :: nMinute= 50
integer(int32) :: nSecond= 10

call f%open("daytime.txt")
do i_i=1,23
    nhour = nhour + 1
    print *, earth%getSunPosition(&
        Now=[nYear,nMonth,nDay,nHour,nMinute,nSecond])
    write(f%fh,*) earth%getSunPosition(&
    Now=[nYear,nMonth,nDay,nHour,nMinute,nSecond])
enddo
call f%close()
call f%plot(option="with lines")

end