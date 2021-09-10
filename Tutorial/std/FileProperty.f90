use IOClass
use TimeClass
implicit none

type(IO_) :: f
type(Time_) :: time

!do 
!    print *, "Last Modified :", f%LastModified("test.txt"),f%lastmodifiedTime
!    print *, "Updated? :", f%updated("test.txt")
!    print *, "Owner ID :", f%owner("test.txt")
!    print *, "File size :", f%size("test.txt")
!    call time%sleep(5)
!enddo

! or
call f%open("test.txt","r")
do 
    print *, "Last Modified :", f%LastModified(),f%lastmodifiedTime
    print *, "Updated? :", f%updated()
    print *, "Owner ID :", f%owner()
    print *, "File size :", f%size()
    call time%sleep(5)
enddo
call f%close()

end