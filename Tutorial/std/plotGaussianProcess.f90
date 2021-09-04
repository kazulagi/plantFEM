use plantfem 
implicit none

type(IO_) :: f
type(Random_) :: random

! open file
call f%open("gaussian_noise.txt","w")

! i_i is a default loop variable.
! Loop over 1,000 times.
do i_i =1,1000
    ! ... and write ITER, RANDOM NUMBER 
    call f%write(i_i,random%gauss(mu=0.0d0, sigma=0.10d0) )
enddo
call f%close()

! plot graph
call f%plot("gaussian_noise.txt", "w l")

end