use IOClass
use TimeClass
use FEMDomainClass
implicit none

type(FEMDomain_) :: cube
type(IO_) :: f
type(Time_) :: time

call cube%create("Cube3D",x_num=100,y_num=100,z_num=100)

call time%start()
call f%open("test_async_write.txt","w",async="no")
do i_i=1, cube%nn()
    write(f%fh,*,asynchronous="no") cube%mesh%nodcoord(i_i,:)
enddo
call f%wait()

call time%show()

end