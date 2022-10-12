use plantfem
use omp_lib
implicit none

type(MPI_) :: mpid
type(Soybean_) :: soybean(930)
integer(int32) :: i


call mpid%start()
call mpid%createStack(930)
do i=1,size(mpid%LocalStack)
    call print( mpid%localstack(i) )
    call soybean( mpid%localstack(i) )%init("Tutorial/obj/realSoybeanConfig.json")
    call soybean( mpid%localstack(i) )%remove()
enddo
call mpid%end()

end