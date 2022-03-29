use LinearSolverClass
use RandomClass
use MPIClass

type(COO_) :: COO
type(CRS_) :: CRS
type(Random_) :: random
real(real64),allocatable :: matmul_vector(:)
type(MPI_) :: mpid


call mpid%start()

call COO%init(10000000)
do i=0,10000000-5
    call COO%add(row=1+i,col=1+i,val=100.0d0)
    call COO%add(row=2+i,col=2+i,val=100.0d0)
    call COO%add(row=3+i,col=3+i,val=100.0d0)
    call COO%add(row=4+i,col=4+i,val=100.0d0)
    call COO%add(row=1+i,col=1+i,val=200.0d0)
    call COO%add(row=2+i,col=2+i,val=200.0d0)
    call COO%add(row=3+i,col=3+i,val=200.0d0)
    call COO%add(row=4+i,col=4+i,val=200.0d0)
    call COO%add(row=1+i,col=2+i,val=200.0d0)
    call COO%add(row=2+i,col=3+i,val=200.0d0)
    call COO%add(row=3+i,col=4+i,val=200.0d0)
    call COO%add(row=4+i,col=4+i,val=200.0d0)
    call COO%add(row=2+i,col=1+i,val=200.0d0)
    call COO%add(row=3+i,col=2+i,val=200.0d0)
    call COO%add(row=4+i,col=3+i,val=200.0d0)
enddo
matmul_vector = random%randn(10000000)
!print *, COO%getAllCol()

CRS = COO%to_CRS()

do i=1,100
    matmul_vector = crs_matvec(CRS,matmul_vector)
enddo
!print *, matmul_vector
!print *, CRS%row_ptr



call COO%remove()

call mpid%end()

end