use plantfem
use iso_fortran_env
implicit none
integer(int32),parameter ::  N = 10
integer(int32),parameter ::  NRHS = 1
integer(int32),parameter ::  LDA = 10
integer(int32),parameter ::   LDB=10
real(real64) :: A( LDA, N )
real(real64) ::  IPIV(N)
real(real64):: B( LDB, NRHS )
real(real64),allocatable :: A_0(:,:)

integer(int32) :: INFO, i

type(IO_) :: f

call f%open("input_file.txt","r")
do i=1,10
    read(f%fh,*) A(i,:)
enddo
do i=1,10
    read(f%fh,*) B(i,1)
enddo

A_0 = A
CALL DGESV ( N,NRHS,A,LDA,IPIV,B,LDB,INFO )


call print(B)
call print("-")
call print(matmul(A_0,B))

end