module OpenMPClass
    use, intrinsic :: iso_fortran_env
    use omp_lib
    implicit none
contains

!#########################################################
subroutine omp_dot_product1(a,b,sum)
    real(real64),intent(in)::a(:),b(:)
    real(real64),intent(out) ::sum
    
!$OMP parallel 
!$omp workshare
sum = dot_product(a,b)    
!$omp end workshare
!$omp end parallel

end subroutine omp_dot_product1
!#########################################################

!#########################################################
subroutine omp_dot_product(a,b,sum)
    real(real64),intent(in)::a(:),b(:)
    real(real64),intent(out) ::sum
    integer(int32) :: i
    real(real64) :: psum
sum=0
!$OMP parallel private(i,psum)
!$omp do reduction(+: sum)
do i=1,size(a)
    sum=sum+a(i)*b(i)
enddo
!$omp end parallel

end subroutine omp_dot_product
!#########################################################


!#########################################################
subroutine omp_dot_product2(a,b,sum)
    real(real64),intent(in)::a(:),b(:)
    real(real64),intent(out) ::sum
    integer(int32) :: i
    real(real64) :: psum
sum=0
!$OMP parallel
!$omp do
do i=1,size(a)
    sum=sum+a(i)*b(i)
enddo
!$omp end parallel

end subroutine omp_dot_product2
!#########################################################

!#########################################################
subroutine omp_matmul(a,b,mm)
    real(real64),intent(in)::a(:,:),b(:)
    real(real64),intent(inout)::mm(:)
    integer(int32) :: i

do i=1,size(a,1)
    !$OMP parallel 
    !$omp workshare
    mm(i) = dot_product(a(i,:),b)    
    !$omp end workshare
    !$omp end parallel

enddo

end subroutine omp_matmul
!#########################################################
end module OpenMPClass
