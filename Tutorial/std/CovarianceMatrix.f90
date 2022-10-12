program main
    use std
    implicit none

    real(real64),parameter :: dt = 0.010d0
    type(Random_) :: random
    type(Math_) :: math
    type(IO_) :: f
    integer(int32) :: i,j
    real(real64) :: ratio = 10000.0d0
    real(real64),allocatable ::dataset1(:),dataset2(:),dataset0(:),&
        darray1(:,:),darray2(:,:)
    
    ! 10 dimension, 20 samples
    darray1=zeros(10,1024)
    darray2=zeros(10,1024) 
    do i=1,size(darray1,1)
        do j=1,size(darray1,2)
            darray1(i,j) = random%gauss(mu=1.0d0, sigma=1.0d0)*sin(2.0d0*math%PI*ratio)
        enddo
    enddo

    do i=1,size(darray2,1)
        do j=1,size(darray2,2)
            darray2(i,j) = darray1(i,j)*sin(-2.0d0*math%PI*ratio)
        enddo
    enddo

    print *, "average Vector/ dimension = 10"
    call print(averageVector(darray1,n=10))
    call f%open("covarianceMatrix.csv")
    print *, "covariance Matrix/ dimension = 10"
    call f%write(covarianceMatrix(darray1,darray2,n=10))
    call f%close()

end program main