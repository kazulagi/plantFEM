program main
    use iso_fortran_env
    use plantfem
    implicit none

    real(real64),allocatable :: NodCoord(:,:)
    integer(int32),allocatable :: ElemNod(:,:)
    integer(int32),allocatable :: ElemMat(:)
    
    integer(int32) :: xn=2 ! 要素分割数　for X方向
    integer(int32) :: yn=2 ! 要素分割数　for Y方向
    
    real(real64) :: lx=2.0d0 ! x方向長さ
    real(real64) :: ly=2.0d0 ! y方向長さ

    real(real64) :: unitx, unity

    integer(int32) :: i,j,n

    unitx=lx/dble(xn)
    unity=ly/dble(yn)
    
    ! creating rectangular mesh
    allocate(NodCoord( (xn+1)*(yn+1) , 2 ))
    allocate(ElemNod( xn*yn,4) )
    allocate(ElemMat(xn*yn) )

    n=0
    do j=1, yn+1
        do i=1, xn+1
            n=n+1
            NodCoord(n,1)=lx/dble(xn)*dble(i-1)
            NodCoord(n,2)=ly/dble(yn)*dble(j-1)
        enddo
    enddo
    
    n=1
    ElemNod(1,1)=1
    ElemNod(1,2)=2
    ElemNod(1,3)=yn+3
    ElemNod(1,4)=yn+2
    ElemNod(2,1)=2
    ElemNod(2,2)=3
    ElemNod(2,3)=yn+4
    ElemNod(2,4)=yn+3

    
    n=0
    do j=1, yn
        do i=1, xn
            n=n+1
            ElemNod(n,1)=i + (j-1)*(xn+1)
            ElemNod(n,2)=i+1 + (j-1)*(xn+1)
            ElemNod(n,3)=xn+2+i+ (j-1)*(xn+1)
            ElemNod(n,4)=xn+1+i + (j-1)*(xn+1)
            ElemMat(n)=1
        enddo
    enddo

    ElemMat(4)=2
    call print(Nodcoord)
    call print(ElemNod)
    call print(ElemMat)
    ! export
    ! ...略
    
end program main
