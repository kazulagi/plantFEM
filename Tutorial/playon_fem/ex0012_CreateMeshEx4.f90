module FEMeshClass
    use iso_fortran_env
    implicit none

    type::FEMesh_
        private
        real(real64),allocatable :: NodCoord(:,:)
        integer(int32),allocatable  :: ElemNod(:,:)
        integer(int32),allocatable :: ElemMat(:)
    contains
        procedure, public :: create => createFEMesh
    end type

contains

subroutine createFEMesh(this,xn, yn, lx, ly)
    class(FEMesh_) :: this

    integer(int32),intent(in) :: xn ! 要素分割数　for X方向
    integer(int32),intent(in) :: yn ! 要素分割数　for Y方向
    
    real(real64),intent(in) :: lx ! x方向長さ
    real(real64),intent(in) :: ly ! y方向長さ

    real(real64) :: unitx, unity

    integer(int32) :: i,j,n

    unitx=lx/dble(xn)
    unity=ly/dble(yn)
    
    ! creating rectangular mesh
    allocate(this%NodCoord( (xn+1)*(yn+1) , 2 ))
    allocate(this%ElemNod( xn*yn,4) )
    allocate(this%ElemMat(xn*yn) )

    n=0
    do j=1, yn+1
        do i=1, xn+1
            n=n+1
            this%NodCoord(n,1)=lx/dble(xn)*dble(i-1)
            this%NodCoord(n,2)=ly/dble(yn)*dble(j-1)
        enddo
    enddo
    
    n=1
    this%ElemNod(1,1)=1
    this%ElemNod(1,2)=2
    this%ElemNod(1,3)=yn+3
    this%ElemNod(1,4)=yn+2
    this%ElemNod(2,1)=2
    this%ElemNod(2,2)=3
    this%ElemNod(2,3)=yn+4
    this%ElemNod(2,4)=yn+3

    
    n=0
    do j=1, yn
        do i=1, xn
            n=n+1
            this%ElemNod(n,1)=i + (j-1)*(xn+1)
            this%ElemNod(n,2)=i+1 + (j-1)*(xn+1)
            this%ElemNod(n,3)=xn+2+i+ (j-1)*(xn+1)
            this%ElemNod(n,4)=xn+1+i + (j-1)*(xn+1)
            this%ElemMat(n)=1
        enddo
    enddo

end subroutine

end module 




program main
    use iso_fortran_env
    use plantfem
    use FEMeshClass
    implicit none

    type(FEMesh_) :: mesh

    ! 引数sと操作sをまとめて1つのクラスにすると、少しすっきりする。
    call mesh%create(xn=2, yn=2, lx=2.0d0, ly=2.0d0)

    ! private宣言により、以下のような参照は不可能

    ! 材料番号のみ修正
    !mesh%ElemMat(4)=2
    
    !call print(mesh%Nodcoord)
    !call print(mesh%ElemNod)
    !call print(mesh%ElemMat)
    
    ! export
    ! ...略
    
end program main
