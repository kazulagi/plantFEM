module VectorClass
    use iso_fortran_env
    use ArrayClass
    implicit none

    type :: Vector_
        logical :: real, float, int, char
        integer(int32) :: VectorSize, RowSize
        integer(int32),allocatable :: intvector(:,:)
        integer(int32),allocatable :: realvector(:,:)
    contains
        procedure, public :: init => initVector
        procedure, public :: copy => copyVector
    end type

contains

! ############################################
subroutine initVector(obj,real, float, int, char,VectorSize, RowSize)
    class(Vector_) ,intent(inout) :: obj
    logical,optional,intent(in) :: real, float, int, char
    integer(int32),optional,intent(in) :: VectorSize, RowSize
    integer(int32) :: i,j,n,m
    if(allocated(obj%intVector) )then
        deallocate(obj%intVector)
    endif
    if(allocated(obj%RealVector) )then
        deallocate(obj%RealVector)
    endif
    n=input(default=1,option=VectorSize)
    m=input(default=1,option=RowSize)
    
    
    if(present(real) )then
        if(real .eqv. .true.)then
            allocate(obj%realVector(n,m) )
            obj%realVector(:,:)=0.0d0
            obj%real = .true.
        endif
    endif

    if(present(int) )then
        if(int .eqv. .true.)then
            allocate(obj%intVector(n,m) )
            obj%intVector(:,:)=0.0d0
            obj%int = .true.
        endif
    endif

end subroutine initVector
! ############################################
subroutine copyVector(obj,vector)
    class(Vector_) ,intent(inout) :: obj
    class(Vector_) ,intent(in) :: vector
    integer(int32) :: n,m
    if(allocated(obj%intVector) )then
        deallocate(obj%intVector)
    endif
    if(allocated(obj%RealVector) )then
        deallocate(obj%RealVector)
    endif
    
    if(allocated(Vector%intVector) )then
        n=size(obj%intVector,1) 
        m=size(obj%intVector,2) 
        allocate(obj%intVector(n,m) )
        obj%intVector(:,:)=vector%intVector(:,:)
    endif
    if(allocated(Vector%RealVector) )then
        n=size(obj%RealVector,1) 
        m=size(obj%RealVector,2) 
        obj%realVector(:,:)=vector%realVector(:,:)
        allocate(obj%intVector(n,m) )
    endif
        
end subroutine copyVector
! ############################################
end module VectorClass