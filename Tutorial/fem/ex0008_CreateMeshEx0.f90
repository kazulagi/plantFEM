program main
    use iso_fortran_env
    implicit none

    real(real64),allocatable :: NodCoord(:,:)
    integer(int32),allocatable :: ElemNod(:,:)
    integer(int32),allocatable :: ElemMat(:)

    integer(int32) :: num_dim =2 ! 2-dimensional
    integer(int32) :: num_node=9 ! 9 nodes
    integer(int32) :: num_elem=4 ! 4 elements
    integer(int32) :: num_node_per_elem=4 ! 4 nodes per 1 element
    
    allocate(NodCoord(num_node,num_dim) )
    allocate(ElemNod(num_elem,num_node_per_elem) )
    allocate(ElemMat(num_elem))
    
    NodCoord(1,1) = 0.0d0; NodCoord(1,2) = 0.0d0;
    NodCoord(2,1) = 1.0d0; NodCoord(2,2) = 0.0d0;
    NodCoord(3,1) = 2.0d0; NodCoord(3,2) = 0.0d0;
    NodCoord(4,1) = 0.0d0; NodCoord(4,2) = 1.0d0;
    NodCoord(5,1) = 1.0d0; NodCoord(5,2) = 1.0d0;
    NodCoord(6,1) = 2.0d0; NodCoord(6,2) = 1.0d0;
    NodCoord(7,1) = 0.0d0; NodCoord(7,2) = 2.0d0;
    NodCoord(8,1) = 1.0d0; NodCoord(8,2) = 2.0d0;
    NodCoord(9,1) = 2.0d0; NodCoord(9,2) = 2.0d0;

    ElemNod(1,1) = 1; ElemNod(1,2) = 2; ElemNod(1,3) = 5; ElemNod(1,4) = 4; 
    ElemNod(2,1) = 2; ElemNod(2,2) = 3; ElemNod(2,3) = 6; ElemNod(2,4) = 5; 
    ElemNod(3,1) = 4; ElemNod(3,2) = 5; ElemNod(3,3) = 8; ElemNod(3,4) = 7; 
    ElemNod(4,1) = 5; ElemNod(4,2) = 6; ElemNod(4,3) = 9; ElemNod(4,4) = 8; 

    ElemMat(1) = 1;
    ElemMat(2) = 1;
    ElemMat(3) = 1;
    ElemMat(4) = 2;

    ! export
    ! ...略
    
    
end program main
