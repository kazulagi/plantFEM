program main
    use StiffnessMatrixClass
    implicit none

    type(StiffnessMatrix_) :: obj
    type(Mesh_) ::   mesh

    allocate(Mesh%NodCoord(8,3) )
    allocate(Mesh%ElemNod(1,8) )

    Mesh%NodCoord(1,:) = (/0.0d0, 0.0d0, 0.0d0  /))
    Mesh%NodCoord(2,:) = (/1.0d0, 0.0d0, 0.0d0  /))
    Mesh%NodCoord(3,:) = (/1.0d0, 1.0d0, 0.0d0  /))
    Mesh%NodCoord(4,:) = (/0.0d0, 1.0d0, 0.0d0  /))
    Mesh%NodCoord(5,:) = (/0.0d0, 0.0d0, 1.0d0  /))
    Mesh%NodCoord(6,:) = (/1.0d0, 0.0d0, 1.0d0  /))
    Mesh%NodCoord(7,:) = (/1.0d0, 1.0d0, 1.0d0  /))
    Mesh%NodCoord(8,:) = (/0.0d0, 1.0d0, 1.0d0  /))

    do i=1,size(Mesh%ElemNod,2)
        Mesh%ElemNod(1,i)=i
    enddo

    
    call obj%init()
    call obj%update()
end program