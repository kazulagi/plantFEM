program main
    use ShapeFunctionClass
    use MeshClass
    implicit none

    type(SHapeFunction_) :: sf
    type(Mesh_) :: mesh
    real(real64) :: nodcoord(8,3) ! 3-d
    real(real128) :: root3_per_3,root3
    integer(int32) :: elemnod(1,8) !8-node isoparametric element


    ! ガウス点座標の計算など。
    nodcoord(1,1:3) = (/-1.0d0, -1.0d0, -1.0d0/)
    nodcoord(2,1:3) = (/1.0d0, -1.0d0, -1.0d0/)
    nodcoord(3,1:3) = (/1.0d0, 1.0d0, -1.0d0/)
    nodcoord(4,1:3) = (/-1.0d0, 1.0d0, -1.0d0/)
    nodcoord(5,1:3) = (/-1.0d0, -1.0d0, 1.0d0/)
    nodcoord(6,1:3) = (/1.0d0, -1.0d0, 1.0d0/)
    nodcoord(7,1:3) = (/1.0d0, 1.0d0, 1.0d0/)
    nodcoord(8,1:3) = (/-1.0d0, 1.0d0, 1.0d0/)
    
    elemnod(1,1) = 1
    elemnod(1,2) = 2
    elemnod(1,3) = 3
    elemnod(1,4) = 4
    elemnod(1,5) = 5
    elemnod(1,6) = 6
    elemnod(1,7) = 7
    elemnod(1,8) = 8

    mesh%nodcoord = nodcoord
    mesh%elemnod = elemnod
    sf%ElemType=mesh%GetElemType()
    call sf%SetType()
    call sf%Get(elem_id=1,nod_coord=mesh%NodCoord,&
		elem_nod=mesh%ElemNod,OptionalGpID=4)

    ! shape functions
    print *, "sf%dNdgzi"
    call print(sf%dNdgzi)
    print *, "sf%ElemCoord"
    call print(sf%ElemCoord)

    print *, "Jmat: Jacobi matrix"
    call print(matmul(sf%dNdgzi,sf%ElemCoord) )


    ! おまけ：無理数で割っちゃだめの巻
    print *, 1.0d0/dsqrt(3.0d0)
    print *, dsqrt(3.0d0)/3.0d0
    print *, 0.57735026919
    root3_per_3=1.0d0/dsqrt(3.0d0)
    print *, root3_per_3
    root3_per_3=dsqrt(3.0d0)/3.0d0
    print *, root3_per_3
    print *, " 0.5773502691896257645091487"
    print *, " 0.5773502691896257645091487"
    print *, "compute as real128"
    root3=sqrt(3.0d0)
    root3_per_3=root3/3.0d0
    print *, root3_per_3
    root3=sqrt(3.0d0)
    root3_per_3=1.0d0/root3
    print *, root3_per_3

end program main
