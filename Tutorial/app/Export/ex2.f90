program main
    use obj
    implicit none

    type(FEMDomain_) :: water
    !  create mesh
    ! From experiment of 2020/6/12, unit:mm, N
    call water%create(Name="water",MeshType="Sphere3D",x_num=12,y_num=11,x_len=9.150d0, y_len=8.150d0,&
    thickness=6.90d0,division=10)
    call water%export(path="../testfd",restart=.true.)

end program