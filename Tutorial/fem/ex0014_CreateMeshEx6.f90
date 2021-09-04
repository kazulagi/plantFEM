program main
    use plantfem
    implicit none

    type(FEMDomain_) :: domain

    call domain%create(meshtype="Cube",x_num=10,y_num=10,z_num=10)
    call domain%resize(x=1.0d0, y=3.0d0, z=10.0d0)
    call domain%json(name="domain.json")
    call domain%msh(name="domain.msh")

end program main