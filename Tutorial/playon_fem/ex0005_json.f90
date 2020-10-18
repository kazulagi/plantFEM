program main
    use plantFEM
    implicit none

    type(FEMDomain_) :: domain
    type(IO_) :: f

    call domain%create(meshtype="Cylinder",x_num=10,y_num=10,z_num=10,x_len=10.0d0,y_len=3.0d0,z_len=2.0d0)
    call domain%json(name="domain.json",endl=.true.)


end program main