program main
    use fem
    implicit none

    type(FEMDomain_) :: domain
    type(MPI_) :: mpid

    call mpid%start()
    
    call domain%create(meshtype="rectangular3D",x_num=5,y_num=5,z_num=10,&
        x_len=5.0d0,y_len=5.0d0,z_len=10.0d0)
        
    call domain%distribute(mpid)

    call mpid%end()
end program