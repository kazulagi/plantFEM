program main
    use plantFEM
    implicit none

    type(FEMDomain_) :: domain ! FEM data-structure
    integer(int32) :: i

    call domain%create(meshtype="rectangular3D",x_num=10,y_num=10,z_num=10,&
    x_len=10.0d0,y_len=10.0d0,z_len=10.0d0)
    
    call domain%divide(i)
    
    
end program main 
