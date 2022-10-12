program main
    use FEMDomainClass
    use FEMSolverClass
    implicit none

    type(FEMDomain_) :: cube(1)
    type(FEMSolver_) :: solver
    integer(int32) :: i,j

    call cube(1)%create("Cube3D")
    call cube(1)%vtk("cube")

    ! short mode:
    call solver%init(FEMDomains=cube(:),DOF=3)

    

end program main