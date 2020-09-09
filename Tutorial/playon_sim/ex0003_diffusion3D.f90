program main
    use sim
    implicit none

    type(FEMDomain_),target :: domain
    type(MaterialProp_) :: Permiability
    type(Boundary_) :: Dirichlet,Initial1,Initial2
    type(DiffusionEq_) :: Solver
    integer(int32) :: i

    call domain%create(meshtype="rectangular3D",x_num=20,y_num=20,z_num=10,&
        x_len=50.0d0,y_len=50.0d0,z_len=10.0d0)

    !call domain%create(meshtype="Sphere3D",x_num=10,y_num=10,z_num=10,&
    !    x_len=50.0d0,y_len=20.0d0,z_len=10.0d0)
    !call domain%gmsh(Name="test")

    call Permiability%create(Name="Permiability",ParaValue=0.10d0,Layer=1) 
    call Dirichlet%create(Category="Dirichlet",BoundValue=1.0d0,x_max=1.0d0,x_min=-1.0d0,Layer=1)
    call Dirichlet%create(Category="Dirichlet",BoundValue=5.0d0,x_max=51.0d0,x_min=49.0d0,Layer=1)
    call Initial2%create(Category="Time",BoundValue=-10.0d0,x_max=20.0d0,x_min=40.0d0,z_min=7.0d0,Layer=1)

    call domain%import(Materials=.true. , Material=Permiability)
    call domain%import(Boundaries=.true. , Boundary=Dirichlet)
    !call domain%import(Boundaries=.true. , Boundary=Initial1)
    call domain%import(Boundaries=.true. , Boundary=Initial2)

    call domain%bake(template="DiffusionEq_" )

    solver%FEMDomain => domain

    solver%dt=1.0d0
    
    call solver%setup()
    call solver%solve(solvertype="BiCGSTAB")
    call solver%display(Name="res",optionalstep=0,displaymode="gmsh")
    do i=1,10
        call solver%update()
        call solver%solve(solvertype="BiCGSTAB")
        call solver%display(Name="res",optionalstep=i,displaymode="gmsh")
    enddo

end program main