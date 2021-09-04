program main
    use sim
    implicit none

    type(FEMDomain_),target :: domain
    type(MaterialProp_) :: YoungsModulus, PoissonRatio,Density,Cohesion,Phi,psi
    type(Boundary_) :: disp_x,disp_y,disp_z
    type(IO_) :: f
    type(FiniteDeform_) :: Solver
    integer(int32) :: i

    call domain%create(meshtype="rectangular3D",x_num=10,y_num=10,z_num=10,&
        x_len=50.0d0,y_len=20.0d0,z_len=10.0d0)

    !call domain%create(meshtype="Sphere3D",x_num=10,y_num=10,z_num=10,&
    !    x_len=50.0d0,y_len=20.0d0,z_len=10.0d0)
    !call domain%gmsh(Name="test")

    call YoungsModulus%create(Name="YoungsModulus",ParaValue=100.0d0,Layer=1) 
    call PoissonRatio%create(Name="PoissonRatio",ParaValue=0.20d0,Layer=2) 
    call Density%create(Name="Density",ParaValue=0.00d0,Layer=3)
    call Cohesion%create(Name="Cohesion",ParaValue=100000000.0d0,Layer=4)
    call Phi%create(Name="Psi",ParaValue=0.0d0,Layer=5)
    call psi%create(Name="psi",ParaValue=0.0d0,Layer=6)

    call disp_x%create(Category="Dirichlet",BoundValue=0.0d0,x_max=1.0d0,x_min=-1.0d0,Layer=1)!x
    call disp_y%create(Category="Dirichlet",BoundValue=0.0d0,x_max=1.0d0,x_min=-1.0d0,Layer=2)!y
    call disp_z%create(Category="Dirichlet",BoundValue=0.0d0,x_max=1.0d0,x_min=-1.0d0,Layer=3)!z
    call disp_x%create(Category="Dirichlet",BoundValue=5.0d0,x_max=51.0d0,x_min=40.0d0,Layer=1)

    call domain%import(Materials=.true., Material=YoungsModulus)
    call domain%import(Materials=.true., Material=PoissonRatio)
    call domain%import(Materials=.true., Material=Density)
    call domain%import(Materials=.true., Material=Cohesion)
    call domain%import(Materials=.true., Material=Phi)
    call domain%import(Materials=.true., Material=Psi)

    call domain%import(Boundaries=.true. , Boundary=disp_x)
    call domain%import(Boundaries=.true. , Boundary=disp_y)
    call domain%import(Boundaries=.true. , Boundary=disp_z)

    call domain%bake(template="FiniteDeform_")

    solver%FEMDomain => domain

    solver%dt=1.0d0
    solver%infinitesimal = .true.

    call solver%DivideBC()
    call solver%solve(solvertype="BiCGSTAB")
    do i=1,10
        call solver%UpdateInitConfig()
        call solver%UpdateBC()
        call solver%solve(solvertype="BiCGSTAB")
        call solver%display(DisplayMode="gmsh",OptionalStep=i,Name="deform")
    enddo

end program main