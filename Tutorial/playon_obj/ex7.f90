program main
    use sim
    implicit none

    type(WaterAbsorption_)::seed
    type(FEMDomain_),target :: water,tissue,tissue2,tissue3
    type(MaterialProp_):: Permiability, YoungsModulus,PoissonRatio
    type(MaterialProp_):: density,Cohesion,phi,psi

    type(MaterialProp_):: a_Psi
    type(MaterialProp_):: a_P
    type(MaterialProp_):: theta_eq
    type(MaterialProp_):: Psi_eq
    type(MaterialProp_):: a_E
    type(MaterialProp_):: a_v
    type(MaterialProp_):: E_eq
    type(MaterialProp_):: v_eq

    type(Boundary_) :: disp_x,disp_y,disp_z
    type(Boundary_) :: traction_x,traction_y,traction_z
    type(Boundary_) :: flux, const 

    

    !  create mesh
    ! From experiment of 2020/6/12, unit:mm, N
    call water%create(Name="water",MeshType="Sphere3D",x_num=12,y_num=11,x_len=9.150d0, y_len=8.150d0,&
        thickness=6.90d0,division=10)
    call tissue%copy(water,onlyMesh=.true.)
    call tissue%rename("tissue")


    ! create material
    ! for deformation analysis
    call YoungsModulus%create(Name="YoungsModulus",ParaValue=13.670d0,Layer=1)
    !call YoungsModulus%create(Name="YoungsModulus",x_max=1500.0d0,x_min=0.0d0,y_max=70.0d0,y_min=0.0d0,&
    !z_max=70.0d0,z_min=0.0d0,ParaValue=6000.0d0,Layer=1)
    call PoissonRatio%create(Name="PoissonRatio",ParaValue=0.30d0,Layer=2)
    !call PoissonRatio%create(Name="PoissonRatio",x_max=100.0d0,x_min=0.0d0,y_max=50.0d0,y_min=0.0d0,&
    !z_max=50.0d0,z_min=0.0d0,ParaValue=0.10d0,Layer=2)
    call Density%create(Name="Density",ParaValue=0.00d0,Layer=3)
    call Cohesion%create(Name="Cohesion",ParaValue=100000000.0d0,Layer=4)
    call Phi%create(Name="Psi",ParaValue=0.0d0,Layer=5)
    call psi%create(Name="psi",ParaValue=0.0d0,Layer=6)

    ! for diffusion analysis
    call Permiability%create(Name="Permiability",ParaValue=0.000010d0,Layer=1)
    !call Permiability%create(Name="Permiability",x_max=300.0d0,x_min=100.0d0,y_max=50.0d0,y_min=0.0d0,&
    !z_max=50.0d0,z_min=0.0d0,ParaValue=1.0d0,Layer=1)
    !call Permiability%create(Name="Permiability",x_max=100.0d0,x_min=0.0d0,y_max=50.0d0,y_min=0.0d0,&
    !z_max=50.0d0,z_min=0.0d0,ParaValue=1.0d0,Layer=1)

    ! for WaterAbsorption analysis
    call a_Psi%create(   Name="a_Psi",   Paravalue=600.0d0,Layer=1)
    !call a_Psi%create(   Name="a_Psi",   Paravalue=0.0d0,Layer=1)
    !call a_P%create(     Name="a_P",     Paravalue=40000.0d0,Layer=1)
    call a_P%create(     Name="a_P",     Paravalue=1200.0d0,Layer=1)

    ! Tugor pressure
    call theta_eq%create(Name="theta_eq",Paravalue=1.0d0,Layer=1)
    !call Psi_eq%create(  Name="Psi_eq",  Paravalue=1.0d0,Layer=1)
    call Psi_eq%create(  Name="Psi_eq",  Paravalue=0.0290d0,Layer=1)
    
    ! Youngs modulus
    call a_E%create(     Name="a_E",     Paravalue=-13.2100d0,Layer=1)
    call a_v%create(     Name="a_v",Paravalue=0.0d0,Layer=1)
    call E_eq%create(    Name="E_eq",Paravalue=0.4580d0,Layer=1)
    call v_eq%create(    Name="v_eq",Paravalue=0.30d0,Layer=1)


    ! import Materials onto FEMDomains
    call tissue%import(Materials=.true., Material=YoungsModulus)
    call tissue%import(Materials=.true., Material=PoissonRatio)
    call tissue%import(Materials=.true., Material=Density)
    call tissue%import(Materials=.true., Material=Cohesion)
    call tissue%import(Materials=.true., Material=Phi)
    call tissue%import(Materials=.true., Material=Psi)
    
    call water%import(Materials=.true., Material=Permiability)

    call seed%import(a_Psi=a_Psi)
    call seed%import(a_P=a_P)
    call seed%import(theta_eq=theta_eq)
    call seed%import(Psi_eq=Psi_eq)
    call seed%import(a_E=a_E)
    call seed%import(a_v=a_v)
    call seed%import(E_eq=E_eq)
    call seed%import(v_eq=v_eq)

    ! visualize material
    call YoungsModulus%gmsh(Name="YoungsModulus",Tag="M : YoungsModulus (kPa)")
    call PoissonRatio%gmsh(Name="PoissonRatio",Tag="M : PoissonRatio")
    call Permiability%gmsh(Name="Permiability",Tag="M : Permiability (cm/s)")
    
    call    a_Psi%gmsh(Name="a_Psi   ", Tag="a_Psi   ")  
    call      a_P%gmsh(Name="a_P     ", Tag="a_P     ")
    call theta_eq%gmsh(Name="theta_eq", Tag="theta_eq")     
    call   Psi_eq%gmsh(Name="Psi_eq  ", Tag="Psi_eq  ")   
    call      a_E%gmsh(Name="a_E     ", Tag="a_E     ")
    call      a_v%gmsh(Name="a_v     ", Tag="a_v     ")
    call     E_eq%gmsh(Name="E_eq    ", Tag="E_eq    ") 
    call     v_eq%gmsh(Name="v_eq    ", Tag="v_eq    ") 
    

    ! createBoundary Conditions
    ! for Deformation analysis
    ! fixed boundary
    ! If multiple values are to be set for a Dirichlet/Neumann/Time Boundary condition,
    ! please use Layer (1, 2, 3...).
    call disp_x%create(Category="Dirichlet",x_max=1.0d0,x_min=-5.0d0,y_max=100.0d0,y_min=-100.0d0,&
    z_max=100.0d0,z_min=-100.0d0,BoundValue=0.0d0,Layer=1,Name="disp_x")
    call disp_y%create(Category="Dirichlet",x_max=1.0d0,x_min=-5.0d0,y_max=100.0d0,y_min=-100.0d0,&
    z_max=100.0d0,z_min=-100.0d0,BoundValue=0.0d0,Layer=2,Name="disp_y")
    call disp_z%create(Category="Dirichlet",x_max=1.0d0,x_min=-5.0d0,y_max=100.0d0,y_min=-100.0d0,&
    z_max=100.0d0,z_min=-100.0d0,BoundValue=0.0d0,Layer=3,Name="disp_z")

    call disp_x%create(Category="Dirichlet",x_max=91.0d0,x_min=85.0d0,y_max=100.0d0,y_min=-100.0d0,&
    z_max=100.0d0,z_min=-100.0d0,BoundValue=0.0d0,Layer=1,Name="disp_x")
    call disp_y%create(Category="Dirichlet",x_max=91.0d0,x_min=85.0d0,y_max=100.0d0,y_min=-100.0d0,&
    z_max=100.0d0,z_min=-100.0d0,BoundValue=0.0d0,Layer=2,Name="disp_y")
    call disp_z%create(Category="Dirichlet",x_max=91.0d0,x_min=85.0d0,y_max=100.0d0,y_min=-100.0d0,&
    z_max=100.0d0,z_min=-100.0d0,BoundValue=0.0d0,Layer=3,Name="disp_z")
    
    ! displacement-loading
    !call disp_z%create(Category="Dirichlet",x_max=310.0d0,x_min=299.0d0,y_max=100.0d0,y_min=-100.0d0,&
    !z_max=100.0d0,z_min=-100.0d0,BoundValue=-10.0d0,Layer=3,Name="disp_z")
    !call disp_z%create(Category="Dirichlet",x_max=310.0d0,x_min=299.0d0,y_max=10.0d0,y_min=-10.0d0,&
    !z_max=10.0d0,z_min=-10.0d0,BoundValue=0.0d0,Layer=3,Name="disp_z")
    !call disp_z%create(Category="Dirichlet",x_max=310.0d0,x_min=299.0d0,y_max=10.0d0,y_min=-10.0d0,&
    !z_max=10.0d0,z_min=-10.0d0,BoundValue=0.0d0,Layer=3,Name="disp_z")
    !call disp_z%create(Category="Dirichlet",x_min=290.0d0,BoundValue=0.0d0,Layer=3,Name="disp_z")
    !call disp_y%create(Category="Dirichlet",x_min=290.0d0,BoundValue=0.0d0,Layer=3,Name="disp_z")
    !call disp_z%create(Category="Dirichlet",x_min=290.0d0,BoundValue=0.0d0,Layer=3,Name="disp_z")
    
    ! traction boundary conditions
    !call traction_x%create("Neumann",x_max=300.0d0,x_min=30.0d0,y_max=120.0d0,y_min=-10.0d0,&
    !    z_max=10.0d0,z_min=-100.0d0,BoundValue=5.0d0)
    !call traction_x%create("Neumann",x_max=-300.0d0,x_min=-400.0d0,y_max=10.0d0,y_min=-1.0d0,&
    !    z_max=1.0d0,z_min=-1.0d0,BoundValue=0.0d0)
    !call traction_x%create("Neumann",x_max=100.0d0,x_min=30.0d0,y_max=520.0d0,y_min=500.0d0,&
    !    z_max=-10.0d0,z_min=-100.0d0,BoundValue=-2.0d0)

    
    ! for Flow analysis
    ! known value
    call const%create(category="Dirichlet",x_max=5.0d0,x_min=-1.0d0,y_max=100.0d0,y_min=-100.0d0,&
    z_max=100.0d0,z_min=-100.0d0,BoundValue=1.0d0,Name="const",Layer=1)
    ! flux boundary
    call const%create(category="Dirichlet",x_max=91.0d0,x_min=85.0d0,y_max=100.0d0,y_min=-100.0d0,&
    z_max=100.0d0,z_min=-100.0d0,BoundValue=1.0d0,Name="const",Layer=1)

    ! visualize on Gmsh
    !call disp_x%gmsh(Dirichlet=.true.,Name="disp_x",Tag="B : disp_x")
    !call disp_y%gmsh(Dirichlet=.true.,Name="disp_y",Tag="B : disp_y")
    !call disp_z%gmsh(Dirichlet=.true.,Name="disp_z",Tag="B : disp_z")
    
    !call traction_x%gmsh(Neumann=.true.,Name="traction_x",Tag="traction_x")
    !call traction_y%gmsh(Neumann=.true.,Name="traction_y",Tag="traction_y")
    !call traction_z%gmsh(Neumann=.true.,Name="traction_z",Tag="traction_z")
    
    !call const%gmsh(Dirichlet=.true.,Name="Constant",Tag="B : Constant")
    
    !call Flux%gmsh(Neumann=.true.,Name="Flux",Tag="Flux")
    

    ! import boundaries onto FEMDomains
    call tissue%import(Boundaries=.true., Boundary=disp_x)
    call tissue%import(Boundaries=.true., Boundary=disp_y)
    call tissue%import(Boundaries=.true., Boundary=disp_z)
    call water%import(Boundaries=.true., Boundary=const)
    !call water%import(Boundaries=.true., Boundary=Flux)

    call tissue%show()
    call water%show()
    
    ! bake data by using templates
    call tissue%bake(template="FiniteDeform_")
    call water%bake(template="DiffusionEq_")    
    
    ! visualize Domains
    call tissue%gmsh(Name="tissue",Tag="Tissue Domain")
    call water%gmsh(Name="water",Tag="Water Domain")
    
    call tissue%export(Name="tissue")
    call water%export(Name="water")

    ! mount data into a seed (An instance of Water-Absorption Solver)
    
    call seed%import(Water=water,Tissue=tissue)
    call seed%bake()
    ! run simulation
    call seed%gnuplot(mode="all")

    call seed%run(timestep=10,dt=1000.0d0,SolverType="BiCGSTAB",&
        Display=.true.,nr_tol=0.010d0,infinitesimal=.true.,interval=100)
    call seed%export(path="../test3",restart=.false.)
    ! visualize data
    !call seed%display()
end program main
