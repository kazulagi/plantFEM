program main
    use sim
    implicit none

    type(WaterAbsorption_)::seed
    type(FEMDomain_) :: water,tissue,tissue2,tissue3
    type(Boundary_) :: disp_x,disp_y,disp_z
    type(Boundary_) :: traction_x,traction_y,traction_z
    type(Boundary_) :: flux, const 

    !  create mesh
    call water%create("rectangular3D",x_num=60,y_num=10,x_len=300.0d0, y_len=50.0d0,&
        thickness=50.0d0,division=10)
    call tissue%copy(water,onlyMesh=.true.)


    ! createBoundary Conditions
    ! for Deformation analysis
    ! fixed boundary
    ! If multiple values are to be set for a Dirichlet/Neumann/Time Boundary condition,
    ! please use Layer (1, 2, 3...).
    call disp_x%create("Dirichlet",x_max=1.0d0,x_min=-5.0d0,y_max=100.0d0,y_min=-100.0d0,&
    z_max=100.0d0,z_min=-100.0d0,BoundValue=0.0d0,Layer=1)
    call disp_y%create("Dirichlet",x_max=1.0d0,x_min=-5.0d0,y_max=100.0d0,y_min=-100.0d0,&
    z_max=100.0d0,z_min=-100.0d0,BoundValue=0.0d0,Layer=2)
    call disp_z%create("Dirichlet",x_max=1.0d0,x_min=-5.0d0,y_max=100.0d0,y_min=-100.0d0,&
    z_max=100.0d0,z_min=-100.0d0,BoundValue=0.0d0,Layer=3)
    
    ! displacement-loading
    call disp_z%create("Dirichlet",x_max=310.0d0,x_min=299.0d0,y_max=100.0d0,y_min=-100.0d0,&
    z_max=100.0d0,z_min=-100.0d0,BoundValue=-10.0d0,Layer=3)
    

    ! traction boundary conditions
    !call traction_x%create("Neumann",x_max=300.0d0,x_min=30.0d0,y_max=120.0d0,y_min=-10.0d0,&
    !    z_max=10.0d0,z_min=-100.0d0,BoundValue=5.0d0)
    !call traction_x%create("Neumann",x_max=-300.0d0,x_min=-400.0d0,y_max=10.0d0,y_min=-1.0d0,&
    !    z_max=1.0d0,z_min=-1.0d0,BoundValue=0.0d0)
    !call traction_x%create("Neumann",x_max=100.0d0,x_min=30.0d0,y_max=520.0d0,y_min=500.0d0,&
    !    z_max=-10.0d0,z_min=-100.0d0,BoundValue=-2.0d0)

    
    ! for Flow analysis
    ! known value
    call const%create("Dirichlet",x_max=1.0d0,x_min=-5.0d0,y_max=100.0d0,y_min=-100.0d0,&
    z_max=100.0d0,z_min=-100.0d0,BoundValue=0.0d0)
    ! flux boundary
    call const%create("Dirichlet",x_max=310.0d0,x_min=299.0d0,y_max=100.0d0,y_min=-100.0d0,&
    z_max=100.0d0,z_min=-100.0d0,BoundValue=20.0d0)


    ! visualize on Gmsh
    call disp_x%gmsh(Dirichlet=.true.,Name="disp_x",Tag="disp_x")
    call disp_y%gmsh(Dirichlet=.true.,Name="disp_y",Tag="disp_y")
    call disp_z%gmsh(Dirichlet=.true.,Name="disp_z",Tag="disp_z")
    
    !call traction_x%gmsh(Neumann=.true.,Name="traction_x",Tag="traction_x")
    !call traction_y%gmsh(Neumann=.true.,Name="traction_y",Tag="traction_y")
    !call traction_z%gmsh(Neumann=.true.,Name="traction_z",Tag="traction_z")
    
    call const%gmsh(Dirichlet=.true.,Name="Constant",Tag="Constant")
    
    !call Flux%gmsh(Neumann=.true.,Name="Flux",Tag="Flux")
    
    ! import boundaries onto FEMDomains
    call tissue%import(Boundaries=.true., Boundary=disp_x)
    call tissue%import(Boundaries=.true., Boundary=disp_y)
    call tissue%import(Boundaries=.true., Boundary=disp_z)
    !call tissue%import(Boundaries=.true., Boundary=traction_x)
    !call tissue%import(Boundaries=.true., Boundary=traction_y)
    !call tissue%import(Boundaries=.true., Boundary=traction_z)
    
    call water%import(Boundaries=.true., Boundary=const)
    !call water%import(Boundaries=.true., Boundary=Flux)



    ! check Domains
    call tissue%gmsh(Name="tissue",Tag="Tissue Domain")
    call water%gmsh(Name="water",Tag="Water Domain")

    ! check boundaries
    call tissue%showBoundaries(Name="tissue")
    call water%showBoundaries(Name="water")


    ! Burn data by using templates
    call tissue%burn(template="FiniteDeform_")
    call water%burn(template="DiffusionEq_")    

    ! mount data into a seed (An instance of Water-Absorption Solver)
    call seed%import(Water=water,Tissue=tissue)

    ! run simulation
    call seed%run(timestep=10)

    ! visualize data
    call seed%display()
    
end program main
