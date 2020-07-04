program main
    use plantFEM
    implicit none

    type(FEMDomain_) :: bar
    type(Boundary_) :: dbc, nbc
    type(MaterialProp_) :: mp
    type(DiffusionEq_) :: solver

    call bar%create(meshtype="Bar1D",x_len=10.0d0,x_num=10)
    
    call dbc%create(Name="const",Category="Dirichlet",x_max=0.0d0,x_min=0.0d0,BoundValue=1.0d0,Layer=1)
    call nbc%create(Name="flow",Category="Neumann",x_max=10.0d0,x_min=10.0d0,BoundValue=1.0d0,Layer=2)
    
    call mp%create(Name="soil1",x_max=10.0d0,x_min=0.0d0,ParaValue=1.0d0,Layer=1)

    call solver%deploy(bar)
    !call sim%setup()



end program main