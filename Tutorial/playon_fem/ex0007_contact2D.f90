program main
    use plantFEM
    implicit none

    type(FEMDomain_) :: domain1, domain2

    type(Boundary_) :: domain1_disp_x,domain1_disp_y
    type(Boundary_) :: domain2_disp_x,domain2_disp_y
    type(MaterialProp_) :: YoungsModulus, PoissonRatio,Density,Cohesion,Phi,psi
    
    type(ContactMechanics_) :: contact

    ! 2-D contact
    !call domain1%import("path/to/file1")
    !call domain2%import("path/to/file2")


    ! Get mesh
    call domain1%create(meshtype="Box2D")
    call domain1%resize(x=10.0d0, y=10.0d0)

    call domain2%create(meshtype="Box2D")
    call domain2%resize(x=10.0d0, y=10.0d0)
    call domain2%move(x=11.0d0)

    ! Get material information
    call YoungsModulus%create(Name="YoungsModulus",ParaValue=100.0d0,Layer=1) 
    call PoissonRatio%create(Name="PoissonRatio",ParaValue=0.20d0,Layer=2) 
    call Density%create(Name="Density",ParaValue=0.00d0,Layer=3)
    call Cohesion%create(Name="Cohesion",ParaValue=100000000.0d0,Layer=4)
    call Phi%create(Name="Psi",ParaValue=0.0d0,Layer=5)
    call psi%create(Name="psi",ParaValue=0.0d0,Layer=6)

    ! Get boundary conditions
    call domain1_disp_x%create(Category="Dirichlet",BoundValue=0.0d0,&
        x_max=1.0d0,x_min=-1.0d0,Layer=1)!x
    call domain1_disp_y%create(Category="Dirichlet",BoundValue=0.0d0,&
        x_max=1.0d0,x_min=-1.0d0,Layer=2)!y

    call domain2_disp_x%create(Category="Dirichlet",BoundValue=0.0d0,&
        x_max=22.0d0,x_min=20.0d0,Layer=1)!x
    call domain2_disp_y%create(Category="Dirichlet",BoundValue=0.0d0,&
        x_max=22.0d0,x_min=20.0d0,Layer=2)!y

    ! Setup conditions
    call domain1%import(Materials=.true., Material=YoungsModulus)
    call domain1%import(Materials=.true., Material=PoissonRatio)
    call domain1%import(Materials=.true., Material=Density)
    call domain1%import(Materials=.true., Material=Cohesion)
    call domain1%import(Materials=.true., Material=Phi)
    call domain1%import(Materials=.true., Material=Psi)
    call domain1%import(Boundaries=.true. , Boundary=domain1_disp_x)
    call domain1%import(Boundaries=.true. , Boundary=domain1_disp_y)

    call domain2%import(Materials=.true., Material=YoungsModulus)
    call domain2%import(Materials=.true., Material=PoissonRatio)
    call domain2%import(Materials=.true., Material=Density)
    call domain2%import(Materials=.true., Material=Cohesion)
    call domain2%import(Materials=.true., Material=Phi)
    call domain2%import(Materials=.true., Material=Psi)
    call domain2%import(Boundaries=.true. , Boundary=domain2_disp_x)
    call domain2%import(Boundaries=.true. , Boundary=domain2_disp_y)
    
    print *, size(domain1%mesh%nodcoord,2)
    return
    ! setup contact
    !call contact%init(domain1, domain2)
    
    !call contact%properties("contact.json")

    !call contact%run()

end program main
