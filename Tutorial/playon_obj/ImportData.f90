program main
    use sim
    implicit none

    type(WaterAbsorption_)::seed
    type(FEMDomain_) :: water,tissue
    type(Boundary_) :: 

    !  create mesh
    call water%create("rectangular3D",x_num=10,y_num=10,x_len=10.0d0, y_len=20.0d0,&
        thickness=5.0d0,division=5)
    call tissue%create("rectangular3D",x_num=10,y_num=10,x_len=10.0d0, y_len=20.0d0,&
        thickness=5.0d0,division=5)
    call tissue%move(x=10.0d0, y=2.0d0, z=1.0d0)

    ! createBoundary
    call tissue%setBoundary(new=.true.,x_max=-10.0d0,x_min=10.0d0,y_max=-10.0d0,y_min=-20.0d0,&
        z_max=-50.0d0,z_min=5.0d0, value=2.0d0)
    call tissue%Boundary%gmsh(Name="DBmesh",Dirichlet=.true.)

    call tissue%export(Name="tissuefile",OptionalFileFormat="stl")
    call water%export(Name="waterfile",OptionalFileFormat="stl")

    call water%GmshPlotMesh(Name="water")
    call tissue%GmshPlotMesh(Name="tissue")

    ! import water and tissue
    call seed%import(Water=Water, Tissue=tissue)
    call seed%export(Name="WaterAndTissue")

end program main
