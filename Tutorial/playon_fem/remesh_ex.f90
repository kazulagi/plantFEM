use plantfem

type(FEMDomain_) :: mesh

call mesh%create(meshtype="Cube3D")
call mesh%vtk("mesh_10x10x10")
call mesh%remesh(meshtype="Sphere3D",x_num=10,y_num=10,z_num=10)
call mesh%move(x=3.0d0)
call mesh%vtk("mesh_2x2x2")

end