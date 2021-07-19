import plantfem as pf

mesh = pf.Mesh()
mesh.read("test.vtk")

f = open("test.txt","w")
f.write(str(mesh.vtkdata.cell_connectivity))

print(mesh.NodCoord)
print(mesh.ElemNod)

mesh.save("test3.vtu",binary=False)

# run serve.f90
plantfem = pf.plantfem()
plantfem.hello(message="Hello, world!")
plantfem.run()