# sample code for python-api of plantfem
import plantfem as pf

# create untitled.f90
plantfem = pf.plantfem()

# sey hello
plantfem.hello(message="Hello, world!")

# create objects
plantfem.soybean(x=0.0,y=0.0,z=0.0)
plantfem.soil(x=10.0)

# run 
plantfem.run()