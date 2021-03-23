import plantfem as pf

soy = pf.soybean(name="hello_soy")
soy.create()
soy.msh(name="hello_soy")

soy.run()