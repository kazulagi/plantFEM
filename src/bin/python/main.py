import plantfem as pf

soy = pf.soybean()
soy.create()
soy.msh(name="hello_soy")
soy.run()