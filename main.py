import plantfem as pf

soy = pf.soybean(name="hello_soy")
soy.create(config="./plantfem/Tutorial/playon_obj/realSoybeanConfig.json")
soy.msh(name="hello_soy")
soy.json(name="hello_soy")
soy.stl(name="hello_soy")

# path to plantfem
soy.run(path="./plantfem")