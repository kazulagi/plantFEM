import plantfem as pf

# create Soybean object
soy = pf.Soybean("Tutorial/obj/soy.json")

# create Light
light = pf.Light(direction=180.0,angle=70.0)

# export only mesh
soy.vtk("mesh")

# compute PPFD (micro-mol/s/m^2)
ppfd = soy.ppfd(light=light)

# export PPFD field
soy.vtk("hello",scalar_field=ppfd)



