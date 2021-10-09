program rotate
   use plantfem
   implicit none
   type(FEMDomain_)::obj
   obj%import(OptionalProjectName='1ontact_1_.scf')
   obj%rotate(x=10.00d0, y=20.00d0, z=30.00d0
   obj%export(OptionalProjectName='1ontact_1_.scf')
end program c
