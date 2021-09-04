use FEMDomainClass

type(FEMDomain_) :: domain

call domain%read("test.vtk",ElementType=VTK_QUAD) ! 2-D 4-node isoparametric element @ vtk
call domain%vtk("test2.vtk",ElementType=VTK_QUAD)

end


