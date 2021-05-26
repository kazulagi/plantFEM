use FEMDomainClass

type(FEMDomain_) :: domain

call domain%read("root.msh",2) !2-D
call domain%vtk("root2")

end