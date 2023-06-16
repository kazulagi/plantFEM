use FEMDomainClass
implicit none

type(FEMDomain_) :: cube

call cube%create("Cube3D",x_num=2,y_num=2,z_num=2)
call cube%vtk("before",scalar=linspace([1.0d0,dble(cube%ne() ) ],cube%ne()) )

! refine element
call cube%refine(ElementID=[1,4,3])
call cube%vtk("cube",scalar=linspace([1.0d0,dble(cube%ne() ) ],cube%ne()) )

end