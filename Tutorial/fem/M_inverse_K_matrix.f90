use FEMDomainClass
implicit none

type(FEMDomain_) :: domain
type(CRS_) :: D

!call domain%create("Cube3D",x_num=1,y_num=1,z_num=1)
call domain%create("Cube3D",x_num=3,y_num=3,z_num=30)
call domain%resize(x=5.0d0,y=5.0d0,z=50.0d0)


D = domain%M_inv_K_Matrix(&
    Density=1.00d0*ones(domain%ne() ) ,&
    YoungModulus=1.00d0*ones(domain%ne() ) ,&
    PoissonRatio=0.30d0*ones(domain%ne() ) )

end 