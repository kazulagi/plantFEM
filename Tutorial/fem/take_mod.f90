use FEMDomainClass
implicit none

type(FEMDomain_) :: culm(1:3),take_mod(1:3),object(1:3)

call take_mod(1)%to_multi_culm(n=10)
call take_mod(2)%to_multi_culm(n=10)
call take_mod(3)%to_multi_culm(n=10)
call take_mod(1)%move(y=-1.0d0)
call take_mod(2)%move(y= 0.0d0)
call take_mod(3)%move(y= 1.0d0)
call take_mod(1)%vtk("take_mod_1")
call take_mod(2)%vtk("take_mod_2")
call take_mod(3)%vtk("take_mod_3")

end

