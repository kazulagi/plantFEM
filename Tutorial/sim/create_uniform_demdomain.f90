use DemDomainClass
implicit none

type(DEMDomain_) :: dem
type(CRS_) :: Kmat
type(Time_) :: time

call time%start()
call dem%closepack(radius=1.0d0,length=[3.0d0,3.0d0,2.0d0])
Kmat = dem%getStiffnessMatrix()
call time%show()
print *, dem%np()
call print(real(Kmat%to_Dense()))
call dem%vtk("test.vtk")

end
