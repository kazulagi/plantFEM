use SoilClass
implicit none

! create ground
type(Soil_) :: soil
type(Random_) :: random
type(DigitalElevationModel_) ::dem
integer(int32) :: i

! create mesh from Digital Elevetion Model (DEM)

! create dummy DEM data
dem%x = zeros(100)
dem%y = zeros(100)
dem%z = zeros(100)
do i=1, 100
    dem%x(i) = random%gauss(mu=0.0d0,sigma=10.0d0)
    dem%y(i) = random%gauss(mu=0.0d0,sigma=10.0d0)
    dem%z(i) = random%gauss(mu=0.0d0,sigma=1.0d0) + 10.0d0
enddo

call soil%import(dem=dem,x_num=20,y_num=20,z_num=6)

call soil%msh("DEM")
call soil%vtk("DEM")

soil%YoungModulus = 100.0d0
soil%PoissonRatio = 0.30d0
soil%Density      = 1.60d0
soil%VoidRatio    = 0.50d0

call soil%PreFlightCheck()

call soil%deform(z_max=1.0d0,disp=[0.0d0,0.0d0,0.0d0])

! result
call soil%vtk("DEM_deformed")


end