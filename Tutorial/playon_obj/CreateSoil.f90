use BoringClass
use SoilClass
implicit none

! create ground
type(Boring_) :: boring(100)
type(Soil_) :: soil
type(Random_) :: random
type(DigitalElevationModel_) ::dem
integer(int32) :: i

! create sample boring data
! From
! http://www.kankyou.pref.saitama.lg.jp/kankyou/newpdf3/00800075.pdf
do i=1,100
    call boring(i)%example()
    boring(i)%position(1) = 10.0d0*random%random()
    boring(i)%position(2) = 10.0d0*random%random()
enddo

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

call soil%import(dem=dem,x_num=10,y_num=10,z_num=5)

call soil%msh("DEM")
call soil%vtk("DEM")



!call soil%import(boringdata=boring)



end