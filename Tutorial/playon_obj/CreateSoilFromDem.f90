use SoilClass
implicit none

! create ground
type(Soil_) :: soil
type(Random_) :: random
type(FEMDomain_)::domain
type(DigitalElevationModel_) ::dem
real(real64),allocatable :: strain(:,:),stress(:,:),trSigma(:),allstress(:,:,:)
integer(int32),parameter :: num_node=10
real(real64) :: E, v
integer(int32) :: i,j

! create mesh from Digital Elevetion Model (DEM)

! create dummy DEM data
dem%x = zeros(num_node)
dem%y = zeros(num_node)
dem%z = zeros(num_node)
do i=1, num_node
    dem%x(i) = random%gauss(mu=0.0d0,sigma=10.0d0)
    dem%y(i) = random%gauss(mu=0.0d0,sigma=10.0d0)
    dem%z(i) = random%gauss(mu=0.0d0,sigma=1.0d0) + 10.0d0
enddo
!domain%mesh%nodcoord = zeros(size(dem%x),3 )
!domain%mesh%nodcoord(:,1) = dem%x
!domain%mesh%nodcoord(:,2) = dem%y
!domain%mesh%nodcoord(:,3) = dem%z

!call domain%mesh%meshing()
!call domain%mesh%convertTriangleToRectangular()
!call print(domain%mesh%elemnod)
!
!call domain%vtk("tri")
!call domain%msh("tri")


call soil%import(dem=dem,x_num=20,y_num=20,z_num=6)

call soil%msh("DEM")
call soil%vtk("DEM")

soil%YoungModulus = 10000.0d0
soil%PoissonRatio = 0.30d0
soil%Density      = 1.60d0
soil%VoidRatio    = 0.50d0

call soil%PreFlightCheck()

! only gravity
call soil%deform(z_max=2.0d0,disp=[0.0d0,0.0d0,0.0d0])

!do i=1,soil%femdomain%ne()
!    strain=soil%femdomain%StrainMatrix(ElementID=i,GaussPoint=2,disp=soil%disp)
!enddo
allstress=zeros(soil%femdomain%ne(),3,3 ) 

trSigma = zeros(soil%femdomain%ne() )
do i=1,soil%femdomain%ne()
    E = soil%YoungModulus(i)
    v = soil%PoissonRatio(i)
    stress=soil%femdomain%stressMatrix(ElementID=i,E=E,v=v,disp=soil%disp)
    trSigma(i) = trace(stress)
    allstress(i,:,:) = stress(:,:)
enddo

! result
call soil%vtk("DEM_deformed")
call soil%vtk("DEM_deformed_s11",scalar=trSigma,ElementType=VTK_HEXAHEDRON)
call soil%vtk("DEM_deformed_disp",vector=soil%disp,ElementType=VTK_HEXAHEDRON)
call soil%vtk("DEM_deformed_str",tensor=allstress,ElementType=VTK_HEXAHEDRON)


end