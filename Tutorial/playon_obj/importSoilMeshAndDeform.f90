program main
    use SoilClass
implicit none


! create ground
type(Soil_) :: soil
real(real64),allocatable :: strain(:,:),stress(:,:),trSigma(:),allstress(:,:,:)
integer(int32),parameter :: num_node=10
real(real64) :: E, v
integer(int32) :: i

! import some DEM file
call soil%import("DEM.vtk")

call soil%msh("DEM2")
call soil%vtk("DEM2")

soil%YoungModulus = 10000.0d0
soil%PoissonRatio = 0.30d0
soil%Density      = 1.60d0
soil%VoidRatio    = 0.50d0

call soil%PreFlightCheck()

! only gravity
! [Notice!] automatically Roller boundaries are introduced
!           in the sides and the bottom.
!           If you want to add an additional boundary conditions, 
!           please enforce them as the following.
call soil%deform(z_max=2.0d0,disp=[0.0d0,0.0d0,0.0d0])

! all stress tensors
allstress=zeros(soil%femdomain%ne(),3,3 ) 

! trace of stress tensors
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

end program main