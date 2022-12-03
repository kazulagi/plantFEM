use SoilClass
implicit none

type(Soil_) :: soil
real(real64):: position(4,2)

position(1,:) = [35.974971, 140.110284]
position(2,:) = [35.974969, 140.110732]
position(3,:) = [35.975853, 140.110765]
position(4,:) = [35.975860, 140.110307]

!position(1,:) = [36.18591928010324, 140.3547876382375]
!position(2,:) = [36.14209134486967, 140.2432176045381]
!position(3,:) = [36.17232012957962, 140.2252465923986]
!position(4,:) = [36.19253491216398, 140.26513929654652]
call soil%init(latitude=position(:,1),longitude=position(:,2),depth=10.0d0,division=[10,10,4])
call soil%vtk("soil")

end
