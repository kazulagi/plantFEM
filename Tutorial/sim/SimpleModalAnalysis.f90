use ArrayClass
use SeismicAnalysisClass
implicit none

type(FEMDomain_) :: soil
type(SeismicAnalysis_) :: seismic
real(real64),allocatable :: Vs(:),x(:),YoungModulus(:),PoissonRatio(:),Density(:)
integer(int32),allocatable :: fix_nodes(:)
real(real64) :: boundary_depth
integer(int32) :: i

! create mesh
call soil%create(meshtype="Cube3D",x_num=12,y_num=12,z_num=5)
call soil%vtk("soil")
call soil%resize(x=2000.0d0,y=2000.0d0,z=1000.0d0)
call soil%move(x=-1000.0d0,y=-1000.0d0,z=-1000.0d0)
Vs = zeros(soil%ne())
do i=1,soil%ne()
    x = soil%centerPosition(ElementID=i)
    boundary_depth = 0.0010d0*x(1)**2 + 0.0010d0*x(2)**2  - 600.0d0
    if(x(3) < boundary_depth )then
        Vs(i) = 3000.0d0 ! 3,000 m/s
    else
        Vs(i) = 300.0d0 ! 300 m/s
    endif
enddo
call soil%vtk("Vs",scalar=Vs)


Density      = zeros(soil%ne() ) 
PoissonRatio = zeros(soil%ne() ) 
YoungModulus = zeros(soil%ne() ) 

! E = Vs*Vs/density * 2*(1+v)
Density      = 1.70d0
PoissonRatio = 0.330d0
YoungModulus = Vs(:)*Vs(:)/Density(:)*2.0d0*(1.0d0+PoissonRatio(:) )
! fix boundaries
fix_nodes = soil%getNodeList(zmax=soil%zmin() )
fix_nodes = fix_nodes // soil%getNodeList(xmax=soil%xmin() ) !append
fix_nodes = fix_nodes // soil%getNodeList(xmin=soil%xmax() )
fix_nodes = fix_nodes // soil%getNodeList(ymax=soil%ymin() )
fix_nodes = fix_nodes // soil%getNodeList(ymin=soil%ymax() )

call seismic%modalAnalysis(&
    femdomain=soil,&
    Density = Density,&
    YoungModulus=YoungModulus,&
    PoissonRatio=PoissonRatio,&
    fix_node_list_x=fix_nodes,&
    fix_node_list_y=fix_nodes,&
    fix_node_list_z=fix_nodes &
    )
call seismic%vtk(name="seismic",num_mode=20,amp=1000000.0d0,scalar_field=Vs)

print *, size(seismic%modeVectors,1),size(seismic%modeVectors,2),soil%nn()*3
    
print *, seismic%frequency(1:20)
!call print(seismic%frequency(1:20) )

! Theoretical formula (Two-layer)
! T = 4*H/Vs = 8.0 sec

! Numerical result
! Fundamental: 0.15 Hz
! T = 1 / 0.15 = 6.667 sec

end 
