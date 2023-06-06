use ArrayClass
use PanicleClass
use SeismicAnalysisClass
implicit none

type(FEMDomain_) :: wheat
type(SeismicAnalysis_) :: seismic
real(real64),allocatable :: Vs(:),x(:),YoungModulus(:),PoissonRatio(:),Density(:)
integer(int32),allocatable :: fix_nodes(:)
real(real64) :: boundary_depth
integer(int32) :: i

! create mesh
! unit : m

wheat = to_wheat_panicle_mesh(&
        num_seed_column         = 10,&
        panicle_seed_interval   = 4.0d0/1000.0d0,&
        panicle_seed_diameter   = 5.0d0/1000.0d0,&
        panicle_seed_length     = 10.0d0/1000.0d0,&
        panicle_panicle_diameter     = 2.0d0/1000.0d0, &
        culm_length = 1.0d0,&
        culm_division = 100,& 
        heights_vs_diameters= &
            [0.00d0 , 6.0d0/1000.0d0] .n. & ! height & diameter
            [0.40d0 , 7.0d0/1000.0d0] .n. & ! height & diameter
            [0.80d0 , 2.0d0/1000.0d0]     & ! height & diameter
        )

call wheat%vtk("wheat")


Density      = zeros(wheat%ne() ) 
PoissonRatio = zeros(wheat%ne() ) 
YoungModulus = zeros(wheat%ne() ) 

! E = Vs*Vs/density * 2*(1+v)
Vs = 120.0d0*ones(wheat%ne())
Density      = 0.70d0
PoissonRatio = 0.330d0
YoungModulus = Vs(:)*Vs(:)/Density(:)*2.0d0*(1.0d0+PoissonRatio(:) )
! fix boundaries
fix_nodes = wheat%getNodeList(zmax=0.10d0 )


call seismic%modalAnalysis(&
    femdomain=wheat,&
    Density = Density,&
    YoungModulus=YoungModulus,&
    PoissonRatio=PoissonRatio,&
    fix_node_list_x=fix_nodes,&
    fix_node_list_y=fix_nodes,&
    fix_node_list_z=fix_nodes &
    )
call seismic%vtk(name="seismic",num_mode=20,amp=1.0d0,with_stress=[1,2])

print *, size(seismic%modeVectors,1),size(seismic%modeVectors,2),wheat%nn()*3

print *, seismic%frequency(1:20)


end 
