use SeismicAnalysisClass
implicit none

! simple example of modal analysis for EbO-FEM.

type(SeismicAnalysis_) :: seis
type(FEMDomain_),allocatable :: femdomains(:)
integer(int32),allocatable :: connectivity(:,:)

real(real64) :: YoungModulus1, YoungModulus2
real(real64) :: PoissonRatio1, PoissonRatio2
real(real64) :: Density1, Density2

allocate(femdomains(2) )

! Mesh generation
call femdomains(1)%create("Cube3D",x_num=30,y_num=4,z_num=2)
call femdomains(1)%resize(x=0.30d0,y=0.040d0,z=0.010d0)
call femdomains(1)%vtk("bar1")

call femdomains(2)%create("Cube3D",x_num=30,y_num=4,z_num=2)
call femdomains(2)%resize(x=0.30d0,y=0.040d0,z=0.010d0)
call femdomains(2)%move(x=0.250d0)
call femdomains(2)%vtk("bar2")

! overlapping connectivity
connectivity = zeros(2,2)
connectivity(1,1:2) = [1, 2] !domain 1 to domain 2
connectivity(2,1:2) = [2, 1] !domain 2 to domain 1

! Material parameters
YoungModulus1 = 10000.0d0*1000.0d0
YoungModulus2 = 10000.0d0*1000.0d0
PoissonRatio1 = 0.30d0
PoissonRatio2 = 0.30d0
Density1 = 1.0d0
Density2 = 1.0d0


call seis%modalAnalysis(&
    femdomains=femdomains,&
    connectivity=connectivity,&
    YoungModulus=YoungModulus1*eyes(femdomains(1)%ne() ) &
                // YoungModulus2*eyes(femdomains(2)%ne() ),&
    PoissonRatio=PoissonRatio1*eyes(femdomains(1)%ne() ) &
                // PoissonRatio2*eyes(femdomains(2)%ne() ),&
    Density=Density1*eyes(femdomains(1)%ne() ) &
                // Density2*eyes(femdomains(2)%ne() ),&
    fix_node_list_x=femdomains(1)%select(x_max=0.0010d0),&
    fix_node_list_y=femdomains(1)%select(x_max=0.0010d0),&
    fix_node_list_z=femdomains(1)%select(x_max=0.0010d0),&
    overset_algorithm=FEMDomain_Overset_GPP, &
    penalty = YoungModulus1*100000.0d0 &
)

call print(seis%frequency(1:10) )
call seis%exportModeShape(DomainID=1,femdomain=femdomains(1),amp=0.00020d0,name="bar_1_" )
call seis%exportModeShape(DomainID=2,femdomain=femdomains(2),amp=0.00020d0,name="bar_2_" )


end