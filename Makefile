FC = mpif90
LD = mpif90

all:server.o
	mpif90 server.o 

install:
	mv *.mod inc/

server.o:PlantFEM.o
	mpif90 inc/*.o server.f90

uuid_module.o:
	mpif90 -Ofast -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/uuid-fortran/uuid_module.f90 -o  inc/uuid_module.o

StringClass.o:
	mpif90 -Ofast -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/StringClass/StringClass.f90 -o  inc/StringClass.o

TimeClass.o:
	mpif90 -Ofast -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/TimeClass/TimeClass.f90 -o  inc/TimeClass.o

TermClass.o:
	mpif90 -Ofast -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/TermClass/TermClass.f90 -o  inc/TermClass.o

MathClass.o:StringClass.o
	mpif90 -Ofast -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/MathClass/MathClass.f90 -o  inc/MathClass.o

PhysicsClass.o:
	mpif90 -Ofast -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/PhysicsClass/PhysicsClass.f90 -o  inc/PhysicsClass.o

HTTPClass.o:MathClass.o
	mpif90 -Ofast -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/HTTPClass/HTTPClass.f90 -o  inc/HTTPClass.o

IOClass.o: MathClass.o StringClass.o
	mpif90 -Ofast -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/IOClass/IOClass.f90 -o  inc/IOClass.o

HTMLClass.o:IOClass.o
	mpif90 -Ofast -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/HTMLClass/HTMLClass.f90 -o  inc/HTMLClass.o

KinematicClass.o:MathClass.o
	mpif90 -Ofast -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/KinematicClass/KinematicClass.f90 -o  inc/KinematicClass.o

RandomClass.o:MathClass.o
	mpif90 -Ofast -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/RandomClass/RandomClass.f90 -o  inc/RandomClass.o

ArrayClass.o: MathClass.o RandomClass.o
	mpif90 -Ofast -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/ArrayClass/ArrayClass.f90 -o  inc/ArrayClass.o

VertexClass.o: ArrayClass.o RandomClass.o
	mpif90 -Ofast -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/VertexClass/VertexClass.f90 -o  inc/VertexClass.o

GraphClass.o: VertexClass.o IOClass.o MathClass.o RandomClass.o
	mpif90 -Ofast -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/GraphClass/GraphClass.f90 -o  inc/GraphClass.o

VectorClass.o:ArrayClass.o
	mpif90 -Ofast -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/VectorClass/VectorClass.f90 -o  inc/VectorClass.o

EquationClass.o:MathClass.o
	mpif90 -Ofast -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/EquationClass/EquationClass.f90 -o  inc/EquationClass.o

MPIClass.o: MathClass.o ArrayClass.o GraphClass.o
	mpif90 -Ofast -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/MPIClass/MPIClass.f90 -o  inc/MPIClass.o

DictionaryClass.o: MathClass.o
	mpif90 -Ofast -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/DictionaryClass/DictionaryClass.f90 -o  inc/DictionaryClass.o

OpenMPClass.o:
	mpif90 -Ofast -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/OpenMPClass/OpenMPClass.f90 -o  inc/OpenMPClass.o

LinearSolverClass.o: TimeClass.o MathClass.o MPIClass.o
	mpif90 -Ofast -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/LinearSolverClass/LinearSolverClass.f90 -o  inc/LinearSolverClass.o

GeometryClass.o: MathClass.o ArrayClass.o
	mpif90 -Ofast -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/GeometryClass/GeometryClass.f90 -o  inc/GeometryClass.o

RouteOptimizationClass.o: GeometryClass.o
	mpif90 -Ofast -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/RouteOptimizationClass/RouteOptimizationClass.f90 -o  inc/RouteOptimizationClass.o

TreeClass.o: ArrayClass.o
	mpif90 -Ofast -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/TreeClass/TreeClass.f90 -o  inc/TreeClass.o

CSVClass.o: IOClass.o ArrayClass.o
	mpif90 -Ofast -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/CSVClass/CSVClass.f90 -o  inc/CSVClass.o

ShapeFunctionClass.o: MathClass.o ArrayClass.o IOClass.o
	mpif90 -Ofast -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/ShapeFunctionClass/ShapeFunctionClass.f90 -o  inc/ShapeFunctionClass.o

STLClass.o: IOClass.o RandomClass.o ArrayClass.o
	mpif90 -Ofast -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/STLClass/STLClass.f90 -o  inc/STLClass.o

WebServerClass.o: TimeClass.o TermClass.o MathClass.o PhysicsClass.o IOClass.o KinematicClass.o RandomClass.o ArrayClass.o VertexClass.o GraphClass.o CSVClass.o VectorClass.o EquationClass.o MPIClass.o DictionaryClass.o OpenMPClass.o LinearSolverClass.o GeometryClass.o TreeClass.o ShapeFunctionClass.o RouteOptimizationClass.o STLClass.o
	mpif90 -Ofast -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/WebserverClass/WebServerClass.f90 -o  inc/WebServerClass.o

PhysicalFieldClass.o: ArrayClass.o IOClass.o LinearSolverClass.o
	mpif90 -Ofast -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/PhysicalFieldClass/PhysicalFieldClass.f90 -o  inc/PhysicalFieldClass.o

std.o: uuid_module.o StringClass.o TimeClass.o TermClass.o MathClass.o PhysicsClass.o HTTPClass.o IOClass.o HTMLClass.o KinematicClass.o RandomClass.o ArrayClass.o VertexClass.o GraphClass.o CSVClass.o VectorClass.o EquationClass.o MPIClass.o DictionaryClass.o OpenMPClass.o LinearSolverClass.o GeometryClass.o TreeClass.o ShapeFunctionClass.o RouteOptimizationClass.o STLClass.o WebServerClass.o PhysicalFieldClass.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/std/std.f90 -o  inc/std.o

MeshClass.o:std.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/MeshClass/MeshClass.f90 -o  inc/MeshClass.o

MaterialPropClass.o:MeshClass.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/MaterialPropClass/MaterialPropClass.f90 -o  inc/MaterialPropClass.o

ControlParameterClass.o:std.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/ControlParameterClass/ControlParameterClass.f90 -o  inc/ControlParameterClass.o

BoundaryConditionClass.o: std.o ArrayClass.o MeshClass.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/BoundaryConditionClass/BoundaryConditionClass.f90 -o  inc/BoundaryConditionClass.o

StrainClass.o: MathClass.o ShapeFunctionClass.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/StrainClass/StrainClass.f90 -o  inc/StrainClass.o

StressClass.o: MathClass.o StrainClass.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/StressClass/StressClass.f90 -o  inc/StressClass.o

ConstitutiveModelClass.o: MathClass.o StressClass.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/ConstitutiveModelClass/ConstitutiveModelClass.f90 -o  inc/ConstitutiveModelClass.o

FEMDomainClass.o : MathClass.o ArrayClass.o ShapeFunctionClass.o MeshClass.o MaterialPropClass.o BoundaryConditionClass.o ControlParameterClass.o std.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/FEMDomainClass/FEMDomainClass.f90 -o  inc/FEMDomainClass.o 

FEMIfaceClass.o: MathClass.o MPIClass.o ArrayClass.o ShapeFunctionClass.o MeshClass.o MaterialPropClass.o BoundaryConditionClass.o ControlParameterClass.o FEMDomainClass.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/FEMIfaceClass/FEMIfaceClass.f90 -o  inc/FEMIfaceClass.o

PostProcessingClass.o:FEMDomainClass.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/PostProcessingClass/PostProcessingClass.f90 -o  inc/PostProcessingClass.o

PreProcessingClass.o: std.o TermClass.o DictionaryClass.o MPIClass.o FEMDomainClass.o ArrayClass.o PostProcessingClass.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/PreProcessingClass/PreProcessingClass.f90 -o  inc/PreProcessingClass.o

fem.o: std.o MeshClass.o MaterialPropClass.o ControlParameterClass.o BoundaryConditionClass.o StrainClass.o StressClass.o ConstitutiveModelClass.o FEMDomainClass.o FEMIfaceClass.o PostProcessingClass.o PreProcessingClass.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/fem/fem.f90 -o  inc/fem.o

DiffusionEquationClass.o: FEMDomainClass.o PostProcessingClass.o LinearSolverClass.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/DiffusionEquationClass/DiffusionEquationClass.f90 -o  inc/DiffusionEquationClass.o

SpaceTimeDiffusionClass.o:fem.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/SpaceTimeDiffusionClass/SpaceTimeDiffusionClass.f90 -o  inc/SpaceTimeDiffusionClass.o

SeismicAnalysisClass.o:fem.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/SeismicAnalysisClass/SeismicAnalysisClass.f90 -o  inc/SeismicAnalysisClass.o

FiniteDeformationClass.o: MathClass.o LinearSolverClass.o FEMDomainClass.o PostProcessingClass.o ConstitutiveModelClass.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/FiniteDeformationClass/FiniteDeformationClass.f90 -o  inc/FiniteDeformationClass.o

ContactMechanicsClass.o: MathClass.o MPIClass.o FEMIfaceClass.o FEMDomainClass.o FiniteDeformationClass.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/ContactMechanicsClass/ContactMechanicsClass.f90 -o  inc/ContactMechanicsClass.o

MultiDiffDeformClass.o: DiffusionEquationClass.o FiniteDeformationClass.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/MultiDiffDeformClass/MultiDiffDeformClass.f90 -o  inc/MultiDiffDeformClass.o

MultiPhysicsClass.o: FEMIfaceClass.o FEMDomainClass.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/MultiPhysicsClass/MultiPhysicsClass.f90 -o  inc/MultiPhysicsClass.o

SpaceTimeDeformClass.o: MathClass.o LinearSolverClass.o FEMDomainClass.o PostProcessingClass.o ConstitutiveModelClass.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/SpaceTimeDeformClass/SpaceTimeDeformClass.f90 -o  inc/SpaceTimeDeformClass.o

PoromechanicsClass.o:fem.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/PoromechanicsClass/PoromechanicsClass.f90 -o  inc/PoromechanicsClass.o

SoilWaterCouplingClass.o:fem.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/SoilWaterCouplingClass/SoilWaterCouplingClass.f90 -o  inc/SoilWaterCouplingClass.o

WaterAbsorptionClass.o: fem.o DiffusionEquationClass.o FiniteDeformationClass.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/WaterAbsorptionClass/WaterAbsorptionClass.f90 -o  inc/WaterAbsorptionClass.o

FieldClass.o: MPIClass.o FEMDomainClass.o FEMIfaceClass.o DictionaryClass.o DiffusionEquationClass.o FiniteDeformationClass.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/FieldClass/FieldClass.f90 -o  inc/FieldClass.o

SimulatorClass.o: MPIClass.o TermClass.o FEMDomainClass.o DiffusionEquationClass.o FiniteDeformationClass.o MultiPhysicsClass.o PostProcessingClass.o ContactMechanicsClass.o FieldClass.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/SimulatorClass/SimulatorClass.f90 -o  inc/SimulatorClass.o

SiCroFClass.o:SimulatorClass.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/SiCroFClass/SiCroFClass.f90 -o  inc/SiCroFClass.o

sim.o: fem.o DiffusionEquationClass.o SpaceTimeDiffusionClass.o SeismicAnalysisClass.o FiniteDeformationClass.o ContactMechanicsClass.o MultiDiffDeformClass.o WaterAbsorptionClass.o SoilWaterCouplingClass.o MultiPhysicsClass.o SpaceTimeDeformClass.o PoromechanicsClass.o FieldClass.o SimulatorClass.o SiCroFClass.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/sim/sim.f90 -o  inc/sim.o

DigitalElevationModelClass.o:std.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/DigitalElevationModelClass/DigitalElevationModelClass.f90 -o  inc/DigitalElevationModelClass.o

BoringClass.o:std.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/BoringClass/BoringClass.f90 -o  inc/BoringClass.o

RidgeClass.o:sim.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/RidgeClass/RidgeClass.f90 -o  inc/RidgeClass.o

AirClass.o:fem.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/AirClass/AirClass.f90 -o  inc/AirClass.o

LightClass.o:fem.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/LightClass/LightClass.f90 -o  inc/LightClass.o

DamClass.o:sim.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/DamClass/DamClass.f90 -o  inc/DamClass.o


FertilizerClass.o:fem.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/FertilizerClass/FertilizerClass.f90 -o  inc/FertilizerClass.o

StemClass.o: KinematicClass.o FEMDomainClass.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/StemClass/StemClass.f90 -o  inc/StemClass.o

PlantRootClass.o: KinematicClass.o FEMDomainClass.o StemClass.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/PlantRootClass/PlantRootClass.f90 -o  inc/PlantRootClass.o

RootClass.o: KinematicClass.o FEMDomainClass.o StemClass.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/RootClass/RootClass.f90 -o  inc/RootClass.o

PetiClass.o: KinematicClass.o StemClass.o FEMDomainClass.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/PetiClass/PetiClass.f90 -o  inc/PetiClass.o

PodClass.o: KinematicClass.o StemClass.o FEMDomainClass.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/PodClass/PodClass.f90 -o  inc/PodClass.o

FlowerClass.o: KinematicClass.o StemClass.o FEMDomainClass.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/FlowerClass/FlowerClass.f90 -o  inc/FlowerClass.o

PanicleClass.o: KinematicClass.o FEMDomainClass.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/PanicleClass/PanicleClass.f90 -o  inc/PanicleClass.o

LeafClass.o: KinematicClass.o FEMDomainClass.o PetiClass.o StemClass.o LightClass.o AirClass.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/LeafClass/LeafClass.f90 -o  inc/LeafClass.o

PlantNodeClass.o: KinematicClass.o FEMDomainClass.o StemClass.o LeafClass.o PetiClass.o PodClass.o FlowerClass.o PlantRootClass.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/PlantNodeClass/PlantNodeClass.f90 -o  inc/PlantNodeClass.o

InsectClass.o: fem.o LeafClass.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/InsectClass/InsectClass.f90 -o  inc/InsectClass.o

LsystemClass.o: KinematicClass.o PreProcessingClass.o FEMDomainClass.o StemClass.o LeafClass.o PetiClass.o PodClass.o FlowerClass.o PlantRootClass.o PlantNodeClass.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/LsystemClass/LsystemClass.f90 -o  inc/LsystemClass.o
SeedClass.o: MathClass.o RandomClass.o LsystemClass.o FEMDomainClass.o PreProcessingClass.o sim.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/SeedClass/SeedClass.f90 -o  inc/SeedClass.o

SoilClass.o: fem.o FertilizerClass.o BoringClass.o DigitalElevationModelClass.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/SoilClass/SoilClass.f90 -o  inc/SoilClass.o

SoybeanClass.o: MathClass.o SeedClass.o LeafClass.o RootClass.o SoilClass.o LightClass.o PlantNodeClass.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/SoybeanClass/SoybeanClass.f90 -o  inc/SoybeanClass.o

RiceClass.o: MathClass.o SeedClass.o LeafClass.o RootClass.o SoilClass.o LightClass.o PlantNodeClass.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/RiceClass/RiceClass.f90 -o  inc/RiceClass.o

FarmClass.o:SoilClass.o SoybeanClass.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/FarmClass/FarmClass.f90 -o  inc/FarmClass.o

obj.o:RidgeClass.o AirClass.o DamClass.o StemClass.o PlantRootClass.o RootClass.o PetiClass.o PodClass.o FlowerClass.o PanicleClass.o LeafClass.o PlantNodeClass.o InsectClass.o LsystemClass.o SeedClass.o SoilClass.o SoybeanClass.o FarmClass.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/obj/obj.f90 -o  inc/obj.o

PlantFEM.o: obj.o
	mpif90 -fopenmp -fopenacc -shared -fPIC -g -fcheck=all -fintrinsic-modules-path inc/  -c src/PlantFEM/PlantFEM.f90 -o  inc/PlantFEM.o

