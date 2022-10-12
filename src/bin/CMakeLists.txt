cmake_minimum_required(VERSION 2.8.10)
# CMakeのバージョン

enable_language(Fortran)
# Fortran向け設定の有効化

project(plantfem Fortran)

find_package(MPI REQUIRED)
add_definitions(${MPI_Fortran_COMPILE_FLAGS})
include_directories(${MPI_Fortran_INCLUDE_PATH})
link_directories(${MPI_Fortran_LIBRARIES})


#target_link_libraries(${exe_name} otherlibs ${MPI_Fortran_LIBRARIES})
# プロジェクト名と使用する言語
#find_package(MPI REQUIRED)
#add_definitions(${MPI_Fortran_COMPILE_FLAGS})
#include_directories(${MPI_Fortran_INCLUDE_PATH})
#link_directories(${MPI_Fortran_LIBRARIES})
#target_link_libraries(${exe_name} otherlibs ${MPI_Fortran_LIBRARIES})
#set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}")
#find_package(MPI REQUIRED COMPONENTS Fortran)
#target_link_libraries(mpi mpi.h)
#if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
#set(dialect "-ffree-form -std=f2008 -fimplicit-none")
#set(bounds "-fbounds-check")
#endif()
#if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
#    set(dialect "-stand f08 -free -implicitnone")
#    set(bounds "-check bounds")
#endif()
#if(CMAKE_Fortran_COMPILER_ID MATCHES "PGI")
#    set(dialect "-Mfreeform -Mdclchk -Mstandard -Mallocatable=03")
#    set(bounds "-C")
#endif()
#
#set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${bounds}")
#set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${dialect}")

#
# Compile.
#
#
#file(GLOB_RECURSE sources  src/*.f90 src/*.h)
#add_executable(prog ${sources})



#add_library(uuid_module ./src/uuid-fortran/uuid_module.f90)
#add_library(StringClass ./src/StringClass/StringClass.f90)
#add_library(TimeClass ./src/TimeClass/TimeClass.f90)
#add_library(TermClass ./src/TermClass/TermClass.f90)
#add_library(MathClass ./src/MathClass/MathClass.f90)
#add_library(PhysicsClass ./src/PhysicsClass/PhysicsClass.f90)
#add_library(HTTPClass ./src/HTTPClass/HTTPClass.f90)
#add_library(IOClass ./src/IOClass/IOClass.f90)
#add_library(HTMLClass ./src/HTMLClass/HTMLClass.f90)
#add_library(KinematicClass ./src/KinematicClass/KinematicClass.f90)
#add_library(RandomClass ./src/RandomClass/RandomClass.f90)
#add_library(ArrayClass ./src/ArrayClass/ArrayClass.f90)
#add_library(VertexClass ./src/VertexClass/VertexClass.f90)
#add_library(GraphClass ./src/GraphClass/GraphClass.f90)
#add_library(VectorClass ./src/VectorClass/VectorClass.f90)
#add_library(EquationClass ./src/EquationClass/EquationClass.f90)
#add_library(MPIClass ./src/MPIClass/MPIClass.f90)
#add_library(DictionaryClass ./src/DictionaryClass/DictionaryClass.f90)
#add_library(OpenMPClass ./src/OpenMPClass/OpenMPClass.f90)
#add_library(LinearSolverClass ./src/LinearSolverClass/LinearSolverClass.f90)
#add_library(GeometryClass ./src/GeometryClass/GeometryClass.f90)
#add_library(RouteOptimizationClass ./src/RouteOptimizationClass/RouteOptimizationClass.f90)
#add_library(TreeClass ./src/TreeClass/TreeClass.f90)
#add_library(CSVClass ./src/CSVClass/CSVClass.f90)
#add_library(ShapeFunctionClass ./src/ShapeFunctionClass/ShapeFunctionClass.f90)
#add_library(STLClass ./src/STLClass/STLClass.f90)
#add_library(WebServerClass ./src/WebserverClass/WebServerClass.f90)
#add_library(PhysicalFieldClass ./src/PhysicalFieldClass/PhysicalFieldClass.f90)
#add_library(std ./src/std/std.f90)
#add_library(MeshClass ./src/MeshClass/MeshClass.f90)
#add_library(MaterialPropClass ./src/MaterialPropClass/MaterialPropClass.f90)
#add_library(ControlParameterClass ./src/ControlParameterClass/ControlParameterClass.f90)
#add_library(BoundaryConditionClass ./src/BoundaryConditionClass/BoundaryConditionClass.f90)
#add_library(StrainClass ./src/StrainClass/StrainClass.f90)
#add_library(StressClass ./src/StressClass/StressClass.f90)
#add_library(ConstitutiveModelClass ./src/ConstitutiveModelClass/ConstitutiveModelClass.f90)
#add_library(FEMDomainClass ./src/FEMDomainClass/FEMDomainClass.f90)
#add_library(FEMIfaceClass ./src/FEMIfaceClass/FEMIfaceClass.f90)
#add_library(PostProcessingClass ./src/PostProcessingClass/PostProcessingClass.f90)
#add_library(PreProcessingClass ./src/PreProcessingClass/PreProcessingClass.f90)
#add_library(fem ./src/fem/fem.f90)
#add_library(DiffusionEquationClass ./src/DiffusionEquationClass/DiffusionEquationClass.f90)
#add_library(SpaceTimeDiffusionClass ./src/SpaceTimeDiffusionClass/SpaceTimeDiffusionClass.f90)
#add_library(SeismicAnalysisClass ./src/SeismicAnalysisClass/SeismicAnalysisClass.f90)
#add_library(FiniteDeformationClass ./src/FiniteDeformationClass/FiniteDeformationClass.f90)
#add_library(ContactMechanicsClass ./src/ContactMechanicsClass/ContactMechanicsClass.f90)
#add_library(MultiDiffDeformClass ./src/MultiDiffDeformClass/MultiDiffDeformClass.f90)
#add_library(MultiPhysicsClass ./src/MultiPhysicsClass/MultiPhysicsClass.f90)
#add_library(SpaceTimeDeformClass ./src/SpaceTimeDeformClass/SpaceTimeDeformClass.f90)
#add_library(PoromechanicsClass ./src/PoromechanicsClass/PoromechanicsClass.f90)
#add_library(SoilWaterCouplingClass ./src/SoilWaterCouplingClass/SoilWaterCouplingClass.f90)
#add_library(WaterAbsorptionClass ./src/WaterAbsorptionClass/WaterAbsorptionClass.f90)
#add_library(FieldClass ./src/FieldClass/FieldClass.f90)
#add_library(SimulatorClass ./src/SimulatorClass/SimulatorClass.f90)
#add_library(sim ./src/sim/sim.f90)
#add_library(DigitalElevationModelClass ./src/DigitalElevationModelClass/DigitalElevationModelClass.f90)
#add_library(BoringClass ./src/BoringClass/BoringClass.f90)
#add_library(RidgeClass ./src/RidgeClass/RidgeClass.f90)
#add_library(AirClass ./src/AirClass/AirClass.f90)
#add_library(DamClass ./src/DamClass/DamClass.f90)
#add_library(LightClass ./src/LightClass/LightClass.f90)
#add_library(FertilizerClass ./src/FertilizerClass/FertilizerClass.f90)
#add_library(StemClass ./src/StemClass/StemClass.f90)
#add_library(PlantRootClass ./src/PlantRootClass/PlantRootClass.f90)
#add_library(RootClass ./src/RootClass/RootClass.f90)
#add_library(PetiClass ./src/PetiClass/PetiClass.f90)
#add_library(PodClass ./src/PodClass/PodClass.f90)
#add_library(FlowerClass ./src/FlowerClass/FlowerClass.f90)
#add_library(PanicleClass ./src/PanicleClass/PanicleClass.f90)
#add_library(LeafClass ./src/LeafClass/LeafClass.f90)
#add_library(PlantNodeClass ./src/PlantNodeClass/PlantNodeClass.f90)
#add_library(InsectClass ./src/InsectClass/InsectClass.f90)
#add_library(LsystemClass ./src/LsystemClass/LsystemClass.f90)
#add_library(SeedClass ./src/SeedClass/SeedClass.f90)
#add_library(SoilClass ./src/SoilClass/SoilClass.f90)
#add_library(SoybeanClass ./src/SoybeanClass/SoybeanClass.f90)
#add_library(RiceClass ./src/RiceClass/RiceClass.f90)
#add_library(FarmClass ./src/FarmClass/FarmClass.f90)
#add_library(obj ./src/obj/obj.f90)
#add_library(PlantFEM ./src/PlantFEM/PlantFEM.f90)

#target_include_directories(server.f90
#  PUBLIC ${PROJECT_SOURCE_DIR}/inc
#)
set(EXECUTABLE a.out)

add_executable(${EXECUTABLE} 
./src/uuid-fortran/uuid_module.f90
./src/StringClass/StringClass.f90
./src/TimeClass/TimeClass.f90
./src/TermClass/TermClass.f90
./src/MathClass/MathClass.f90
./src/PhysicsClass/PhysicsClass.f90
./src/HTTPClass/HTTPClass.f90
./src/IOClass/IOClass.f90
./src/HTMLClass/HTMLClass.f90
./src/KinematicClass/KinematicClass.f90
./src/RandomClass/RandomClass.f90
./src/ArrayClass/ArrayClass.f90
./src/VertexClass/VertexClass.f90
./src/GraphClass/GraphClass.f90
./src/VectorClass/VectorClass.f90
./src/EquationClass/EquationClass.f90
./src/MPIClass/MPIClass.f90
./src/DictionaryClass/DictionaryClass.f90
./src/OpenMPClass/OpenMPClass.f90
./src/LinearSolverClass/LinearSolverClass.f90
./src/GeometryClass/GeometryClass.f90
./src/RouteOptimizationClass/RouteOptimizationClass.f90
./src/TreeClass/TreeClass.f90
./src/CSVClass/CSVClass.f90
./src/ShapeFunctionClass/ShapeFunctionClass.f90
./src/STLClass/STLClass.f90
./src/WebserverClass/WebServerClass.f90
./src/PhysicalFieldClass/PhysicalFieldClass.f90
./src/std/std.f90
./src/MeshClass/MeshClass.f90
./src/MaterialPropClass/MaterialPropClass.f90
./src/ControlParameterClass/ControlParameterClass.f90
./src/BoundaryConditionClass/BoundaryConditionClass.f90
./src/StrainClass/StrainClass.f90
./src/StressClass/StressClass.f90
./src/ConstitutiveModelClass/ConstitutiveModelClass.f90
./src/FEMDomainClass/FEMDomainClass.f90
./src/FEMIfaceClass/FEMIfaceClass.f90
./src/PostProcessingClass/PostProcessingClass.f90
./src/PreProcessingClass/PreProcessingClass.f90
./src/fem/fem.f90
./src/DiffusionEquationClass/DiffusionEquationClass.f90
./src/SpaceTimeDiffusionClass/SpaceTimeDiffusionClass.f90
./src/SeismicAnalysisClass/SeismicAnalysisClass.f90
./src/FiniteDeformationClass/FiniteDeformationClass.f90
./src/ContactMechanicsClass/ContactMechanicsClass.f90
./src/MultiDiffDeformClass/MultiDiffDeformClass.f90
./src/MultiPhysicsClass/MultiPhysicsClass.f90
./src/SpaceTimeDeformClass/SpaceTimeDeformClass.f90
./src/PoromechanicsClass/PoromechanicsClass.f90
./src/SoilWaterCouplingClass/SoilWaterCouplingClass.f90
./src/WaterAbsorptionClass/WaterAbsorptionClass.f90
./src/FieldClass/FieldClass.f90
./src/SimulatorClass/SimulatorClass.f90
./src/sim/sim.f90
./src/DigitalElevationModelClass/DigitalElevationModelClass.f90
./src/BoringClass/BoringClass.f90
./src/RidgeClass/RidgeClass.f90
./src/AirClass/AirClass.f90
./src/DamClass/DamClass.f90
./src/LightClass/LightClass.f90
./src/FertilizerClass/FertilizerClass.f90
./src/StemClass/StemClass.f90
./src/PlantRootClass/PlantRootClass.f90
./src/RootClass/RootClass.f90
./src/PetiClass/PetiClass.f90
./src/PodClass/PodClass.f90
./src/FlowerClass/FlowerClass.f90
./src/PanicleClass/PanicleClass.f90
./src/LeafClass/LeafClass.f90
./src/PlantNodeClass/PlantNodeClass.f90
./src/InsectClass/InsectClass.f90
./src/LsystemClass/LsystemClass.f90
./src/SeedClass/SeedClass.f90
./src/SoilClass/SoilClass.f90
./src/SoybeanClass/SoybeanClass.f90
./src/RiceClass/RiceClass.f90
./src/FarmClass/FarmClass.f90
./src/obj/obj.f90
./src/PlantFEM/PlantFEM.f90
server.f90
)