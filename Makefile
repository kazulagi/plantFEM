FC = mpif90
FFlags_0 = -Ofast -march=native -fopenmp
FFlags_1 = -shared -fopenmp -Ofast -fPIC -fno-var-tracking-assignments  -g -fcheck=all -fintrinsic-modules-path inc/ 
FFlags_2 = -Ofast -march=native -fopenacc

CFlags_0 = -fopenmp -O3  -fPIC 

LAPACK_BLAS = -llapack -lblas


./inc/PlantFEM.o: ./inc/sim.o ./inc/obj.o ./src/PlantFEM/PlantFEM.f90
	$(FC) $(FFlags_1)  -c ./src/PlantFEM/PlantFEM.f90 -o  ./inc/PlantFEM.o

libplantfem.so: ./inc/obj.o  
	$(FC) $(FFlags_1)  -o libplantfem.so ./inc/*.o 
 
./inc/obj.o: ./inc/sim.o ./inc/EarthClass.o ./inc/CivilItemClass.o ./inc/DigitalElevationModelClass.o ./inc/BoringClass.o ./inc/LightClass.o ./inc/FertilizerClass.o ./inc/RidgeClass.o ./inc/AirClass.o ./inc/DamClass.o ./inc/EnvironmentClass.o ./inc/LoggerClass.o ./inc/StemClass.o ./inc/PlantRootClass.o ./inc/RootClass.o ./inc/EarClass.o ./inc/PetiClass.o ./inc/PodClass.o ./inc/FlowerClass.o ./inc/PanicleClass.o ./inc/LeafClass.o ./inc/PlantNodeClass.o ./inc/InsectClass.o ./inc/LsystemClass.o ./inc/SeedClass.o ./inc/SoilClass.o ./inc/SoybeanClass.o ./inc/ArabidopsisClass.o ./inc/RiceClass.o ./inc/WheatClass.o ./inc/MaizeClass.o ./inc/GrapeClass.o ./inc/FarmClass.o ./inc/FactoryClass.o ./inc/SceneClass.o ./src/obj/obj.f90
	$(FC) $(FFlags_1)  -c ./src/obj/obj.f90 -o  ./inc/obj.o

./inc/sim.o:  ./inc/fem.o ./inc/WaveKernelClass.o ./inc/TSFEMClass.o ./inc/LoggerClass.o ./inc/ModalAnalysisClass.o ./inc/ElastoPlasticityClass.o ./inc/ReactorClass.o ./inc/SeepageFlowClass.o ./inc/DiffusionEquationClass.o ./inc/SpaceTimeDiffusionClass.o ./inc/SeismicAnalysisClass.o ./inc/MultiDOFSystemClass.o ./inc/FiniteDeformationClass.o ./inc/ContactMechanicsClass.o ./inc/ContactDiffusionClass.o ./inc/MultiDiffDeformClass.o ./inc/WaterAbsorptionClass.o ./inc/SoilWaterCouplingClass.o ./inc/MultiPhysicsClass.o ./inc/SpaceTimeDeformClass.o ./inc/PoromechanicsClass.o ./inc/FieldClass.o ./inc/SimulatorClass.o ./src/sim/sim.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/sim/sim.f90 -o  ./inc/sim.o

./inc/fem.o: ./inc/std.o ./inc/MeshClass.o ./inc/MaterialPropClass.o ./inc/ControlParameterClass.o ./inc/BoundaryConditionClass.o ./inc/StrainClass.o ./inc/StressClass.o ./inc/ConstitutiveModelClass.o ./inc/FEMDomainClass.o ./inc/FEMSolverClass.o ./inc/FEMIfaceClass.o ./inc/PostProcessingClass.o ./inc/PreProcessingClass.o ./src/fem/fem.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/fem/fem.f90 -o  ./inc/fem.o

./inc/std.o: ./inc/uuid_module.o ./inc/OpenACCClass.o ./inc/PhysicalConstantsClass.o ./inc/BitClass.o ./inc/StringClass.o ./inc/TimeClass.o ./inc/TermClass.o ./inc/MathClass.o ./inc/ConsoleClass.o ./inc/PhysicsClass.o ./inc/HTTPClass.o ./inc/IOClass.o ./inc/HTMLClass.o ./inc/KinematicClass.o ./inc/RandomClass.o ./inc/ArrayClass.o ./inc/AnalystClass.o ./inc/ElasticityClass.o ./inc/SpectreAnalysisClass.o ./inc/SparseClass.o ./inc/RangeClass.o ./inc/PCAClass.o ./inc/SDEClass.o ./inc/VertexClass.o ./inc/GraphClass.o ./inc/CSVClass.o ./inc/VectorClass.o ./inc/EquationClass.o ./inc/MPIClass.o ./inc/DictionaryClass.o ./inc/OpenMPClass.o ./inc/LTISystemClass.o ./inc/GAClass.o ./inc/LinearSolverClass.o ./inc/GeometryClass.o ./inc/TreeClass.o ./inc/ShapeFunctionClass.o ./inc/RouteOptimizationClass.o ./inc/STLClass.o ./inc/WebserverClass.o ./inc/PhysicalFieldClass.o ./inc/SPACClass.o ./src/std/std.f90
	$(FC) $(FFlags_1)  -c ./src/std/std.f90 -o  ./inc/std.o


./inc/PCAClass.o: ./inc/IOClass.o ./inc/MathClass.o ./inc/ArrayClass.o ./src/PCAClass/PCAClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/PCAClass/PCAClass.f90 -o ./inc/PCAClass.o $(LAPACK_BLAS)

./inc/SDEClass.o: ./inc/RandomClass.o ./inc/ArrayClass.o ./inc/MathClass.o ./src/SDEClass/SDEClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/SDEClass/SDEClass.f90 -o ./inc/SDEClass.o $(LAPACK_BLAS)

./inc/VertexClass.o: ./inc/ArrayClass.o ./inc/RandomClass.o ./src/VertexClass/VertexClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/VertexClass/VertexClass.f90  -o ./inc/VertexClass.o $(LAPACK_BLAS)

./inc/GraphClass.o: ./inc/VertexClass.o ./inc/IOClass.o ./inc/MathClass.o ./inc/RandomClass.o ./src/GraphClass/GraphClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/GraphClass/GraphClass.f90  -o ./inc/GraphClass.o $(LAPACK_BLAS)

./inc/VectorClass.o: ./inc/ArrayClass.o ./src/VectorClass/VectorClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/VectorClass/VectorClass.f90  -o ./inc/VectorClass.o $(LAPACK_BLAS)

./inc/EquationClass.o: ./inc/MathClass.o ./src/EquationClass/EquationClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/EquationClass/EquationClass.f90  -o ./inc/EquationClass.o $(LAPACK_BLAS)

./inc/MPIClass.o:  ./inc/MathClass.o ./inc/ArrayClass.o ./inc/GraphClass.o ./src/MPIClass/MPIClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/MPIClass/MPIClass.f90 -o ./inc/MPIClass.o $(LAPACK_BLAS)

./inc/DictionaryClass.o: ./inc/MathClass.o ./inc/IOClass.o ./src/DictionaryClass/DictionaryClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/DictionaryClass/DictionaryClass.f90 -o ./inc/DictionaryClass.o $(LAPACK_BLAS)

./inc/OpenMPClass.o: ./src/OpenMPClass/OpenMPClass.f90
	$(FC)  $(FFlags_1)  -c ./src/OpenMPClass/OpenMPClass.f90 -o ./inc/OpenMPClass.o

./inc/LTISystemClass.o: ./inc/ArrayClass.o ./inc/IOClass.o ./inc/MathClass.o ./inc/RandomClass.o ./src/LTISystemClass/LTISystemClass.f90
	$(FC)  $(FFlags_1)  -c ./src/LTISystemClass/LTISystemClass.f90 -o ./inc/LTISystemClass.o

./inc/GAClass.o: ./inc/RandomClass.o ./inc/ArrayClass.o ./inc/MathClass.o ./src/GAClass/GAClass.f90
	$(FC)  $(FFlags_1)  -c ./src/GAClass/GAClass.f90 -o ./inc/GAClass.o

./inc/LinearSolverClass.o: ./inc/IOClass.o ./inc/TimeClass.o ./inc/MathClass.o ./inc/ArrayClass.o ./inc/SparseClass.o ./inc/RandomClass.o ./src/LinearSolverClass/LinearSolverClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/LinearSolverClass/LinearSolverClass.f90  -o ./inc/LinearSolverClass.o $(LAPACK_BLAS)

./inc/GeometryClass.o: ./inc/MathClass.o ./inc/ArrayClass.o ./src/GeometryClass/GeometryClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/GeometryClass/GeometryClass.f90  -o ./inc/GeometryClass.o $(LAPACK_BLAS)

./inc/RouteOptimizationClass.o: ./inc/GeometryClass.o ./src/RouteOptimizationClass/RouteOptimizationClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/RouteOptimizationClass/RouteOptimizationClass.f90  -o ./inc/RouteOptimizationClass.o $(LAPACK_BLAS)

./inc/TreeClass.o: ./inc/ArrayClass.o ./src/TreeClass/TreeClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/TreeClass/TreeClass.f90  -o ./inc/TreeClass.o $(LAPACK_BLAS)

./inc/CSVClass.o: ./inc/ArrayClass.o ./inc/IOClass.o  ./src/CSVClass/CSVClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/CSVClass/CSVClass.f90  -o ./inc/CSVClass.o $(LAPACK_BLAS)

./inc/ShapeFunctionClass.o: ./inc/MathClass.o ./inc/ArrayClass.o ./inc/IOClass.o ./src/ShapeFunctionClass/ShapeFunctionClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/ShapeFunctionClass/ShapeFunctionClass.f90  -o ./inc/ShapeFunctionClass.o $(LAPACK_BLAS)

./inc/STLClass.o: ./inc/IOClass.o ./inc/RandomClass.o ./inc/ArrayClass.o ./src/STLClass/STLClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/STLClass/STLClass.f90  -o ./inc/STLClass.o $(LAPACK_BLAS)

./inc/WebserverClass.o: ./inc/TimeClass.o ./inc/TermClass.o ./inc/MathClass.o ./inc/PhysicsClass.o ./inc/IOClass.o ./inc/KinematicClass.o ./inc/RandomClass.o ./inc/ArrayClass.o ./inc/VertexClass.o ./inc/GraphClass.o ./inc/CSVClass.o ./inc/VectorClass.o ./inc/EquationClass.o ./inc/DictionaryClass.o ./inc/OpenMPClass.o ./inc/LinearSolverClass.o ./inc/GeometryClass.o ./inc/TreeClass.o ./inc/ShapeFunctionClass.o ./inc/RouteOptimizationClass.o ./inc/STLClass.o ./src/WebserverClass/WebServerClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/WebserverClass/WebServerClass.f90  -o ./inc/WebserverClass.o $(LAPACK_BLAS)


./inc/PhysicalFieldClass.o: ./inc/ArrayClass.o ./inc/IOClass.o ./inc/LinearSolverClass.o ./src/PhysicalFieldClass/PhysicalFieldClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/PhysicalFieldClass/PhysicalFieldClass.f90  -o ./inc/PhysicalFieldClass.o $(LAPACK_BLAS)


./inc/SPACClass.o: ./inc/MathClass.o ./inc/ArrayClass.o ./inc/IOClass.o ./inc/SpectreAnalysisClass.o ./src/SPACClass/SPACClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/SPACClass/SPACClass.f90  -o ./inc/SPACClass.o $(LAPACK_BLAS)

./inc/RandomClass.o: ./inc/MathClass.o ./src/RandomClass/RandomClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/RandomClass/RandomClass.f90   -o ./inc/RandomClass.o $(LAPACK_BLAS)

./inc/ArrayClass.o: ./inc/MathClass.o ./inc/RandomClass.o ./src/ArrayClass/ArrayClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/ArrayClass/ArrayClass.f90   -o ./inc/ArrayClass.o $(LAPACK_BLAS)

./inc/AnalystClass.o: ./inc/uuid_module.o ./inc/ListClass.o ./inc/MathClass.o ./inc/ArrayClass.o ./src/AnalystClass/AnalystClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/AnalystClass/AnalystClass.f90   -o ./inc/AnalystClass.o $(LAPACK_BLAS)

./inc/ElasticityClass.o: ./inc/ArrayClass.o ./src/ElasticityClass/ElasticityClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/ElasticityClass/ElasticityClass.f90   -o ./inc/ElasticityClass.o $(LAPACK_BLAS)

./inc/RangeClass.o: ./inc/IOClass.o ./inc/MathClass.o ./src/RangeClass/RangeClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/RangeClass/RangeClass.f90   -o ./inc/RangeClass.o $(LAPACK_BLAS)

./inc/SpectreAnalysisClass.o: ./inc/IOClass.o ./inc/MathClass.o ./inc/ArrayClass.o ./src/SpectreAnalysisClass/SpectreAnalysisClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/SpectreAnalysisClass/SpectreAnalysisClass.f90   -o ./inc/SpectreAnalysisClass.o $(LAPACK_BLAS)


./inc/SparseClass-c.o: ./src/SparseClass/SparseClass-c.c
	gcc -c  $(CFlags_0)./src/SparseClass/SparseClass-c.c -o ./inc/SparseClass-c.o

./inc/SparseClass.o: ./inc/ArrayClass.o ./inc/RandomClass.o ./inc/RangeClass.o ./inc/SparseClass-c.o ./src/SparseClass/SparseClass.f90  
	$(FC) -O3 -march=native $(FFlags_1)  -c  ./inc/SparseClass-c.o ./src/SparseClass/SparseClass.f90 -o ./inc/SparseClass.o $(LAPACK_BLAS)

./inc/KinematicClass.o: ./inc/MathClass.o ./src/KinematicClass/KinematicClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/KinematicClass/KinematicClass.f90  -o ./inc/KinematicClass.o  $(LAPACK_BLAS)

./inc/HTMLClass.o: ./inc/IOClass.o ./src/HTMLClass/HTMLClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/HTMLClass/HTMLClass.f90 -o ./inc/HTMLClass.o  $(LAPACK_BLAS)

./inc/IOClass.o: ./inc/uuid_module.o ./inc/MathClass.o ./inc/StringClass.o ./inc/ListClass.o ./src/IOClass/IOClass.f90 
	$(FC) -cpp $(FFlags_0)  $(FFlags_1)  -c ./src/IOClass/IOClass.f90  -o ./inc/IOClass.o  $(LAPACK_BLAS)


./inc/HTTPClass.o: ./inc/MathClass.o  ./src/HTTPClass/HTTPClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/HTTPClass/HTTPClass.f90 -o ./inc/HTTPClass.o  $(LAPACK_BLAS)


./inc/PhysicsClass.o: ./src/PhysicsClass/PhysicsClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/PhysicsClass/PhysicsClass.f90   -o ./inc/PhysicsClass.o  $(LAPACK_BLAS)


./inc/ConsoleClass.o: ./inc/MathClass.o ./src/ConsoleClass/ConsoleClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/ConsoleClass/ConsoleClass.f90  -o ./inc/ConsoleClass.o  $(LAPACK_BLAS)

./inc/MathClass.o:	./inc/StringClass.o ./src/MathClass/MathClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/MathClass/MathClass.f90  -o ./inc/MathClass.o  $(LAPACK_BLAS)



./inc/uuid_module.o	:	./src/uuid-fortran/uuid_module.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/uuid-fortran/uuid_module.f90 -o ./inc/uuid_module.o  $(LAPACK_BLAS)

./inc/ListClass.o	: ./src/ListClass/ListClass.f90
	$(FC) $(FFlags_2) $(FFlags_1)  -c ./src/ListClass/ListClass.f90   -o ./inc/ListClass.o  $(LAPACK_BLAS)

./inc/OpenACCClass.o:  ./src/OpenACCClass/OpenACCClass.f90
	$(FC) $(FFlags_2) $(FFlags_1)  -c ./src/OpenACCClass/OpenACCClass.f90 -o ./inc/OpenACCClass.o  $(LAPACK_BLAS)

./inc/BitClass.o:  ./src/BitClass/BitClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/BitClass/BitClass.f90 -o ./inc/BitClass.o  $(LAPACK_BLAS)

./inc/PhysicalConstantsClass.o:  ./src/PhysicalConstantsClass/PhysicalConstantsClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/PhysicalConstantsClass/PhysicalConstantsClass.f90 -o ./inc/PhysicalConstantsClass.o  $(LAPACK_BLAS)

./inc/StringClass.o:  ./src/StringClass/StringClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/StringClass/StringClass.f90 -o ./inc/StringClass.o  $(LAPACK_BLAS)


 ./inc/TimeClass.o:	./src/TimeClass/TimeClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/TimeClass/TimeClass.f90  -o ./inc/TimeClass.o  $(LAPACK_BLAS)

 ./inc/TermClass.o:	./src/TermClass/TermClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/TermClass/TermClass.f90  -o ./inc/TermClass.o  $(LAPACK_BLAS)




./inc/MeshClass.o: ./inc/std.o ./src/MeshClass/MeshClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/MeshClass/MeshClass.f90 -o  ./inc/MeshClass.o

./inc/MaterialPropClass.o: ./inc/MeshClass.o ./src/MaterialPropClass/MaterialPropClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/MaterialPropClass/MaterialPropClass.f90 -o  ./inc/MaterialPropClass.o

./inc/ControlParameterClass.o: ./inc/std.o ./src/ControlParameterClass/ControlParameterClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/ControlParameterClass/ControlParameterClass.f90 -o  ./inc/ControlParameterClass.o


./inc/BoundaryConditionClass.o: ./inc/std.o ./inc/MeshClass.o ./src/BoundaryConditionClass/BoundaryConditionClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/BoundaryConditionClass/BoundaryConditionClass.f90 -o  ./inc/BoundaryConditionClass.o

./inc/StrainClass.o: ./inc/std.o ./src/StrainClass/StrainClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/StrainClass/StrainClass.f90 -o  ./inc/StrainClass.o

./inc/StressClass.o: ./inc/StrainClass.o ./inc/MathClass.o ./src/StressClass/StressClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/StressClass/StressClass.f90 -o  ./inc/StressClass.o

./inc/ConstitutiveModelClass.o: ./inc/StressClass.o ./inc/MathClass.o ./src/ConstitutiveModelClass/ConstitutiveModelClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/ConstitutiveModelClass/ConstitutiveModelClass.f90 -o  ./inc/ConstitutiveModelClass.o

./inc/FEMDomainClass.o :  ./inc/MathClass.o ./inc/StressClass.o ./inc/MathClass.o ./inc/ArrayClass.o ./inc/ShapeFunctionClass.o ./inc/MeshClass.o ./inc/MaterialPropClass.o ./inc/BoundaryConditionClass.o ./inc/ControlParameterClass.o ./src/FEMDomainClass/FEMDomainClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/FEMDomainClass/FEMDomainClass.f90 -o  ./inc/FEMDomainClass.o 


./inc/FEMSolverClass.o : ./inc/SparseClass.o ./inc/FEMDomainClass.o ./src/FEMSolverClass/FEMSolverClass.f90
	$(FC) $(FFlags_0) -fbacktrace $(FFlags_1)  -c ./src/FEMSolverClass/FEMSolverClass.f90 -o  ./inc/FEMSolverClass.o 

./inc/FEMIfaceClass.o: ./inc/FEMDomainClass.o ./src/FEMIfaceClass/FEMIfaceClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/FEMIfaceClass/FEMIfaceClass.f90 -o  ./inc/FEMIfaceClass.o

./inc/PostProcessingClass.o: ./inc/FEMDomainClass.o ./src/PostProcessingClass/PostProcessingClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/PostProcessingClass/PostProcessingClass.f90 -o  ./inc/PostProcessingClass.o

./inc/PreProcessingClass.o: ./inc/FEMDomainClass.o ./inc/PostProcessingClass.o ./src/PreProcessingClass/PreProcessingClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/PreProcessingClass/PreProcessingClass.f90 -o  ./inc/PreProcessingClass.o



./inc/WaveKernelClass.o:  ./inc/fem.o ./src/WaveKernelClass/WaveKernelClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/WaveKernelClass/WaveKernelClass.f90 -o  ./inc/WaveKernelClass.o

./inc/TSFEMClass.o:  ./inc/fem.o ./inc/WaveKernelClass.o ./src/TSFEMClass/TSFEMClass.f90
	$(FC) $(FFlags_1)  -c ./src/TSFEMClass/TSFEMClass.f90 -o  ./inc/TSFEMClass.o

./inc/LoggerClass.o:  ./inc/fem.o ./src/LoggerClass/LoggerClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/LoggerClass/LoggerClass.f90 -o  ./inc/LoggerClass.o

./inc/ElastoPlasticityClass.o:  ./inc/fem.o ./src/ElastoPlasticityClass/ElastoPlasticityClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/ElastoPlasticityClass/ElastoPlasticityClass.f90 -o  ./inc/ElastoPlasticityClass.o $(LAPACK_BLAS)

./inc/ModalAnalysisClass.o:  ./inc/fem.o ./src/ModalAnalysisClass/ModalAnalysisClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/ModalAnalysisClass/ModalAnalysisClass.f90 -o  ./inc/ModalAnalysisClass.o

./inc/ReactorClass.o:  ./inc/fem.o ./src/ReactorClass/ReactorClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/ReactorClass/ReactorClass.f90 -o  ./inc/ReactorClass.o

./inc/SeepageFlowClass.o:  ./inc/fem.o ./src/SeepageFlowClass/SeepageFlowClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/SeepageFlowClass/SeepageFlowClass.f90 -o  ./inc/SeepageFlowClass.o

./inc/DiffusionEquationClass.o:  ./inc/fem.o ./src/DiffusionEquationClass/DiffusionEquationClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/DiffusionEquationClass/DiffusionEquationClass.f90 -o  ./inc/DiffusionEquationClass.o

./inc/SpaceTimeDiffusionClass.o:  ./inc/fem.o ./src/SpaceTimeDiffusionClass/SpaceTimeDiffusionClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/SpaceTimeDiffusionClass/SpaceTimeDiffusionClass.f90 -o  ./inc/SpaceTimeDiffusionClass.o

./inc/SeismicAnalysisClass.o:  ./inc/fem.o ./inc/ModalAnalysisClass.o ./src/SeismicAnalysisClass/SeismicAnalysisClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/SeismicAnalysisClass/SeismicAnalysisClass.f90 -o  ./inc/SeismicAnalysisClass.o

./inc/MultiDOFSystemClass.o:  ./inc/fem.o ./inc/SeismicAnalysisClass.o ./src/MultiDOFSystemClass/MultiDOFSystemClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/MultiDOFSystemClass/MultiDOFSystemClass.f90 -o  ./inc/MultiDOFSystemClass.o

./inc/FiniteDeformationClass.o:  ./inc/fem.o ./src/FiniteDeformationClass/FiniteDeformationClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/FiniteDeformationClass/FiniteDeformationClass.f90 -o  ./inc/FiniteDeformationClass.o

./inc/ContactMechanicsClass.o:  ./inc/fem.o ./inc/FiniteDeformationClass.o ./src/ContactMechanicsClass/ContactMechanicsClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/ContactMechanicsClass/ContactMechanicsClass.f90 -o  ./inc/ContactMechanicsClass.o

./inc/ContactDiffusionClass.o:  ./inc/fem.o ./inc/FiniteDeformationClass.o ./src/ContactDiffusionClass/ContactDiffusionClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/ContactDiffusionClass/ContactDiffusionClass.f90 -o  ./inc/ContactDiffusionClass.o

./inc/MultiDiffDeformClass.o:  ./inc/fem.o ./inc/DiffusionEquationClass.o ./inc/FiniteDeformationClass.o ./src/MultiDiffDeformClass/MultiDiffDeformClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/MultiDiffDeformClass/MultiDiffDeformClass.f90 -o  ./inc/MultiDiffDeformClass.o

./inc/MultiPhysicsClass.o:  ./inc/fem.o ./src/MultiPhysicsClass/MultiPhysicsClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/MultiPhysicsClass/MultiPhysicsClass.f90 -o  ./inc/MultiPhysicsClass.o

./inc/SpaceTimeDeformClass.o:  ./inc/fem.o ./inc/PostProcessingClass.o ./src/SpaceTimeDeformClass/SpaceTimeDeformClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/SpaceTimeDeformClass/SpaceTimeDeformClass.f90 -o  ./inc/SpaceTimeDeformClass.o

./inc/PoromechanicsClass.o:  ./inc/fem.o ./src/PoromechanicsClass/PoromechanicsClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/PoromechanicsClass/PoromechanicsClass.f90 -o  ./inc/PoromechanicsClass.o

./inc/SoilWaterCouplingClass.o:  ./inc/fem.o ./src/SoilWaterCouplingClass/SoilWaterCouplingClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/SoilWaterCouplingClass/SoilWaterCouplingClass.f90 -o  ./inc/SoilWaterCouplingClass.o

./inc/WaterAbsorptionClass.o:  ./inc/fem.o ./inc/DiffusionEquationClass.o ./inc/FiniteDeformationClass.o ./src/WaterAbsorptionClass/WaterAbsorptionClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/WaterAbsorptionClass/WaterAbsorptionClass.f90 -o  ./inc/WaterAbsorptionClass.o

./inc/FieldClass.o:  ./inc/fem.o  ./inc/DiffusionEquationClass.o ./inc/FiniteDeformationClass.o ./src/FieldClass/FieldClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/FieldClass/FieldClass.f90 -o  ./inc/FieldClass.o

./inc/SimulatorClass.o:  ./inc/fem.o ./inc/DiffusionEquationClass.o ./inc/FiniteDeformationClass.o ./inc/MultiPhysicsClass.o ./inc/PostProcessingClass.o ./inc/ContactMechanicsClass.o ./inc/FieldClass.o ./src/SimulatorClass/SimulatorClass.f90
	$(FC) $(FFlags_0) $(FFlags_1)  -c ./src/SimulatorClass/SimulatorClass.f90 -o  ./inc/SimulatorClass.o

./inc/EarthClass.o: ./inc/sim.o ./src/EarthClass/EarthClass.f90
	$(FC) $(FFlags_1)  -c ./src/EarthClass/EarthClass.f90 -o  ./inc/EarthClass.o

./inc/CivilItemClass.o: ./inc/sim.o ./src/CivilItemClass/CivilItemClass.f90
	$(FC) $(FFlags_1)  -c ./src/CivilItemClass/CivilItemClass.f90 -o  ./inc/CivilItemClass.o

./inc/DigitalElevationModelClass.o: ./inc/sim.o ./src/DigitalElevationModelClass/DigitalElevationModelClass.f90
	$(FC) $(FFlags_1)  -c ./src/DigitalElevationModelClass/DigitalElevationModelClass.f90 -o  ./inc/DigitalElevationModelClass.o

./inc/BoringClass.o: ./inc/sim.o ./src/BoringClass/BoringClass.f90
	$(FC) $(FFlags_1)  -c ./src/BoringClass/BoringClass.f90 -o  ./inc/BoringClass.o

./inc/RidgeClass.o: ./inc/sim.o ./src/RidgeClass/RidgeClass.f90
	$(FC) $(FFlags_1)  -c ./src/RidgeClass/RidgeClass.f90 -o  ./inc/RidgeClass.o

./inc/AirClass.o: ./inc/sim.o ./src/AirClass/AirClass.f90
	$(FC) $(FFlags_1)  -c ./src/AirClass/AirClass.f90 -o  ./inc/AirClass.o

./inc/DamClass.o: ./inc/sim.o ./src/DamClass/DamClass.f90
	$(FC) $(FFlags_1)  -c ./src/DamClass/DamClass.f90 -o  ./inc/DamClass.o

./inc/LightClass.o: ./inc/sim.o ./inc/EarthClass.o ./src/LightClass/LightClass.f90
	$(FC) $(FFlags_1)  -c ./src/LightClass/LightClass.f90 -o  ./inc/LightClass.o

./inc/EnvironmentClass.o: ./inc/sim.o ./inc/LightClass.o ./inc/AirClass.o ./src/EnvironmentClass/EnvironmentClass.f90
	$(FC) $(FFlags_1)  -c ./src/EnvironmentClass/EnvironmentClass.f90 -o  ./inc/EnvironmentClass.o

./inc/FertilizerClass.o: ./inc/sim.o ./src/FertilizerClass/FertilizerClass.f90
	$(FC) $(FFlags_1)  -c ./src/FertilizerClass/FertilizerClass.f90 -o  ./inc/FertilizerClass.o

./inc/StemClass.o: ./inc/sim.o ./src/StemClass/StemClass.f90
	$(FC) $(FFlags_1)  -c ./src/StemClass/StemClass.f90 -o  ./inc/StemClass.o

./inc/PlantRootClass.o: ./inc/sim.o ./inc/StemClass.o ./src/PlantRootClass/PlantRootClass.f90
	$(FC) $(FFlags_1)  -c ./src/PlantRootClass/PlantRootClass.f90 -o  ./inc/PlantRootClass.o

./inc/RootClass.o: ./inc/sim.o ./inc/StemClass.o ./src/RootClass/RootClass.f90
	$(FC) $(FFlags_1)  -c ./src/RootClass/RootClass.f90 -o  ./inc/RootClass.o

./inc/EarClass.o: ./inc/sim.o ./inc/StemClass.o ./src/EarClass/EarClass.f90
	$(FC) $(FFlags_1)  -c ./src/EarClass/EarClass.f90 -o  ./inc/EarClass.o

./inc/PetiClass.o: ./inc/sim.o  ./inc/StemClass.o ./src/PetiClass/PetiClass.f90
	$(FC) $(FFlags_1)  -c ./src/PetiClass/PetiClass.f90 -o  ./inc/PetiClass.o

./inc/PodClass.o: ./inc/sim.o  ./inc/StemClass.o  ./src/PodClass/PodClass.f90
	$(FC) $(FFlags_1)  -c ./src/PodClass/PodClass.f90 -o  ./inc/PodClass.o

./inc/FlowerClass.o: ./inc/sim.o  ./inc/StemClass.o  ./src/FlowerClass/FlowerClass.f90
	$(FC) $(FFlags_1)  -c ./src/FlowerClass/FlowerClass.f90 -o  ./inc/FlowerClass.o

./inc/PanicleClass.o: ./inc/sim.o ./inc/StemClass.o ./src/PanicleClass/PanicleClass.f90
	$(FC) $(FFlags_1)  -c ./src/PanicleClass/PanicleClass.f90 -o  ./inc/PanicleClass.o
	
./inc/LeafClass.o: ./inc/sim.o ./inc/StemClass.o  ./inc/PetiClass.o ./inc/LightClass.o ./inc/AirClass.o ./src/LeafClass/LeafClass.f90
	$(FC) $(FFlags_1)  -c ./src/LeafClass/LeafClass.f90 -o  ./inc/LeafClass.o

./inc/PlantNodeClass.o: ./inc/sim.o ./inc/StemClass.o ./inc/LeafClass.o ./inc/PetiClass.o ./inc/PodClass.o ./inc/FlowerClass.o ./inc/PlantRootClass.o  ./src/PlantNodeClass/PlantNodeClass.f90
	$(FC) $(FFlags_1)  -c ./src/PlantNodeClass/PlantNodeClass.f90 -o  ./inc/PlantNodeClass.o

./inc/InsectClass.o: ./inc/sim.o ./inc/LeafClass.o ./src/InsectClass/InsectClass.f90
	$(FC) $(FFlags_1)  -c ./src/InsectClass/InsectClass.f90 -o  ./inc/InsectClass.o
 
./inc/LsystemClass.o: ./inc/sim.o ./inc/StemClass.o ./inc/LeafClass.o ./inc/PetiClass.o ./inc/PodClass.o ./inc/FlowerClass.o ./inc/PlantRootClass.o ./inc/PlantNodeClass.o ./src/LsystemClass/LsystemClass.f90
	$(FC) $(FFlags_1)  -c ./src/LsystemClass/LsystemClass.f90 -o  ./inc/LsystemClass.o

./inc/SeedClass.o: ./inc/sim.o ./inc/LsystemClass.o ./src/SeedClass/SeedClass.f90
	$(FC) $(FFlags_1)  -c ./src/SeedClass/SeedClass.f90 -o  ./inc/SeedClass.o
 
./inc/SoilClass.o: ./inc/sim.o ./inc/FertilizerClass.o ./inc/BoringClass.o ./inc/DigitalElevationModelClass.o ./inc/EarthClass.o ./src/SoilClass/SoilClass.f90
	$(FC) $(FFlags_1)  -c ./src/SoilClass/SoilClass.f90 -o  ./inc/SoilClass.o

./inc/SoybeanClass.o: ./inc/sim.o ./inc/SeedClass.o ./inc/LeafClass.o ./inc/RootClass.o ./inc/LightClass.o ./inc/PlantNodeClass.o ./inc/StemClass.o ./inc/EnvironmentClass.o ./src/SoybeanClass/SoybeanClass.f90
	$(FC) $(FFlags_1)  -c ./src/SoybeanClass/SoybeanClass.f90 -o  ./inc/SoybeanClass.o
 
./inc/MaizeClass.o: ./inc/sim.o ./inc/LeafClass.o ./inc/StemClass.o ./inc/RootClass.o ./inc/EarClass.o ./inc/PanicleClass.o ./src/MaizeClass/MaizeClass.f90
	$(FC) $(FFlags_1)  -c ./src/MaizeClass/MaizeClass.f90 -o  ./inc/MaizeClass.o
 
./inc/GrapeClass.o: ./inc/sim.o ./inc/LeafClass.o ./inc/StemClass.o ./inc/RootClass.o ./src/GrapeClass/GrapeClass.f90
	$(FC) $(FFlags_1)  -c ./src/GrapeClass/GrapeClass.f90 -o  ./inc/GrapeClass.o

./inc/RiceClass.o: ./inc/sim.o ./inc/LeafClass.o ./inc/StemClass.o ./inc/RootClass.o ./inc/PanicleClass.o ./src/RiceClass/RiceClass.f90
	$(FC) $(FFlags_1)  -c ./src/RiceClass/RiceClass.f90 -o  ./inc/RiceClass.o

 ./inc/WheatClass.o: ./inc/sim.o ./inc/LeafClass.o ./inc/StemClass.o ./inc/RootClass.o ./inc/PanicleClass.o  ./src/WheatClass/WheatClass.f90
	$(FC) $(FFlags_1)  -c ./src/WheatClass/WheatClass.f90 -o  ./inc/WheatClass.o

 ./inc/ArabidopsisClass.o: ./inc/sim.o ./inc/LeafClass.o ./inc/StemClass.o ./inc/RootClass.o ./src/ArabidopsisClass/ArabidopsisClass.f90
	$(FC) $(FFlags_1)  -c ./src/ArabidopsisClass/ArabidopsisClass.f90 -o  ./inc/ArabidopsisClass.o

 ./inc/FarmClass.o: ./inc/sim.o ./inc/SoilClass.o ./inc/SoybeanClass.o ./src/FarmClass/FarmClass.f90
	$(FC) $(FFlags_1)  -c ./src/FarmClass/FarmClass.f90 -o  ./inc/FarmClass.o

 ./inc/FactoryClass.o: ./inc/sim.o ./inc/SoybeanClass.o ./src/FactoryClass/FactoryClass.f90
	$(FC) $(FFlags_1) -c ./src/FactoryClass/FactoryClass.f90 -o  ./inc/FactoryClass.o

 ./inc/SceneClass.o: ./inc/sim.o ./inc/SoybeanClass.o ./src/SceneClass/SceneClass.f90
	$(FC) $(FFlags_1)  -c ./src/SceneClass/SceneClass.f90 -o  ./inc/SceneClass.o
