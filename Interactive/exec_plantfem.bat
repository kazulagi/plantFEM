@echo off 

echo ">"
echo "plantFEM >> Fortran Interactive mode for Linux & OS X... "
echo "Here, fortran script runs with  plantFEM-kernel."
echo "You can debug your code with plantFEM-API"
echo "  "
echo ">"
echo ">"
echo "Initializing ..."
set numcore=1

echo "Please input fortran script *.f90 | or check manual by 'man'  "
echo "  "

:LOOP
    set /p answer= ">>> " 


    if %answer%==bye ( 
        
        goto :END
    )

    if %answer%==man ( 
        type ".man_plantFEM"
        
        goto :LOOP
    ) 
    


    if %answer% == "exit" (
        exit 0
    )

    if %answer% == man_plantFEM (
        type ".man_plantFEM"
        goto :LOOP
    )

    if %answer% == install (
        install.bat
        goto :LOOP
    )
    if %answer% ==compress (
        ./bin/compress
        goto :LOOP
    )
    if %answer% ==run (
        ./bin/run
        goto :LOOP
    )
    if %answer% == ls (
        echo " "
        pwd
        echo " "
        ls
        echo " "
        goto :LOOP
    )
    if %answer% == cpucore (
        echo "Current num of cpu-core is :: $numcore "
        echo "Please input num of cpu-core"
        set /P numcore = " >>> "
        echo "Current num of cpu-core is :: $numcore "
        goto :LOOP
    )

    call :get_filename %answer%
    :get_filename
    echo %~x1

    set EXTENSION=%~x1
    echo "extension is %EXTENSION%"
    if %EXTENSION% ==.f90 (
        
        gfortran -o a.exe  .\inc\ArrayOperationClass.o  .\inc\BoundaryConditionClass.o  .\inc\ConstitutiveModelClass.o  .\inc\ContactMechanicsClass.o  .\inc\ControlParameterClass.o  .\inc\DictionaryClass.o  .\inc\DiffusionEquationClass.o  .\inc\FEMDomainClass.o  .\inc\FEMIfaceClass.o  .\inc\FieldClass.o  .\inc\FiniteDeformationClass.o  .\inc\LinearSolverClass.o  .\inc\MPIClass.o  .\inc\MaterialPropClass.o  .\inc\MathClass.o  .\inc\MeshOperationClass.o  .\inc\MultiDiffDeformClass.o  .\inc\MultiPhysicsClass.o  .\inc\OpenMPClass.o  .\inc\PostProcessingClass.o  .\inc\PreProcessingClass.o  .\inc\ShapeFunctionClass.o  .\inc\plantFEMClass.o  .\inc\SimulatorClass.o  .\inc\SpaceTimeDeformClass.o  .\inc\TermClass.o  .\inc\TreeClass.o  %answer%
        a.exe        
        goto :LOOP
    )

goto :LOOP


:END

echo See you again!