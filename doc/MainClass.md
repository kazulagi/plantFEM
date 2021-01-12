## Class Name :: main

[README](README.md)>>[main](Document/main.md)

### Instruction:
This is an example of Main Script.

### Requirement
- SimulatorClass

### Example
```fortran
program main
    use SimulatorClass

    implicit none
    
    type(MPI_)              :: MPIData
    type(Field_),target     :: world
    integer                 :: TotalStep=40
    real(8)                 :: time
    
    call StartMPI(MPIData)
    print *, "Time duration (sec.): "
    read *, time
    call ImportField(world)
    !call ShiftField(world,10.0d0,3)
    call Simulator( world,OptionalStep=TotalStep,OptionalTime=time)
    call ExportField(world)
    call EndMPI(MPIData)
    
end program main
```