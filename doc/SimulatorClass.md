## Class Name :: SimulatorClass

[README](README.md)>>[SimulatorClass](Document/SimulatorClass.md)

### Instruction:
SimulatorClass is a set of solvers for a field. Solvers can be deployed and mounted onto field by using Following methods.

* Simulator
```
subroutine Simulator(world,OptionalStep,OptionalTime)
    type(Field_),target,intent(inout)   :: world
    type(Simulator_),target             :: sim
    integer,optional,intent(in)  :: OptionalStep
    real(8),optional,intent(in)  :: OptionalTime

```


* RunSimulation
```
subroutine RunSimulation(sim,field,step)
    type(Field_),target,intent(inout)  :: field
    type(Simulator_),intent(inout) :: sim
    integer,intent(in)             :: step

```

* InitializeSimulator
```
subroutine InitializeSimulator(sim,field)
    type(Field_    ),intent(inout) :: field
    type(Simulator_),intent(inout) :: sim

```

* DeploySimulator
```
subroutine DeploySimulator(sim,field)
    type(Field_),target,intent(inout)  :: field
    type(Simulator_),intent(inout) :: sim

```

* DisplaySimulation
```
subroutine DisplaySimulation(sim,field,step)

    type(Field_),target,intent(inout)  :: field
    type(Simulator_),intent(inout) :: sim
    integer,intent(in)             :: step
    
```

* SetSimulatorTime
```
subroutine SetSimulatorTime(sim,field,time)

    type(Field_),target,intent(inout)  :: field
    type(Simulator_),intent(inout) :: sim
    real(8),intent(in)  :: time

```

### Attribute/DataType
```
type :: Simulator_
    type(DiffusionEq_) ,allocatable :: DiffusionEq_Array(:)
    type(FiniteDeform_),allocatable :: FiniteDeform_Array(:)
    type(MultiPhysics_)  ,allocatable :: MultiPhysics_Array(:)
end type
```

### Requirements
- mpi
- MPIClass
- TermClass
- FEMDomainClass
- DiffusionEquationClass
- FiniteDeformationClass
- MultiPhysicsClass
- PostProcessingClass
- FieldClass