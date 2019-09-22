## Class Name :: MPIClass

[README](README.md)>>[MPIClass](Document/MPIClass.md)

### Instruction:
MPIClass contains all entities which is necessary to run MPI. Following methods are also implemented.


* StartMPI
```
subroutine StartMPI(obj)
    type(MPI_),intent(inout)::obj

```

* EndMPI
```
subroutine EndMPI(obj)
    type(MPI_),intent(inout)::obj

```

### Attribute/DataType
```

    type:: MPI_
        integer :: ierr
        integer :: MyRank
        integer :: PeTot
        integer :: Comm1
        integer :: Comm2
        integer :: Comm3
        integer :: Comm4
        integer :: Comm5
        real(8) :: stime
        real(8) :: etime
    end type    
```

### Requirements
- mpi