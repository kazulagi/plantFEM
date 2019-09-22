## Class Name :: FEMDomainClass

[README](README.md)>>[FEMDomainClass](Document/FEMDomainClass.md)

### Instruction:
FEMDomainClass focuses on defining and operating discretized domains for universal problems in terms of Finite Element Method. It has full-set information for a problem. Following methods are defined.


* DeallocateFEMDomain
```
subroutine DeallocateFEMDomain(obj)
    class(FEMDomain_),intent(inout)::obj
```


* InitializeFEMDomain    
```
subroutine InitializeFEMDomain(obj)
    class(FEMDomain_),intent(inout)::obj

```


* ImportFEMDomain    
```
subroutine ImportFEMDomain(obj,OptionalFileFormat,OptionalProjectName,FileHandle)
    class(FEMDomain_),intent(inout)::obj
    character*4,optional,intent(in)::OptionalFileFormat
    character*70,optional,intent(in)::OptionalProjectName
```


* MergeFEMDomain    
```
subroutine MergeFEMDomain(inobj1,inobj2,outobj)
    class(FEMDomain_),intent(in) ::inobj1,inobj2
    class(FEMDomain_),intent(out)::outobj
    
```


* ExportFEMDomain     
```
subroutine ExportFEMDomain(obj,OptionalFileFormat,OptionalProjectName,FileHandle)
    class(FEMDomain_),intent(inout)::obj
    character*4,optional,intent(in)::OptionalFileFormat
    character*70,optional,intent(in)::OptionalProjectName
    
```


* AddDBoundCondition    
```
subroutine AddDBoundCondition(obj,xmin,xmax,ymin,ymax,zmin,zmax,&
    tmin,tmax,valx,valy,valz)
    class(FEMDomain_),intent(inout)::obj
    real(8),optional,intent(in)::xmin,xmax
    real(8),optional,intent(in)::ymin,ymax
    real(8),optional,intent(in)::zmin,zmax
    real(8),optional,intent(in)::tmin,tmax
```

### Attribute/DataType
```

    type :: FEMDomain_
        type(ShapeFunction_)    :: ShapeFunction
        type(Mesh_)             :: Mesh
        type(MaterialProp_)     :: MaterialProp
        type(Boundary_)         :: Boundary
        type(ControlParameter_) :: ControlPara
        character*200 :: FilePath
        character*200 :: FileName
        character*9 :: Dtype
        character*20 :: SolverType
    end type FEMDomain_
```

### Requirements

- ArrayOperationClass
- ShapeFunctionClass
- MeshOperationClass
- MaterialPropClass
- BoundaryConditionClass
- ControlParaeterClass