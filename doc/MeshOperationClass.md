## Class Name :: MeshOperationClass

[README](README.md)>>[MeshOperationClass](Document/MeshOperationClass.md)

### Instruction:
MeshOperationClass keeps and operates mesh object of Finite Element Method. Following methods are defined.



* DeallocateMesh
```
subroutine DeallocateMesh(obj)
    class(Mesh_),intent(inout)::obj

```


* InitializeMesh
```
subroutine InitializeMesh(obj)
    class(Mesh_),intent(inout)::obj

```


* ImportElemNod
```
subroutine ImportElemNod(obj,elem_nod)
    class(Mesh_),intent(inout)::obj
    integer,intent(in)::elem_nod(:,:)

```


* ImportNodCoord
```
subroutine ImportNodCoord(obj,nod_coord)
    class(Mesh_),intent(inout)::obj
    real(8),intent(in)::nod_coord(:,:)

```


* ImportElemMat
```
subroutine ImportElemMat(obj,elem_mat)
    class(Mesh_),intent(inout)::obj
    integer,intent(in)::elem_mat(:)

```


* GetFacetElement
```
subroutine GetFacetElement(obj)
    class(Mesh_),intent(inout)::obj


```


* GetSurface2D
```
subroutine GetSurface2D(obj)
    class(Mesh_),intent(inout)::obj

```


* GetSurface
```
subroutine GetSurface(obj)
    class(Mesh_),intent(inout)::obj

```


* GetInterface
```
subroutine GetInterface(obj1,obj2,iface1,iface2)
    class(Mesh_),intent(inout)::obj1,obj2
    class(Mesh_),intent(inout)::iface1,iface2

```


* GetInterfaceElemNod
```
subroutine GetInterfaceElemNod(obj,iface)
    type(Mesh_),intent(in)::obj
    type(Mesh_),intent(inout)::iface

```



* GetBoundingBox
```
subroutine GetBoundingBox(obj,BBox)
    class(Mesh_),intent(in)::obj
    class(Mesh_),intent(inout)::BBox

```


* GetFacetElemInsideBox
```
subroutine GetFacetElemInsideBox(obj,BBox,iface)
    type(Mesh_),intent(in)::obj,BBox
    type(Mesh_),intent(inout)::iface

```


* GetInterSectBox
```
subroutine GetInterSectBox(obj1,obj2,BBox)
    type(Mesh_),intent(in)::obj1,obj2
    type(Mesh_),intent(inout)::BBox

```



* GetNextFacets
```
subroutine GetNextFacets(obj)
    class(Mesh_),intent(inout)::obj
```


* MergeMesh
```
subroutine MergeMesh(inobj1,inobj2,outobj)
    class(Mesh_),intent(in) ::inobj1,inobj2
    class(Mesh_),intent(out)::outobj

```



* ExportElemNod
```
subroutine ExportElemNod(obj,elem_nod)
    class(Mesh_),intent(inout)::obj
    integer,allocatable,intent(inout)::elem_nod(:,:)

```



* ExportNodCoord
```
subroutine ExportNodCoord(obj,nod_coord)
    class(Mesh_),intent(inout)::obj
    real(8),allocatable,intent(inout)::nod_coord(:,:)

```




* ExportSurface2D
```
subroutine ExportSurface2D(obj,surface_nod)
    class(Mesh_),intent(inout)::obj
    integer,allocatable,intent(inout)::surface_nod(:)

```




* DisplayMesh
```
subroutine DisplayMesh(obj,OptionalFolderName,OptionalFormat)
    class(Mesh_),intent(inout)::obj
    character*70,optional,intent(in):: OptionalFolderName
    character*4,optional,intent(in) :: OptionalFormat

```


### Attribute/Datatype
```

    type:: Mesh_
        real(8),allocatable::NodCoord(:,:)
        real(8),allocatable::NodCoordInit(:,:)
        integer,allocatable::ElemNod(:,:)
        integer,allocatable::FacetElemNod(:,:)
        integer,allocatable::NextFacets(:,:)
        integer,allocatable::SurfaceLine2D(:)
        integer,allocatable::ElemMat(:)
        integer,allocatable::SubMeshNodFromTo(:,:)
        integer,allocatable::SubMeshElemFromTo(:,:)
        integer,allocatable::SubMeshSurfFromTo(:,:)
        integer,allocatable::GlobalNodID(:)
        character*70::ElemType
        character*70 ErrorMsg
    end type Mesh_
```

### Requirements
- MathClass