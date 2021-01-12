## Class Name :: ArrayOperationClass
[README](README.md)>>[ArrayOperationClass](Document/ArrayOperationClass.md)   
   
### Instruction:
ArrayOperationClass focuses on operating Array objects. Following methods are defined.
* MergeArray:    

```
subroutine MergeArray(a,b,c)
    intent(in)::a(:,:)
    intent(in)::b(:,:)
    allocatable,intent(out)::c(:,:)
```


* CopyArray

```
subroutine CopyArray(a,ac)
    intent(in)::a(:,:)
    allocatable,intent(inout)::ac(:,:)
```

* TrimArray


```

subroutine TrimArray(a,k)
    allocatable,intent(inout)::a(:,:)
    intent(in)::k
```


* ImportArray

```
subroutine ImportArray(Mat,OptionalFileHandle,OptionalSizeX,OptionalSizeY)
    allocatable,intent(inout)::Mat(:,:)
    optional,intent(in)::OptionalFileHandle,OptionalSizeX,OptionalSizeY

```
* ExportArray


```
subroutine ExportArraySize(Mat,RankNum,OptionalFileHandle)
    intent(in)::Mat(:,:)
    optional,intent(in)::OptionalFileHandle
    intent(in)::RankNum

```
* ExportArraySize


```
ExportArray(Mat,OptionalFileHandle)
    intent(in)::Mat(:,:)
    optional,intent(in)::OptionalFileHandle
```


