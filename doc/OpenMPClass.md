## Class Name :: OpenMPClass

[README](README.md)>>[OpenMPClass](Document/OpenMPClass.md)

### Instruction:
OpenMPClass is a toolbox for floating-point operation by using OpenMP. Followings are implemented.


* omp_dot_product1
```
subroutine omp_dot_product1(a,b,sum)
    real(8),intent(in)::a(:),b(:)
    real(8),intent(out) ::sum
```


* omp_dot_product
```
subroutine omp_dot_product(a,b,sum)
    real(8),intent(in)::a(:),b(:)
    real(8),intent(out) ::sum

```


* omp_dot_product2
```
subroutine omp_dot_product2(a,b,sum)
    real(8),intent(in)::a(:),b(:)
    real(8),intent(out) ::sum

```


* omp_matmul
```
subroutine omp_matmul(a,b,mm)
    real(8),intent(in)::a(:,:),b(:)
    real(8),intent(inout)::mm(:)

```

### Requirements
- omp_lib