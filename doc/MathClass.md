## Class Name :: MathClass

[README](README.md)>>[MathClass](Document/MathClass.md)

### Instruction:
MathClass is a class for mathematical operations. Following methods are defined.


* SearchNearestCoord
```
function SearchNearestCoord(Array,x)  result(id)
	real(8),intent(in) :: Array(:,:)
	real(8),intent(in) :: x(:)

```


* SearchIDIntVec
```
function SearchIDIntVec(Vec,val) result(id_)
	integer,intent(in) :: Vec(:)
	integer,intent(in) :: val

```

* heapsort
```
subroutine heapsort(n,array)
  integer,intent(in) :: n
  integer,intent(inout) :: array(1:n)

```

* cross_product
```
function cross_product(a,b) result (c)
	real(8), intent(in) :: a(:),b(:)
	real(8), allocatable :: c(:)

```

* diadic
```
function diadic(a,b) result(c)
  real(8), intent(in) :: a(:), b(:)
	real(8), allocatable :: c(:,:)

```

* calcgz
```
subroutine calcgz(x2,x11,x12,nod_coord,gzi)
	real(8), intent(in) :: nod_coord(:,:)
	real(8),intent(out) :: gzi
	integer,intent(in):: x2,x11,x12

```

* eigen_2d
```
subroutine eigen_2d(Amat,eigenvector)
	real(8),intent(in)::Amat(:,:)
	real(8),intent(inout)::eigenvector(:,:)

```

* signmm
```
function signmm(a) result(b)
	real(8),intent(in)::a
	real(8) b

```

* det_mat
```
recursive function det_mat(a,n) result(det)
  integer, intent(in) :: n
  real(8), intent(in) :: a(n, n)
  real(8) det, b(n-1, n-1)

```


* trans_rank_2
```
subroutine trans_rank_2(A,A_T)
	real(8),intent(in)::A(:,:)
	real(8),allocatable,intent(out)::A_T(:,:)

```

* trans1
```
function trans1(A) result(A_T)
	real(8),intent(in)::A(:)
	real(8),allocatable::A_T(:,:)

```

* trans2
```
function trans2(A) result(A_T)
	real(8),intent(in)::A(:,:)
	real(8),allocatable::A_T(:,:)

```

* inverse_rank_2
```
subroutine inverse_rank_2(A,A_inv)
	real(8),intent(in)::A(:,:)
	real(8),allocatable::A_inv(:,:)

```


* tensor_exponential
```
subroutine tensor_exponential(A,expA,TOL,itr_tol)
  real(8),intent(in)::A(:,:),TOL
  real(8),allocatable,intent(inout)::expA(:,:)
  integer, intent(in)::itr_tol

```


* tensor_expo_der
```
subroutine tensor_expo_der(A,expA_A,TOL,itr_tol)
  real(8),intent(in)::A(:,:),TOL
  real(8),allocatable,intent(inout)::expA_A(:,:,:,:)
  integer, intent(in)::itr_tol

```


* GetNormRe
```
function GetNormRe(a) result(b)
	real(8),intent(in)::a(:)
	real(8) :: b

```


* GetNormMatRe
```
function GetNormMatRe(a) result(b)
	real(8),intent(in)::a(:,:)
	real(8) :: b

```

* trace
```
function trace(a) result(b)
	real(8),intent(in)::a(:,:)
	real(8) :: b

```