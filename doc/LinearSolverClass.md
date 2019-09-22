## Class Name :: LinearSolverClass

[README](README.md)>>[LinearSolverClass](Document/LinearSolverClass.md)

### Instruction:
LinearSolverClass contains Linear solvers such as Gauss-Seidel, BiCGSTAB. Following methods are available.

* gauss_seidel
```
subroutine gauss_seidel(a, b, x, n, itrmax, er0)
  integer, intent(in) :: n, itrmax
  real(8), intent(in)  :: a(n, n), b(n), er0
  real(8), intent(out) :: x(n)

```

* gauss_jordan_pv
```
subroutine gauss_jordan_pv(a0, x, b, n)
  integer, intent(in) :: n
  real(8), intent(in) :: a0(n,n), b(n)
  real(8), intent(out) :: x(n)

```

* bicgstab1d
```
subroutine bicgstab1d(a, b, x, n, itrmax, er)
  integer, intent(in) :: n, itrmax
  real(8), intent(in) :: a(n,n), b(n), er
  real(8), intent(inout) :: x(n)

```

* bicgstab_nr
```
subroutine bicgstab_nr(a, b, x, n, itrmax, er,u_nod_x, u_nod_y)
  integer, intent(in) :: n, itrmax,u_nod_x(:),u_nod_y(:)
  real(8), intent(in) :: a(n,n),b(n), er
  real(8), intent(inout) :: x(n)

```

* bicgstab_nr1
```
subroutine bicgstab_nr1(a, b, x, n, itrmax, er,u_nod_x, u_nod_y,u_nod_dis_x,u_nod_dis_y)
  integer, intent(in) :: n, itrmax,u_nod_x(:),u_nod_y(:)
  real(8), intent(in) :: a(n,n),b(n), er,u_nod_dis_x(:),u_nod_dis_y(:)
  real(8), intent(inout) :: x(n)

```

* bicgstab_dirichlet
```
subroutine bicgstab_dirichlet(a, b, x, n, itrmax, er,DBoundNodID, DBoundVal,SetBC)
  integer, intent(in) :: n, itrmax,DBoundNodID(:,:),SetBC
  real(8), intent(in) :: a(n,n),b(n), er,DBoundVal(:,:)
  real(8), intent(inout) :: x(n)


```

* modify_residual_1
```
subroutine modify_residual_1(r,x, u_nod_x, u_nod_y,u_nod_dis_x,u_nod_dis_y)
	integer,intent(in)::u_nod_x(:),u_nod_y(:)
	real(8), intent(in) :: u_nod_dis_x(:),u_nod_dis_y(:)
	real(8),intent(inout)::r(:),x(:)

```

* modify_residual
```
subroutine modify_residual(r, u_nod_x, u_nod_y)
	integer,intent(in)::u_nod_x(:),u_nod_y(:)
	real(8),intent(inout)::r(:)

```


* modify_residual_dirichlet
```
subroutine modify_residual_dirichlet(r,x, DBoundNodID, DBoundVal,SetBC)
  integer,intent(in)::DBoundNodID(:,:),SetBC
  real(8),intent(in)::DBoundVal(:,:)
	real(8),intent(inout)::r(:),x(:)

```
