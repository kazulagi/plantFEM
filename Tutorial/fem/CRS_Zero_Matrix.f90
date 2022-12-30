use FEMDomainClass
implicit none

type(FEMDomain_) :: cube
type(CRS_) :: ZeroMatrix,ZeroMatrix_r
type(Time_) :: time

integer(int32) :: ElementID

call cube%create("Cube3D",x_num=100,y_num=100,z_num=200)
print *, "mesh is created."

! CRS-formatted 3-D ZeroMatrix
call time%start()
ZeroMatrix = cube%ZeroMatrix(DOF=3)
call time%show()

call time%start()
ZeroMatrix_r = cube%ZeroMatrix(DOF=3,Regacy=.true. )
call time%show()

! test :: both should be the same
print *, size(ZeroMatrix_r%row_ptr),size(ZeroMatrix%row_ptr)
print *, size(ZeroMatrix_r%col_idx),size(ZeroMatrix%col_idx)
print *, size(ZeroMatrix_r%val),size(ZeroMatrix%val)

call print(maxval(abs((ZeroMatrix_r%row_ptr-ZeroMatrix%row_ptr))))
call print(maxval(abs((ZeroMatrix_r%col_idx-ZeroMatrix%col_idx))))
call print(maxval(abs((ZeroMatrix_r%val-ZeroMatrix%val))))


end