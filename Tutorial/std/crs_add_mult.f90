use SparseClass
implicit none

type(COO_) :: coo(2)
type(CRS_) :: crs(3)

!sample_wave = wave%create(Hz = 100.0d0,wavetype="white")
!call f%plot(x=sample_wave(:,1),fx = sample_wave(:,2) )

call coo(1)%init(num_row=4)
call coo(2)%init(num_row=4)

call coo(1)%add(row=1,col=1,val=1.0d0)
call coo(1)%add(row=2,col=2,val=1.0d0)
call coo(1)%add(row=3,col=3,val=1.0d0)
call coo(1)%add(row=4,col=4,val=10.0d0)
call coo(1)%add(row=1,col=2,val=-1.0d0)
call coo(1)%add(row=2,col=1,val=-1.0d0)
call coo(1)%add(row=2,col=3,val=-1.0d0)
call coo(1)%add(row=3,col=2,val=-1.0d0)

call coo(2)%add(row=1,col=1,val=2.0d0)
call coo(2)%add(row=2,col=2,val=2.0d0)
call coo(2)%add(row=3,col=3,val=2.0d0)
call coo(2)%add(row=4,col=4,val=2.0d0)
call coo(2)%add(row=1,col=2,val=-2.0d0)
call coo(2)%add(row=2,col=1,val=-2.0d0)
call coo(2)%add(row=2,col=3,val=-2.0d0)
call coo(2)%add(row=3,col=2,val=-2.0d0)

crs(1) = coo(1)%to_CRS()

print *, crs(1)%col_idx
print *, crs(1)%row_ptr
print *, crs(1)%val

crs(2) = coo(2)%to_CRS()
crs(3) = crs(1) - 10.0d0*crs(2)

print *, "col"
call print(crs(3)%to_dense())

end