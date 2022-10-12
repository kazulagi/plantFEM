use COOClass

type(COO_) :: coo
type(CRS_) :: CRS

call COO%init(1000000)
do i=0,1000000-5
    call COO%add(row=1+i,col=1+i,val=100.0d0)
    call COO%add(row=2+i,col=2+i,val=100.0d0)
    call COO%add(row=3+i,col=3+i,val=100.0d0)
    call COO%add(row=4+i,col=4+i,val=100.0d0)
    call COO%add(row=1+i,col=1+i,val=200.0d0)
    call COO%add(row=2+i,col=2+i,val=200.0d0)
    call COO%add(row=3+i,col=3+i,val=200.0d0)
    call COO%add(row=4+i,col=4+i,val=200.0d0)
    call COO%add(row=1+i,col=2+i,val=200.0d0)
    call COO%add(row=2+i,col=3+i,val=200.0d0)
    call COO%add(row=3+i,col=4+i,val=200.0d0)
    call COO%add(row=4+i,col=4+i,val=200.0d0)
    call COO%add(row=2+i,col=1+i,val=200.0d0)
    call COO%add(row=3+i,col=2+i,val=200.0d0)
    call COO%add(row=4+i,col=3+i,val=200.0d0)
enddo

print *, COO%getAllCol()

CRS = COO%to_CRS()
print *, CRS%row_ptr

call COO%remove()


end