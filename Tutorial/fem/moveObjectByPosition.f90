use FEMDomainClass
implicit none


type(FEMDomain_) :: cube

call cube%create("Cube3D")
call cube%vtk("step_1")
print *, "step 1"
print *, cube%xrange()
print *, cube%yrange()
print *, cube%zrange()

! move position by keyword
call cube%move(to="center")
call cube%vtk("step_2")
print *, "step 2"
print *, cube%xrange()
print *, cube%yrange()
print *, cube%zrange()

! move position by keyword
call cube%move(to="origin")
call cube%vtk("step_2")
print *, "step 3"
print *, cube%xrange()
print *, cube%yrange()
print *, cube%zrange()



end