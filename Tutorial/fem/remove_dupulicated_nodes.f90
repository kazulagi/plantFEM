use FEMDomainClass
implicit none

type(FEMDomain_) :: cube
type(Time_) :: time

call cube%create("Cube3D",x_num=3,y_num=4,z_num=5)
call cube%resize(x=3.0d0,y=4.0d0,z=5.0d0)
call cube%vtk("cube_0")

print *, cube%nn(),cube%ne()
do i_i=1,20
    cube%mesh%elemnod = cube%mesh%elemnod .v. (cube%mesh%elemnod + cube%nn())
    cube%mesh%nodcoord = cube%mesh%nodcoord .v. cube%mesh%nodcoord 
enddo
print *, cube%nn(), cube%ne()

call time%start()
call cube%remove_duplication(epsilon=0.010d0)
call time%show()

print *, cube%nn(),cube%ne()
call cube%vtk("cube_1")
call system("diff cube_0.vtk cube_1.vtk")

end