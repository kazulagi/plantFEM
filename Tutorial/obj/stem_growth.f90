use StemClass
implicit none

type(stem_) :: stem

call stem%init(z_num=20)
call stem%resize(x=3.0d0/1000.0d0,y=3.0d0/1000.0d0,z=30.0d0/1000.0d0)
call stem%vtk("stem")

! enlong stem
call stem%set_material(YoungModulus=100.0d0*1000,PoissonRatio=0.30d0,side_stiffness_ratio=10.0d0)
call stem%grow(pressure=-30.0d0*1000)
call stem%vtk("stem_1")

end 