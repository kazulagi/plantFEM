use PanicleClass

type(Panicle_) :: panicle

call panicle%init(Length=0.30d0,Width=0.0010d0,x_num=2,y_num=2,z_num=300,&
    rice=.true.,&
    rice_seed_interval=0.0020d0,&
    rice_seed_branch_length=0.0010d0,&
    rice_seed_length=0.0060d0,&
    rice_seed_width=0.0040d0,&
    rice_seed_thickness=0.0020d0,&
    rice_panicle_curvature=0.60d0,&
    rice_seed_division=[3,3,3])

call panicle%vtk("rice")

end