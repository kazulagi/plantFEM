use StemClass
implicit none

type(Stem_) :: stem

! create bamboo-like hollow stems


stem = to_hollow_stem(&
    division=[5,5,300],&
    int_radius=0.100d0,&
    out_radius=0.20d0,&
    internode_len=0.50d0,&
    internode_thickness=0.20d0,&
    total_len=5.0d0)

call stem%vtk("hollow_5")


stem = to_hollow_stem(&
    division=[5,5,300],&
    int_radius=0.00d0,&
    out_radius=0.20d0,&
    internode_len=1.0d0,&
    internode_thickness=0.20d0,&
    total_len=5.0d0)

call stem%vtk("hollow_4")

stem = to_hollow_stem(&
    division=[5,5,300],&
    int_radius=0.100d0,&
    out_radius=0.20d0,&
    internode_len=1.0d0,&
    internode_thickness=0.20d0,&
    total_len=5.0d0)

call stem%vtk("hollow_3")


stem = to_hollow_stem(&
    division=[5,5,300],&
    int_radius=0.150d0,&
    out_radius=0.20d0,&
    internode_len=1.0d0,&
    internode_thickness=0.10d0,&
    total_len=5.0d0)

call stem%vtk("hollow_2")

stem = to_hollow_stem(&
    division=[5,5,300],&
    int_radius=0.150d0,&
    out_radius=0.20d0,&
    internode_len=1.0d0,&
    internode_thickness=0.050d0,&
    total_len=5.0d0)

call stem%vtk("hollow_1")

end