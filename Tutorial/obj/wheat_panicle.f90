program main
use PanicleClass
implicit none

type(FEMDomain_) :: wheat_panicle


wheat_panicle = to_wheat_panicle_mesh(&
        num_seed_column         = 40,&
        panicle_seed_interval   = 1.0d0/1000.0d0,&
        panicle_seed_diameter   = 5.0d0/1000.0d0,&
        panicle_seed_length     = 10.0d0/1000.0d0,&
        panicle_panicle_diameter     = 3.0d0/1000.0d0, &
        culm_length = 0.80d0,&
        culm_diameter = 7.0d0/1000.0d0,&
        culm_division = 100 &
    )

call wheat_panicle%vtk("test")

end program