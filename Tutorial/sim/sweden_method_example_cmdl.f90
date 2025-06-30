program sweden_method_example_cmdl
    use SoilMechanicsClass
    use ListClass
    implicit none

    type(SoilMechanics_) :: SoilMech
    type(IO_) :: f
    character(:),allocatable :: name
    
    name = argv() .get. 1
    ! slope stability analysis by sweden method
    call f%open(name+"_min_Fs_value.txt","w")
    call f%write(&
            SoilMech%sweden_method(&
                name=name,&
                slope_angle  = freal(argv() .get. 2),& ! (double) 30 deg.
                slope_height = freal(argv() .get. 3),& ! (double) 5 m
                density      = freal(argv() .get. 4),& ! (double) 1.8 t/m^3
                num_division =  fint(argv() .get. 5),& ! (int   ) 20 slices 
                c            = freal(argv() .get. 6),& ! (double) 40 kPa
                phi          = freal(argv() .get. 7) & ! (double) 10 deg.
            ) &
        )
    call f%close()
    
    ! example :: 
    ! ./sweden_method_example_cmdl.out test 30.0 5.0 1.8 20 40.0 10.0

end program sweden_method_example_cmdl