program sweden_method_example
    use SoilMechanicsClass
    implicit none

    type(SoilMechanics_) :: SoilMech

    ! slope stability analysis by sweden method
    print *, SoilMech%sweden_method(&
            name="test",&
            slope_angle=30.0d0,& ! (double) 30 deg.
            slope_height=5.0d0,& ! (double) 5 m
            density=1.80d0,&     ! (double) 1.8 t/m^3
            num_division=20,&    ! (int   ) 20 slices 
            c=40.0d0,&           ! (double) 40 kPa
            phi=10.0d0 &         ! (double) 10 deg.
        )

end program sweden_method_example