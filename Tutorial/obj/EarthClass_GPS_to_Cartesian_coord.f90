program main
    use EarthClass
    implicit none

    ! GPS(lat, lon) to Cartesian coordinate
    print *, to_Cartesian(&
        latitude = 36.103774791666666d0,&
        longitude=140.08785504166664d0,&
        origin=JP_Cartesian_Origin(ID=9) )
    


end program main